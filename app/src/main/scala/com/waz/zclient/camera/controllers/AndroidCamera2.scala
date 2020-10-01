package com.waz.zclient.camera.controllers

import java.io.{ByteArrayOutputStream, Closeable}
import java.util.Comparator
import java.util.concurrent.ArrayBlockingQueue

import android.content.Context
import android.graphics._
import android.hardware.camera2._
import android.media.ImageReader.OnImageAvailableListener
import android.media.{ExifInterface, Image, ImageReader}
import android.os.{Handler, HandlerThread}
import android.util.Size
import android.view.{Display, Surface, SurfaceHolder, WindowManager}
import com.waz.bitmap.BitmapUtils
import com.waz.log.BasicLogging.LogTag.DerivedLogTag
import com.waz.threading.Threading
import com.waz.utils.returning
import com.waz.zclient.camera.FlashMode

import scala.collection.JavaConverters._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

case class Orientation(orientation: Int)

case class CameraData(cameraId: String, facing: Int)

case class CombinedCaptureResult(image: Image,
                                 metadata: CaptureResult,
                                 orientation: Int,
                                 format: Int
                                ) extends Closeable {
  override def close(): Unit = image.close()
}

class AndroidCamera2(cameraData: CameraData,
                     cxt: Context,
                     width: Int,
                     height: Int,
                     flashMode: FlashMode,
                     texture: SurfaceTexture)
  extends WireCamera with DerivedLogTag {

  import ExifInterface._
  import WireCamera._
  import com.waz.zclient.log.LogUI._

  private lazy val cameraThread = returning(new HandlerThread("CameraThread")) {
    _.start()
  }
  private lazy val imageReaderThread = returning(new HandlerThread("ImageReaderThread")) {
    _.start()
  }

  private val cameraHandler = new Handler(cameraThread.getLooper)
  private val imageReaderHandler = new Handler(imageReaderThread.getLooper)

  private val cameraManager = cxt.getApplicationContext.getSystemService(Context.CAMERA_SERVICE).asInstanceOf[CameraManager]
  private val cameraCharacteristics = cameraManager.getCameraCharacteristics(cameraData.cameraId)

  private lazy val compatibleSizes = cameraCharacteristics.get(CameraCharacteristics.SCALER_STREAM_CONFIGURATION_MAP)
    .getOutputSizes(ImageFormat.JPEG)
    .maxBy { size => size.getHeight * size.getWidth }

  private lazy val imageReader: ImageReader = returning(ImageReader.newInstance(compatibleSizes.getWidth, compatibleSizes.getHeight, ImageFormat.JPEG, ImageBufferSize)) { reader =>
    reader.setOnImageAvailableListener(null, imageReaderHandler)
  }

  private var cameraRequest: Option[CaptureRequest.Builder] = None
  private var cameraSession: Option[CameraCaptureSession] = None
  private var camera: Option[CameraDevice] = None
  private var orientation: Orientation = Orientation(0)

  def initCamera(): Unit = {
    openCamera(cameraManager, cameraData.cameraId, cameraHandler)

    texture.setDefaultBufferSize(getPreviewSize.w.toInt, getPreviewSize.h.toInt)

    val surface = new Surface(texture)
    val targets = List(surface, imageReader.getSurface)

    camera.foreach { device: CameraDevice =>
      cameraRequest = returning(Option(device.createCaptureRequest(CameraDevice.TEMPLATE_PREVIEW))) {
        _.foreach(_.addTarget(surface))
      }
      createCameraSession(targets, device)
    }
  }

  private def openCamera(cameraManager: CameraManager, id: String, cameraHandler: Handler): Unit = {
    try {
      cameraManager.openCamera(id, new CameraDevice.StateCallback {
        override def onOpened(device: CameraDevice): Unit = {
          camera = Option(device)
        }

        override def onDisconnected(device: CameraDevice): Unit = {
          release()
          camera = None
        }

        override def onError(camera: CameraDevice, errorCode: Int): Unit = {
          val msg = errorCode match {
            case CameraDevice.StateCallback.ERROR_CAMERA_DEVICE => "Fatal (device)"
            case CameraDevice.StateCallback.ERROR_CAMERA_DISABLED => "Device policy"
            case CameraDevice.StateCallback.ERROR_CAMERA_IN_USE => "Camera in use"
            case CameraDevice.StateCallback.ERROR_CAMERA_SERVICE => "Fatal (service)"
            case CameraDevice.StateCallback.ERROR_MAX_CAMERAS_IN_USE => "Maximum cameras in use"
            case _ => "Unknown"
          }
          error(l"Camera  error: ($errorCode) $msg")
        }
      }, cameraHandler)
    } catch {
      case ex: CameraAccessException => error(l"Camera access error when opening camera: ", ex)
    }
  }

  private def createCameraSession(targets: List[Surface], camera: CameraDevice): Unit = {
    try {
      camera.createCaptureSession(targets.asJava, new CameraCaptureSession.StateCallback {
        override def onConfigured(session: CameraCaptureSession): Unit = {
          cameraSession = Option(session)
          cameraRequest.foreach { request =>
            request.set(CaptureRequest.CONTROL_MODE.asInstanceOf[CaptureRequest.Key[Any]], CameraMetadata.CONTROL_MODE_AUTO)
            request.set(CaptureRequest.CONTROL_AF_MODE.asInstanceOf[CaptureRequest.Key[Any]], CameraMetadata.CONTROL_AF_MODE_CONTINUOUS_PICTURE)
            request.set(CaptureRequest.FLASH_MODE.asInstanceOf[CaptureRequest.Key[Any]], getSupportedFlashMode(flashMode))
            session.setRepeatingRequest(request.build(), null, cameraHandler)
          }
        }

        override def onConfigureFailed(session: CameraCaptureSession): Unit = {
          //Release open session
          session.abortCaptures()
          session.close()
          cameraSession = None
        }

      }, cameraHandler)
    } catch {
      case ex: CameraAccessException => error(l"Camera access error when creating camera session: ", ex)
    }
  }

  private def getSupportedFlashModesFromCamera =
    Option(cameraCharacteristics.get(CameraCharacteristics.CONTROL_AE_AVAILABLE_MODES)).fold(Set.empty[FlashMode])(_.toSet.map(FlashMode.get))

  private def getSupportedFlashMode(fm: FlashMode) = if (getSupportedFlashModes.contains(flashMode)) flashMode.mode else FlashMode.OFF.mode

  override def getPreviewSize: PreviewSize = getPreviewOutputSize(cameraCharacteristics, classOf[SurfaceHolder])

  override def takePicture(shutter: => Unit): Future[Array[Byte]] = {
    val promise = Promise[Array[Byte]]
    try {
      takePhoto().onComplete {
        case Success(photoResult) =>
          val buffer = photoResult.image.getPlanes.head.getBuffer
          val data = new Array[Byte](buffer.remaining())
          buffer.get(data)
          // Correct the orientation, if needed.
          val result = photoResult.orientation match {
            case ORIENTATION_NORMAL => data
            case ORIENTATION_UNDEFINED =>
              val corrected = BitmapUtils.fixOrientationForUndefined(BitmapFactory.decodeByteArray(data, 0, data.length), photoResult.orientation)
              generateOutputByteArray(corrected)
            case _ =>
              val corrected = BitmapUtils.fixOrientation(BitmapFactory.decodeByteArray(data, 0, data.length), photoResult.orientation)
              generateOutputByteArray(corrected)
          }
          promise.success(result)
        case Failure(exception) => promise.failure(exception)
      }(Threading.Ui)
    } catch {
      case ex: CameraAccessException =>
        error(l"Camera access error when taking a photo : ", ex)
        promise.failure(ex)
    }
    promise.future
  }

  private def generateOutputByteArray(corrected: Bitmap): Array[Byte] =
    returning(new ByteArrayOutputStream()) {
      corrected.compress(Bitmap.CompressFormat.JPEG, 100, _)
    }.toByteArray

  private def takePhoto(): Future[CombinedCaptureResult] = {
    val promise = Promise[CombinedCaptureResult]()
    val imageQueue = new ArrayBlockingQueue[Image](ImageBufferSize)
    imageReader.setOnImageAvailableListener(new OnImageAvailableListener {
      override def onImageAvailable(reader: ImageReader): Unit = {
        val image = reader.acquireNextImage()
        imageQueue.add(image)
      }
    }, imageReaderHandler)

    cameraSession.foreach { session: CameraCaptureSession =>
      val captureRequest = returning(session.getDevice.createCaptureRequest(CameraDevice.TEMPLATE_STILL_CAPTURE)) {
        _.addTarget(imageReader.getSurface)
      }
      session.capture(captureRequest.build(), new CameraCaptureSession.CaptureCallback {
        private def computeExifOrientation(rotationDegrees: Int, mirrored: Boolean) = (rotationDegrees, mirrored) match {
          case (0, false)    => ORIENTATION_NORMAL
          case (0, true)     => ORIENTATION_FLIP_HORIZONTAL
          case (90, false)   => ORIENTATION_ROTATE_90
          case (90, true)    => ORIENTATION_TRANSPOSE
          case (180, false)  => ORIENTATION_ROTATE_180
          case (180, true)   => ORIENTATION_FLIP_VERTICAL
          case (270, false)  => ORIENTATION_TRANSVERSE
          case (270, true)   => ORIENTATION_ROTATE_270
          case _             => ORIENTATION_UNDEFINED
        }

        private def computeRelativeOrientation(orientation: Int, mirrored: Boolean): Int =
          if (orientation != android.view.OrientationEventListener.ORIENTATION_UNKNOWN) {
            val sensorOrientation = cameraCharacteristics.get(CameraCharacteristics.SENSOR_ORIENTATION)

            //Round the orientation to the nearest 90
            var deviceOrientation = (orientation + 45) / 90 * 90

            //if front-facing flip the image
            if (mirrored) deviceOrientation = -deviceOrientation

            //Find the relative orientation between the camera sensor and the device orientation
            (sensorOrientation + deviceOrientation + 360) % 360
          } else 0

        override def onCaptureCompleted(session: CameraCaptureSession,
                                        request: CaptureRequest,
                                        result: TotalCaptureResult): Unit = {
          super.onCaptureCompleted(session, request, result)
          val timeoutExc = new RuntimeException("Image de-queuing took too long")
          val timeoutRunnable = new Runnable {
            override def run(): Unit = promise.failure(timeoutExc)
          }
          imageReaderHandler.postDelayed(timeoutRunnable, ImageCaptureTimeoutMillis)

          val image = imageQueue.take()

          imageReaderHandler.removeCallbacks(timeoutRunnable)
          imageReader.setOnImageAvailableListener(null, null)

          while (imageQueue.size > 0) {
            imageQueue.take().close()
          }
          val mirrored = getCurrentCameraFacing == CameraMetadata.LENS_FACING_FRONT
          val relativeOrientation = computeRelativeOrientation(orientation.orientation, mirrored)
          val exifOrientation = computeExifOrientation(relativeOrientation, mirrored)
          promise.success(CombinedCaptureResult(image, result, exifOrientation, imageReader.getImageFormat))
        }
      }, cameraHandler)
    }
    promise.future
  }

  override def release(): Unit = {
    cameraRequest = None
    imageReader.discardFreeBuffers()
    imageReader.close()
    cameraSession.foreach { session: CameraCaptureSession =>
      session.stopRepeating()
      session.close()
    }
    camera.foreach(_.close())
  }

  override def setFocusArea(touchRect: Rect, w: Int, h: Int): Future[Unit] = Future.successful(Unit) //TODO set the control regions for the focus

  override def setFlashMode(fm: FlashMode): Unit = {
    cameraSession.foreach { session: CameraCaptureSession =>
      cameraRequest.foreach { request =>
        request.set(CaptureRequest.FLASH_MODE.asInstanceOf[CaptureRequest.Key[Any]], getSupportedFlashMode(fm))
        session.setRepeatingRequest(request.build(), null, cameraHandler)
      }
    }
  }

  override def getSupportedFlashModes: Set[FlashMode] = getSupportedFlashModesFromCamera

  override def setOrientation(o: Orientation): Unit = {
    orientation = o
  }

  override def getCurrentCameraFacing: Int = cameraCharacteristics.get(CameraCharacteristics.LENS_FACING)

  class CompareSizesByArea extends Comparator[Size] {
    def compare(lhs: Size, rhs: Size): Int = { // We cast here to ensure the multiplications won't overflow
      (lhs.getWidth * lhs.getHeight - rhs.getWidth * rhs.getHeight).signum
    }
  }

  def getDisplaySmartSize(display: Display): SmartSize = {
    val outpoint = new Point()
    display.getRealSize(outpoint)
    new SmartSize(outpoint.x, outpoint.y)
  }

  def getPreviewOutputSize[T](characteristics: CameraCharacteristics, targetClass: Class[T]): PreviewSize = {
    val windowManager = cxt.getApplicationContext.getSystemService(Context.WINDOW_SERVICE).asInstanceOf[WindowManager]
    val screenSize = getDisplaySmartSize(windowManager.getDefaultDisplay)
    val hdScreen = screenSize.long >= Size_1080p.long | screenSize.short >= Size_1080p.short
    val maxSize = if (hdScreen) Size_1080p else screenSize

    val config = characteristics.get(CameraCharacteristics.SCALER_STREAM_CONFIGURATION_MAP)
    val allSizes = config.getOutputSizes(targetClass)

    val validSizes = allSizes.sortBy(s => s.getHeight * s.getWidth).map(s => new SmartSize(s.getWidth, s.getHeight)).reverse
    val newSmartSize = validSizes.filter(smartSize => smartSize.long <= maxSize.long && smartSize.short <= maxSize.short).head
    newSmartSize.getSize
  }
}

class SmartSize(width: Int, height: Int) {
  val getSize: PreviewSize = PreviewSize(width, height)
  val long: Float = Math.max(getSize.w, getSize.h)
  val short: Float = Math.min(getSize.w, getSize.h)
}

