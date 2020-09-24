package com.waz.zclient.camera.controllers

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, Closeable}
import java.util.concurrent.ArrayBlockingQueue

import android.content.Context
import android.graphics._
import android.hardware.camera2._
import android.media.ImageReader.OnImageAvailableListener
import android.media.{ExifInterface, Image, ImageReader}
import android.os.{Handler, HandlerThread}
import android.view.Surface
import com.waz.log.BasicLogging.LogTag.DerivedLogTag
import com.waz.threading.Threading
import com.waz.utils.returning
import com.waz.zclient.camera.FlashMode

import scala.collection.JavaConverters._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

case class CameraData(cameraId: String, pixelFormat: Int, orientation: Orientation)

case class CombinedCaptureResult(image: Image,
                                 metadata: CaptureResult,
                                 orientation: Int,
                                 format: Int
                                ) extends Closeable {
  override def close(): Unit = image.close()
}

class AndroidCamera2(cameraData: CameraData, cxt: Context, flashMode: FlashMode, texture: SurfaceTexture)
  extends DerivedLogTag with WireCamera {

  private val cameraThread = returning(new HandlerThread("CameraThread")) {
    _.start()
  }
  private val imageReaderThread = returning(new HandlerThread("ImageReaderThread")) {
    _.start()
  }
  private val cameraHandler = new Handler(cameraThread.getLooper)
  private val imageReaderHandler = new Handler(imageReaderThread.getLooper)
  private var imageReader: ImageReader = _
  private val cameraManager = cxt.getApplicationContext.getSystemService(Context.CAMERA_SERVICE).asInstanceOf[CameraManager]
  private val cameraCharacteristics = cameraManager.getCameraCharacteristics(cameraData.cameraId)
  private var cameraRequest: CaptureRequest.Builder = _
  private var cameraSession: CameraCaptureSession = _
  private var previewSize: Option[PreviewSize] = None
  private val camera = openCamera(cameraManager, cameraData.cameraId, cameraHandler)

  private val ImageBufferSize = 3
  private val ImageCaptureTimeoutMillis = 5000

  private def openCamera(cameraManager: CameraManager, id: String, cameraHandler: Handler): Future[Option[CameraDevice]] = {
    val promise = Promise[Option[CameraDevice]]()
    try {
      cameraManager.openCamera(id, new CameraDevice.StateCallback {
        override def onOpened(camera: CameraDevice): Unit = {
          createCameraSession(camera)
          promise.success(Option(camera))
        }

        override def onDisconnected(camera: CameraDevice): Unit = promise.success(None)

        override def onError(camera: CameraDevice, error: Int): Unit = {
          val msg = error match {
            case CameraDevice.StateCallback.ERROR_CAMERA_DEVICE => "Fatal (device)"
            case CameraDevice.StateCallback.ERROR_CAMERA_DISABLED => "Device policy"
            case CameraDevice.StateCallback.ERROR_CAMERA_IN_USE => "Camera in use"
            case CameraDevice.StateCallback.ERROR_CAMERA_SERVICE => "Fatal (service)"
            case CameraDevice.StateCallback.ERROR_MAX_CAMERAS_IN_USE => "Maximum cameras in use"
            case _ => "Unknown"
          }
          val exc = new RuntimeException("Camera  error:" + "(" + error + ") " + msg)
          promise.failure(exc)
        }
      }, cameraHandler)
    } catch {
      case ex: CameraAccessException => promise.failure(ex)
    }
    promise.future
  }

  def createCameraSession(camera: CameraDevice) = {
    cameraRequest = camera.createCaptureRequest(CameraDevice.TEMPLATE_PREVIEW)
    val size = cameraCharacteristics.get(CameraCharacteristics.SCALER_STREAM_CONFIGURATION_MAP)
      .getOutputSizes(cameraData.pixelFormat)
      .maxBy { size => size.getHeight * size.getWidth }
    imageReader = ImageReader.newInstance(
      size.getWidth, size.getHeight, cameraData.pixelFormat, ImageBufferSize)

    val surface = new Surface(texture)
    cameraRequest.addTarget(surface)
    camera.createCaptureSession(List(surface).asJava, new CameraCaptureSession.StateCallback {
      override def onConfigured(session: CameraCaptureSession): Unit = {
        cameraSession = session
        cameraRequest.set(CaptureRequest.CONTROL_MODE, CameraMetadata.CONTROL_MODE_AUTO)
        cameraRequest.set(CaptureRequest.CONTROL_AF_MODE, CameraMetadata.CONTROL_AF_MODE_AUTO)
        cameraRequest.set(CaptureRequest.FLASH_MODE, getSupportedFlashMode(flashMode))
        cameraSession.setRepeatingRequest(cameraRequest.build, null, cameraHandler)
      }

      override def onConfigureFailed(session: CameraCaptureSession): Unit = super.onConfigureFailed(session)

    }, cameraHandler)
  }

  private def getSupportedFlashModesFromCamera =
    Option(cameraCharacteristics.get(CameraCharacteristics.CONTROL_AE_AVAILABLE_MODES)).fold(Set.empty[FlashMode])(_.toSet.map(FlashMode.get))

  private def getSupportedFlashMode(fm: FlashMode) = if (getSupportedFlashModes.contains(flashMode)) flashMode.mode else FlashMode.OFF.mode

  override def getPreviewSize: PreviewSize = previewSize.getOrElse(PreviewSize(0, 0))

  override def takePicture(shutter: => Unit): Future[Array[Byte]] = {
    val promise = Promise[Array[Byte]]
    takePhoto().onComplete {
      case Success(value) =>
        value.format match {
          case ImageFormat.JPEG || ImageFormat.DEPTH_JPEG =>
            val buffer = value.image.getPlanes.head.getBuffer
            val data = new Array[Byte](buffer.remaining())
            buffer.get(data)

            val exif = new ExifInterface(new ByteArrayInputStream(data))
            exif.setAttribute(ExifInterface.TAG_ORIENTATION, value.orientation.toString)
            exif.saveAttributes()

            promise.success(data)
          case _ => promise.failure(new RuntimeException("Invalid Image format"))
        }
      case Failure(exception) => promise.failure(exception)
    }(Threading.Ui)
    promise.future
  }

  private def generateOutputByteArray(corrected: Bitmap): Array[Byte] = {
    val output = new ByteArrayOutputStream()
    corrected.compress(Bitmap.CompressFormat.JPEG, 100, output)
    output.toByteArray
  }

  private def takePhoto(): Future[CombinedCaptureResult] = {
    val promise = Promise[CombinedCaptureResult]()
    val imageQueue = new ArrayBlockingQueue[Image](ImageBufferSize)
    imageReader.setOnImageAvailableListener(new OnImageAvailableListener {
      override def onImageAvailable(reader: ImageReader): Unit = {
        val image = reader.acquireNextImage()
        imageQueue.add(image)
      }
    }, imageReaderHandler)

    val captureRequest = returning(cameraSession.getDevice.createCaptureRequest(CameraDevice.TEMPLATE_STILL_CAPTURE)) {
      _.addTarget(imageReader.getSurface)
    }
    cameraSession.capture(captureRequest.build(), new CameraCaptureSession.CaptureCallback {

      private def computeExifOrientation(rotationDegrees: Int, mirrored: Boolean) = (rotationDegrees, mirrored) match {
        case (0, false) => ExifInterface.ORIENTATION_NORMAL
        case (0, true) => ExifInterface.ORIENTATION_FLIP_HORIZONTAL
        case (180, false) => ExifInterface.ORIENTATION_ROTATE_180
        case (180, true) => ExifInterface.ORIENTATION_FLIP_VERTICAL
        case (270, true) => ExifInterface.ORIENTATION_TRANSVERSE
        case (90, false) => ExifInterface.ORIENTATION_ROTATE_90
        case (90, true) => ExifInterface.ORIENTATION_TRANSPOSE
        case (270, true) => ExifInterface.ORIENTATION_ROTATE_270
        case (270, false) => ExifInterface.ORIENTATION_TRANSVERSE
        case (_, _) => ExifInterface.ORIENTATION_UNDEFINED
      }

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

        val mirrored = cameraCharacteristics.get(CameraCharacteristics.LENS_FACING) == CameraMetadata.LENS_FACING_FRONT
        val exifOrientation = computeExifOrientation(cameraData.orientation.orientation, mirrored)
        promise.success(CombinedCaptureResult(image, result, exifOrientation, imageReader.getImageFormat))
      }
    }, cameraHandler)
    promise.future
  }

  override def release(): Unit = {
    cameraRequest = null
    cameraSession.close()
    imageReaderThread.quitSafely()
    cameraThread.quitSafely()
    camera.foreach {
      case Some(x) => x.close()
    }(Threading.Ui)
  }

  override def setFocusArea(touchRect: Rect, w: Int, h: Int): Future[Unit] = Future.successful(Unit) //TODO set the control regions for the focus

  override def setFlashMode(fm: FlashMode): Unit = {
    cameraRequest.set(CaptureRequest.FLASH_MODE, getSupportedFlashMode(fm))
    cameraSession.setRepeatingRequest(cameraRequest.build(), null, cameraHandler)
  }

  override def getSupportedFlashModes: Set[FlashMode] = getSupportedFlashModesFromCamera

  override def setOrientation(o: Orientation): Unit = Unit //Not needed for now
}
