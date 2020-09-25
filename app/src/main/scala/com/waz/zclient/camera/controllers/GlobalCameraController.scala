/**
 * Wire
 * Copyright (C) 2019 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.waz.zclient.camera.controllers

import java.util.concurrent.{Executors, ThreadFactory}

import android.content.Context
import android.graphics._
import android.hardware.camera2.{CameraCharacteristics, CameraManager, CameraMetadata}
import android.view.OrientationEventListener
import com.waz.log.BasicLogging.LogTag.DerivedLogTag
import com.waz.threading.Threading
import com.waz.utils.RichFuture
import com.waz.zclient.WireContext
import com.waz.zclient.camera.{CameraFacing, FlashMode}
import com.waz.zclient.core.logging.Logger
import com.waz.zclient.utils.Callback
import com.wire.signals.{CancellableFuture, EventContext, Signal}

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}

class GlobalCameraController(cameraFactory: CameraFactory)(implicit cxt: WireContext, eventContext: EventContext)
  extends DerivedLogTag {

  implicit val cameraExecutionContext: ExecutionContext = new ExecutionContext {
    private val executor = Executors.newSingleThreadExecutor(new ThreadFactory {
      override def newThread(r: Runnable): Thread = new Thread(r, "CAMERA")
    })

    override def reportFailure(cause: Throwable): Unit = Logger.error("GlobalCameraController", "Problem executing on Camera Thread.", cause)

    override def execute(runnable: Runnable): Unit = executor.submit(runnable)
  }

  private val cameraManager = cxt.getApplicationContext.getSystemService(Context.CAMERA_SERVICE).asInstanceOf[CameraManager]

  //values protected for testing
  protected[camera] val availableCameraData = cameraFactory.getAvailableCameraData(cameraManager)
  protected[camera] var currentCamera = Option.empty[WireCamera]
  protected[camera] var loadFuture = CancellableFuture.cancelled[(PreviewSize, Set[FlashMode])]()
  protected[camera] var currentCameraData = availableCameraData.headOption //save this in global controller for consistency during the life of the app

  val currentFlashMode = Signal(FlashMode.OFF)

  val deviceOrientation = Signal(Orientation(0))

  def getCurrentCameraFacing = currentCamera.map(_.getCurrentCameraFacing)

  /**
   * Cycles the currentCameraInfo to point to the next camera in the list of camera devices. This does NOT, however,
   * start the camera. The previous camera should be released and then openCamera() should be called again
   */
  def setNextCamera() = currentCameraData.foreach(c => currentCameraData = availableCameraData.lift((c.cameraId.toInt + 1) % availableCameraData.size))

  /**
   * Returns a Future of a PreviewSize object representing the preview size that the camera preview will draw to.
   * This should be used to calculate the aspect ratio for re-sizing the texture
   */
  def openCamera(texture: SurfaceTexture, w: Int, h: Int) = {
    loadFuture.cancel()
    loadFuture = currentCameraData.fold(CancellableFuture.cancelled[(PreviewSize, Set[FlashMode])]()) { data =>
      CancellableFuture {
        currentCamera = Some(cameraFactory(data, texture, cxt, w, h, deviceOrientation.currentValue.getOrElse(Orientation(0)), currentFlashMode.currentValue.getOrElse(FlashMode.OFF)))
        val previewSize = currentCamera.map(_.getPreviewSize).getOrElse(PreviewSize(0, 0))
        val flashModes = currentCamera.map(_.getSupportedFlashModes).getOrElse(Set.empty)
        (previewSize, flashModes)
      }
    }
    loadFuture
  }

  def takePicture(onShutter: => Unit) = Future {
    currentCamera match {
      case Some(c) => c.takePicture(onShutter)
      case _ => Future.failed(new RuntimeException("Take picture cannot be called while the camera is closed"))
    }
  }.flatten

  def releaseCamera(callback: Callback[Void]): Unit = releaseCamera().andThen {
    case _ => Option(callback).foreach(_.callback(null))
  }(Threading.Ui)

  def releaseCamera(): Future[Unit] = {
    loadFuture.cancel()
    Future {
      currentCamera.foreach { c =>
        c.release()
        currentCamera = None
      }
    }
  }

  def setFocusArea(touchRect: Rect, w: Int, h: Int) = Future {
    currentCamera match {
      case Some(c) => c.setFocusArea(touchRect, w, h)
      case _ => Future.failed(new RuntimeException("Can't set focus when camera is closed"))
    }
  }.flatten

  currentFlashMode.on(cameraExecutionContext)(fm => currentCamera.foreach(_.setFlashMode(fm)))

  deviceOrientation.on(cameraExecutionContext)(o => currentCamera.foreach(_.setOrientation(o)))

}

trait CameraFactory {
  def getAvailableCameraData(cameraManager: CameraManager): Seq[CameraData]

  def apply(data: CameraData, texture: SurfaceTexture, cxt: Context, width: Int, height: Int, devOrientation: Orientation, flashMode: FlashMode) : WireCamera
}

class AndroidCameraFactory extends CameraFactory {
  override def apply(data: CameraData, texture: SurfaceTexture, cxt: Context, width: Int, height: Int, devOrientation: Orientation, flashMode: FlashMode): WireCamera =
    new AndroidCamera2(data, cxt, width, height, flashMode, texture)

  override def getAvailableCameraData(cameraManager: CameraManager) = try {
    val cameraIds = cameraManager.getCameraIdList.filter { cameraId =>
      val characteristics = cameraManager.getCameraCharacteristics(cameraId)
      val capabilities = characteristics.get(
        CameraCharacteristics.REQUEST_AVAILABLE_CAPABILITIES)
      if (capabilities != null) {
        if (capabilities.contains(CameraMetadata.REQUEST_AVAILABLE_CAPABILITIES_BACKWARD_COMPATIBLE)) true else false
      } else {
        false
      }
    }

    val compatibleCameras = Seq.empty[CameraData].asJava
    cameraIds.foreach { id =>
      val characteristics = cameraManager.getCameraCharacteristics(id)
      val capabilities = characteristics.get(
        CameraCharacteristics.REQUEST_AVAILABLE_CAPABILITIES)
      val outputFormats = characteristics.get(
        CameraCharacteristics.SCALER_STREAM_CONFIGURATION_MAP).getOutputFormats
      compatibleCameras.add(CameraData(id, ImageFormat.JPEG))

      if (capabilities.contains(CameraMetadata.REQUEST_AVAILABLE_CAPABILITIES_RAW) &&
        outputFormats.contains(ImageFormat.RAW_SENSOR)) {
        compatibleCameras.add(CameraData(id, ImageFormat.RAW_SENSOR))
      }

      if (capabilities.contains(CameraMetadata.REQUEST_AVAILABLE_CAPABILITIES_DEPTH_OUTPUT) &&
        outputFormats.contains(ImageFormat.DEPTH_JPEG)) {
        compatibleCameras.add(CameraData(id, ImageFormat.DEPTH_JPEG))
      }
    }
    compatibleCameras.asScala
  } catch {
    case e: Throwable =>
      Logger.warn("GlobalCameraController", "Failed to retrieve camera info - camera is likely unavailable", e)
      Seq.empty
  }
}

trait WireCamera {
  def getPreviewSize: PreviewSize

  def takePicture(shutter: => Unit): Future[Array[Byte]]

  def release(): Unit

  def setOrientation(o: Orientation): Unit

  def setFocusArea(touchRect: Rect, w: Int, h: Int): Future[Unit]

  def setFlashMode(fm: FlashMode): Unit

  def getSupportedFlashModes: Set[FlashMode]

  def getCurrentCameraFacing: Int
}

object GlobalCameraController {
  val Ratio_16_9: Double = 16.0 / 9.0
  val MediumSize = 1448
}

object WireCamera {
  val FOCUS_MODE_AUTO = "auto"
  val FOCUS_MODE_CONTINUOUS_PICTURE = "continuous-picture"
  val ASPECT_TOLERANCE: Double = 0.1
  val camCoordsRange = 2000
  val camCoordsOffset = 1000
  val focusWeight = 1000
}

/**
 * Calculates the device's right-angle orientation based upon its rotation from its 'natural orientation',
 * which is always 0.
 */
trait Orientation {
  val orientation: Int
}

object Orientation {
  def apply(rot: Int): Orientation =
    if (rot == OrientationEventListener.ORIENTATION_UNKNOWN) Portrait_0
    else rot match {
      case r if (r > 315 && r <= 359) || (r >= 0 && r <= 45) => Portrait_0
      case r if r > 45 && r <= 135 => Landscape_90
      case r if r > 135 && r <= 225 => Portrait_180
      case r if r > 225 && r <= 315 => Landscape_270
      case _ =>
        Logger.warn("GlobalCameraController", "Unexpected orientation value: $rot")
        Portrait_0
    }
}

case object Portrait_0 extends Orientation {
  override val orientation: Int = 0
}

case object Landscape_90 extends Orientation {
  override val orientation: Int = 90
}

case object Portrait_180 extends Orientation {
  override val orientation: Int = 180
}

case object Landscape_270 extends Orientation {
  override val orientation: Int = 270
}

//CameraInfo.orientation is fixed for any given device, so we only need to store it once.
case class CameraInfo(id: Int, cameraFacing: CameraFacing, fixedOrientation: Int)

protected[camera] case class PreviewSize(w: Float, h: Float) {
  def hasSize = w != 0 && h != 0
}




