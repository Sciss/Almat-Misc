/*
 *  BirdTest.scala
 *  (Unlike)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.unlike

import java.awt.RenderingHints
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.util.concurrent.TimeUnit
import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.kollflitz
import de.sciss.processor.Processor

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, blocking}

object BirdTest extends App {
  val startFrame  =   60
  val endFrame    = 2170

  val pAll = Processor[Unit]("Render") { self =>
    val numFramesM  = endFrame - startFrame
    val numFrames   = numFramesM + 1
    val progCount   = numFramesM + numFrames
    // val numFrames   = endFrame - startFrame + 1
    val TimeOut     = Duration(10, TimeUnit.SECONDS)
    val products    = Seq.tabulate[PhaseCorrelation.Product](numFramesM) { frameOff =>
      val inFrameA = frameOff + startFrame
      val inFrameB = inFrameA + 1
      val config = PhaseCorrelation.Config(
        pathA = mkBirdIn(inFrameA),
        pathB = mkBirdIn(inFrameB),
        downSample = 1.0)

      val pCorr = PhaseCorrelation(config)
      pCorr.start()
      val prod  = Await.result(pCorr, TimeOut)
      self.checkAborted()
      self.progress = (frameOff + 1).toDouble / progCount
      // println(prod)
      prod
    }

    import kollflitz.Ops._

    val tx0     = products.map(_.translateX).integrate
    val ty0     = products.map(_.translateY).integrate
    val meanTx  = tx0.mean
    val meanTy  = ty0.mean

    // println(s"meanTx = $meanTx, meanTy = $meanTy")

    val tx      = (0.0 +: tx0).map(_ - meanTx)
    val ty      = (0.0 +: ty0).map(_ - meanTy)

    val products1 = tx zip ty
    products1.zipWithIndex.foreach { case ((translateX, translateY), frameOff) =>
      //      val prod = prod0.copy(translateX = prod0.translateX - meanTx,
      //                            translateY = prod0.translateY - meanTy)
      blocking {
        val fOut = mkBirdOut(frameOff + 1)
        if (!fOut.exists()) {
          val imageIn   = ImageIO.read(mkBirdIn(frameOff + startFrame))
          val imageOut  = {
            // Note: JPG export fails with `TYPE_INT_ARGB`!
            val res = new BufferedImage(imageIn.getWidth, imageIn.getHeight, BufferedImage.TYPE_INT_RGB)
            val g   = res.createGraphics()
            g.drawImage(imageIn, 0, 0, null)  // "background"
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING , RenderingHints.VALUE_ANTIALIAS_ON         )
            g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
            g.setRenderingHint(RenderingHints.KEY_RENDERING    , RenderingHints.VALUE_RENDER_QUALITY       )
            val at = AffineTransform.getTranslateInstance(translateX, translateY)
            g.transform(at)
            // g.drawImage(imageIn, at, null)
            g.drawImage(imageIn, 0, 0, null)
            g.dispose()
            res
          }
          ImageIO.write(imageOut, "jpg", fOut)
        }
      }
      self.checkAborted()
      self.progress = (frameOff + 1 + numFramesM).toDouble / progCount
    }

    ()
  }

  waitForProcessor(pAll)
  println("_" * 33)
  pAll.monitor()
  pAll.onSuccess {
    case _ =>
      Thread.sleep(200)
      sys.exit()
  }

  def mkBirdIn (frame: Int): File = file("_creation") / "bird"     / f"$frame%04d.jpg"
  def mkBirdOut(frame: Int): File = file("_creation") / "bird_out" / f"bird-$frame%d.jpg"
}