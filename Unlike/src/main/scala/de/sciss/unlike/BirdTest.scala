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

import java.awt.{Color, RenderingHints}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.util.concurrent.TimeUnit
import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.{numbers, kollflitz}
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
        pathB = mkBirdIn(inFrameB))

      val pCorr = PhaseCorrelation(config)
      pCorr.start()
      val prod  = Await.result(pCorr, TimeOut)
      self.checkAborted()
      self.progress = (frameOff + 1).toDouble / progCount
      // println(prod)
      prod
    }

    import kollflitz.Ops._

    val tx0     = 0.0 +: products.map(_.translateX).integrate
    val ty0     = 0.0 +: products.map(_.translateY).integrate
    val totTx   = tx0.last
    val totTy   = ty0.last

    val tx1     = tx0.zipWithIndex.map { case (in, idx) =>
      import numbers.Implicits._
      val off = totTx * idx.linlin(0, numFramesM, 0, -1)
      in /* - meanTx */ + off
    }
    val ty1     = ty0.zipWithIndex.map { case (in, idx) =>
      import numbers.Implicits._
      val off = totTy * idx.linlin(0, numFramesM, 0, -1)
      in /* - meanTy */ + off
    }

    val meanTx  = tx1.mean
    val meanTy  = ty1.mean

    // println(s"meanTx = $meanTx, meanTy = $meanTy")

    val tx      = tx1.map(_ - meanTx)
    val ty      = ty1.map(_ - meanTy)

    val products1 = tx zip ty
    products1.zipWithIndex.foreach { case ((translateX, translateY), frameOff) =>
      blocking {
        val fOut = mkBirdOut(frameOff + 1)
        if (!fOut.exists()) {
          val imageIn   = ImageIO.read(mkBirdIn(frameOff + startFrame))
          val imgW      = imageIn.getWidth
          val imgH      = imageIn.getHeight
          val imageOut  = {
            // Note: JPG export fails with `TYPE_INT_ARGB`!
            val res = new BufferedImage(imgW, imgH, BufferedImage.TYPE_INT_RGB)
            val g   = res.createGraphics()
            // g.drawImage(imageIn, 0, 0, null)  // "background"
            g.setColor(Color.black)
            g.fillRect(0, 0, imgW, imgH)
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