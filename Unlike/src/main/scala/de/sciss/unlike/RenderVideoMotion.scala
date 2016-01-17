/*
 *  RenderVideoMotion.scala
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

import java.awt.geom.AffineTransform
import java.awt.{RenderingHints, Color}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.numbers
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{ProcessorLike, ProcessorFactory}

import scala.concurrent.blocking

object RenderVideoMotion extends ProcessorFactory {
  import PhaseCorrelation.{Product => Frame}

  object Missing {
    case object Truncate extends Missing
    case class Fill(rgba: Int = 0xFF000000) extends Missing
  }
  sealed trait Missing

  case class Config(input: File, output: File, format: ImageFormat = ImageFormat.PNG,
                    startFrame: Int, endFrame: Int, resetOutputCount: Boolean = true,
                    transforms: Vec[Frame], accumulate: Boolean = true,
                    moveToLast: Boolean = true, missing: Missing = Missing.Fill(),
                    verbose: Boolean = false) {

    require(endFrame - startFrame + 1 == transforms.size, s"Expecting ${transforms.size} frames")
  }

  type Product = Unit

  type Repr = RenderVideoMotion

  protected def prepare(config: RenderVideoMotion.Config): Prepared = new Impl(config)

  private final class Impl(val config: Config) extends ProcessorImpl[Product, Repr] with Repr {
    protected def body(): Unit = {
      import config.{transforms => transforms0, _}

      def mkFile(template: File, fr: Int): File = {
        val name = template.name.format(fr)
        template.parentOption.fold[File](file(name))(_ / name)
      }

      def mkInput (fr: Int): File = mkFile(input , fr)
      def mkOutput(fr: Int): File = mkFile(output, fr)

      val numFramesM  = endFrame - startFrame
      val numFrames   = numFramesM + 1

      val outFrOff    = if (resetOutputCount) 1 else startFrame

      var transforms = transforms0
      transforms match {
        case head +: tail if accumulate =>
          transforms = tail.scanLeft(head) { (pred, next) =>
            next.copy(translateX = pred.translateX + next.translateX,
                      translateY = pred.translateY + next.translateY)
          }
        case _ =>
      }

      transforms match {
        case init :+ last if moveToLast =>
          val totTx = last.translateX
          val totTy = last.translateY

          if (verbose) println(f"last ($totTx%1.2f $totTy%1.2f)")

          transforms = transforms.zipWithIndex.map { case (in, idx) =>
            import numbers.Implicits._
            val offX = totTx * idx.linlin(0, numFramesM, 0, -1)
            val offY = totTy * idx.linlin(0, numFramesM, 0, -1)
            in.copy(translateX = in.translateX + offX,
                    translateY = in.translateY + offY)
          }

        case _ =>
      }

      def reduce(elem: Frame => Double)(acc: Iterator[Double] => Double) =
        if (transforms.isEmpty) 0.0 else acc(transforms.iterator.map(elem))

      val meanTx  = reduce(_.translateX)(_.sum) / numFrames
      val meanTy  = reduce(_.translateY)(_.sum) / numFrames
      val minTx   = reduce(_.translateX)(_.min) - meanTx
      val maxTx   = reduce(_.translateX)(_.max) - meanTx
      val minTy   = reduce(_.translateY)(_.min) - meanTy
      val maxTy   = reduce(_.translateY)(_.max) - meanTy

      if (verbose)
        println(f"mean ($meanTx%1.2f, $meanTy%1.2f), min ($minTx%1.2f, $minTy%1.2f), max ($maxTx%1.2f, $maxTy%1.2f)")

      val (dx, dy, dw, dh, bg) = missing match {
        case Missing.Truncate =>
          import math.floor
          (-maxTx, -maxTy, floor(-maxTx + minTx).toInt, floor(-maxTy + minTy).toInt, Color.black)
//           floor(min(-maxTx, minTx)).toInt, floor(min(-maxTy, minTy)).toInt, Color.black

        case Missing.Fill(rgba) =>
          (-meanTx, -meanTy, 0, 0, new Color(rgba, true))
      }

      if (verbose) println(f"dx/y ($dx%1.2f, $dy%1.2f), dw/h ($dw%d, $dh%d)")

      transforms.zipWithIndex.foreach { case (prod, frameOff) =>
        import prod.{translateX, translateY}
        blocking {
          val fOut = mkOutput(frameOff + outFrOff)
          if (!fOut.exists()) {
            val fIn       = mkInput(frameOff + startFrame)
            val imageIn   = ImageIO.read(fIn)
            val imgWIn    = imageIn.getWidth
            val imgHIn    = imageIn.getHeight
            val imgWOut   = math.max(1, imgWIn + dw)
            val imgHOut   = math.max(1, imgHIn + dh)
            val imageOut  = {
              // Note: JPG export fails with `TYPE_INT_ARGB`!
              val res = new BufferedImage(imgWOut, imgHOut, BufferedImage.TYPE_INT_RGB)
              val g   = res.createGraphics()
              g.setColor(bg)
              g.fillRect(0, 0, imgWOut, imgHOut)
              g.setRenderingHint(RenderingHints.KEY_ANTIALIASING , RenderingHints.VALUE_ANTIALIAS_ON         )
              g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
              g.setRenderingHint(RenderingHints.KEY_RENDERING    , RenderingHints.VALUE_RENDER_QUALITY       )
              val at = AffineTransform.getTranslateInstance(translateX + dx, translateY + dy)
              g.transform(at)
              g.drawImage(imageIn, 0, 0, null)
              g.dispose()
              res
            }
            format.write(fOut, imageOut)
          }
        }
        checkAborted()
        progress = (frameOff + 1).toDouble / numFrames
      }
    }
  }
}
trait RenderVideoMotion extends ProcessorLike[RenderVideoMotion.Product, RenderVideoMotion] {
  def config: RenderVideoMotion.Config
}
