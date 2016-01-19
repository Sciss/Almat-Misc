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
import java.awt.image.{BufferedImageOp, BufferedImage}
import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.numbers
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{ProcessorLike, ProcessorFactory}

import scala.annotation.tailrec
import scala.concurrent.blocking

object RenderVideoMotion extends ProcessorFactory {
  import PhaseCorrelation.{Product => Frame}

  object Missing {
    case object Truncate extends Missing
    case class Fill(rgba: Int = 0xFF000000) extends Missing
  }
  sealed trait Missing

  case class Config(input: File, output: File, format: ImageFormat = ImageFormat.PNG,
                    frames: Vec[(Int, Frame)], resetOutputCount: Boolean = true,
                    accumulate: Boolean = true,
                    moveToLast: Boolean = true, missing: Missing = Missing.Fill(),
                    filters: Seq[BufferedImageOp] = Nil,
                    verbose: Boolean = false) {
  }

  type Product = Unit

  type Repr = RenderVideoMotion

  protected def prepare(config: RenderVideoMotion.Config): Prepared = new Impl(config)

  /** @param weight combination for two adjacent peaks, such as `math.min`, `math.max` or mean (default). */
  def twoStepOptimization(frames: Vec[Int], transforms: Map[(Int, Int), Frame],
                          weight: (Double, Double) => Double = { (a, b) => (a + b)/2 }): Vec[(Int, Frame)] = {
    @tailrec
    def loop(in: Vec[Int], out: Vec[(Int, Frame)]): Vec[(Int, Frame)] =
      in match {
        case a +: (tail1 @ b +: tail0) =>
          val ab = transforms((a, b))

          tail0 match {
            case c +: tail =>
              val bc = transforms((b, c))
              val ac = transforms((a, c))

              val wPeak = weight(ab.peak, bc.peak)
              if (wPeak < ac.peak) {
                import ab.{translateX => abx, translateY => aby}
                import bc.{translateX => bcx, translateY => bcy}
                import ac.{translateX => acx, translateY => acy}
                val p1x = abx + bcx
                val p1y = aby + bcy
                import numbers.Implicits._
                // If b lies outside ac, leave it as is.
                // The two alternatives would be:
                // - clip, i.e. `abx.clip(0, p1x).linlin(...)`
                // - fall back to `else` case (do not exchange transforms at all)
                val p2x = if (abx >= 0 && abx <= p1x) abx.linlin(0, p1x, 0, acx) else abx
                val p2y = if (aby >= 0 && aby <= p1y) aby.linlin(0, p1y, 0, acy) else aby
                val p3x = acx - p2x
                val p3y = acy - p2y
                val abT = ab.copy(translateX = p2x, translateY = p2y)
                val bcT = bc.copy(translateX = p3x, translateY = p3y)
                // drop `a` and `b`, add two transformed pairs
                loop(in = tail0, out = out :+ (b -> abT) :+ (c -> bcT))

              } else {  // drop `a`, keep first pair
                loop(in = tail1, out = out :+ (b -> ab))
              }

            case _ =>
              out :+ (b -> ab)
          }

        case _ => out
      }

    val out0 = frames.headOption.fold[Vec[(Int, Frame)]](Vector.empty)(a => Vector(a -> Frame.identity))
    loop(in = frames, out = out0)
  }

  private final class Impl(val config: Config) extends ProcessorImpl[Product, Repr] with Repr {
    override def toString = s"RenderVideoMotion@${hashCode.toHexString}"

    protected def body(): Unit = {
      import config._

      def mkFile(template: File, fr: Int): File = {
        val name = template.name.format(fr)
        template.parentOption.fold[File](file(name))(_ / name)
      }

      def mkInput (fr: Int): File = mkFile(input , fr)
      def mkOutput(fr: Int): File = mkFile(output, fr)

      val numFrames   = frames.size   // numFramesM + 1
      val numFramesM  = numFrames - 1 // endFrame - startFrame

      val outFrOff    = if (resetOutputCount) 1 else frames.headOption.fold(1)(_._1)

      var transforms = frames.map(_._2) // transforms0
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

      (transforms zip frames.map(_._1)).zipWithIndex.foreach { case ((prod, frameIn), frameOff) =>
        import prod.{translateX, translateY}
        blocking {
          val fOut = mkOutput(frameOff + outFrOff)
          if (!fOut.exists()) {
            val fIn       = mkInput(frameIn)
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
              val imageFlt = (imageIn /: filters)((in, op) => op.filter(in, null))
              g.drawImage(imageFlt, 0, 0, null)
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
