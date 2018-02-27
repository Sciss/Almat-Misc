/*
 *  EstimateVideoMotion.scala
 *  (Unlike)
 *
 *  Copyright (c) 2015-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.unlike

import de.sciss.file._
import de.sciss.kollflitz
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{ProcessorFactory, ProcessorLike}
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_2D

object EstimateVideoMotion extends ProcessorFactory {
  import PhaseCorrelation.{Product => Frame}

//  * @param twoStep      if `true`, tries to improve stability by choosing the best peak between
//  *                     directly adjacent frames and images spaced two frames apart. This will
//  *                     obviously make the process take twice the time.

  /** @param input        input template file, such as `my-dir/frame-%d.png`. It must have one `%d`
    *                     field.
    * @param frames       the sequence of frames to process, applying each index to the `input` template
    * @param output       output template to store the motion estimation data in, such as
    *                     `my-dir/motion-%d-%d.json`. It must have two `%d` fields.
    * @param overwrite    if `true` reprocess input frames even if JSON files already exist for it.
    *                     if `false`, those frames will be skipped
    */
  case class Config(input: File, frames: Vec[Int], output: Option[File],
                    overwrite: Boolean = false,
                    settings: PhaseCorrelation.Settings = PhaseCorrelation.Settings())

  type Product = Vec[Frame]

  type Repr = EstimateVideoMotion

  protected def prepare(config: Config): Prepared = new Impl(config)

  /** Reads the motion data for the given frame sequences. The result
    * will be collection for the adjacent frame-pairs in the configuration's
    * frames field, i.e. one size less than `frames.length`.
    *
    * Throws an exception, if `config.output` is not defined.
    */
  def read(config: Config): Product = {
    import config._
    import kollflitz.Ops._
    val template = output.get
    frames.mapPairs { (frameA, frameB) =>
      val f = mkOutput(frameA, frameB, template)
      JsonUtil.read[Frame](f)
    }
  }

  def mkOutput(frameA: Int, frameB: Int, template: File): File = {
    val name = template.name.format(frameA, frameB)
    template.parentOption.fold[File](file(name))(_ / name)
  }

  private final class Impl(val config: Config) extends ProcessorImpl[Product, Repr] with Repr {
    import config._

    override def toString = s"EstimateVideoMotion@${hashCode.toHexString}"

    protected def body(): Product = {

      def mkInput(fr: Int): File = {
        val name = input.name.format(fr)
        input.parentOption.fold[File](file(name))(_ / name)
      }

      def mkOutputOpt(frameA: Int, frameB: Int): Option[File] = output.map(mkOutput(frameA, frameB, _))

      val numFrames   = frames.size
      val progWeight  = 1.0 / (numFrames - 1)

      var pred: Image        = null
      var predFrame          = Int.MinValue
      var fft : DoubleFFT_2D = null

      def prepareData(fr: Int) = {
        val img = PhaseCorrelation.prepareImage(mkInput(fr), settings)
        if (fft == null) fft = PhaseCorrelation.prepareFFT(img)
        PhaseCorrelation.prepareData(img, fft)
      }

      import kollflitz.Ops._
      var idx = 0
      frames.mapPairs { (frameA, frameB) =>
        val outputF   = mkOutputOpt(frameA, frameB)
        val prod0     = outputF.collect {
          case f if f.exists() && !overwrite => JsonUtil.read[Frame](f)
        }
        val prod = prod0.getOrElse {
          if (predFrame != frameA) {
            pred      = prepareData(frameA)
            predFrame = frameA
          }
          val succ = prepareData(frameB)
//          val pCorr = PhaseCorrelation(c)
//          pCorr.start()
//          val res = await(pCorr, offset = progOff, weight = progWeight)
          val res = PhaseCorrelation.process(imgTA = pred, imgTB = succ, fft = fft, settings = settings)
          outputF.foreach { f =>
            JsonUtil.write[Frame](res, f)
          }
          pred      = succ
          predFrame = frameB
          res
        }

        checkAborted()
        idx += 1
        progress = idx.toDouble * progWeight

        prod
      }
    }
  }
}
trait EstimateVideoMotion extends ProcessorLike[EstimateVideoMotion.Product, EstimateVideoMotion] {
  def config: EstimateVideoMotion.Config
}