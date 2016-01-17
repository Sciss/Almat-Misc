/*
 *  EstimateVideoMotion.scala
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

import de.sciss.file._
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{ProcessorFactory, ProcessorLike}

object EstimateVideoMotion extends ProcessorFactory {
  import PhaseCorrelation.{Product => Frame}

  /** @param input        input template file, such as `my-dir/frame-%d.png`
    * @param startFrame   first frame to process that will be applied to the `input` template
    * @param endFrame     last frame (inclusive) to process that will be applied to the `input` template
    * @param outputDir    directory to store the motion estimation data in. Files using
    *                     the `input` template's name will be created here, exchanging the
    *                     file extension for `.json`.
    * @param overwrite    if `true` reprocess input frames even if `.json` files already exist for it.
    *                     if `false`, those frames will be skipped
    */
  case class Config(input: File, startFrame: Int, endFrame: Int, outputDir: Option[File],
                    overwrite: Boolean = false,
                    settings: PhaseCorrelation.Settings = PhaseCorrelation.Settings()) {

    require(startFrame <= endFrame, s"startFrame ($startFrame) must be <= endFrame ($endFrame)")
  }

  type Product = Vec[Frame]

  type Repr = EstimateVideoMotion

  protected def prepare(config: Config): Prepared = new Impl(config)

  def read(config: Config): Product = {
    import config._
    val dir = outputDir.get
    startFrame to endFrame map { frame =>
      val f = (dir / input.name.format(frame)).replaceExt("json")
      JsonUtil.read[Frame](f)
    }
  }

  private final class Impl(val config: Config) extends ProcessorImpl[Product, Repr] with Repr {
    import config._

    protected def body(): Product = {
      val numFramesM  = endFrame - startFrame
      val numFrames   = numFramesM + 1

      def mkInput(fr: Int): File = {
        val name = input.name.format(fr)
        input.parentOption.fold[File](file(name))(_ / name)
      }

      def mkOutput(fr: Int): Option[File] = outputDir.map { dir =>
        (dir / input.name.format(fr)).replaceExt("json")
      }

      mkOutput(startFrame).foreach { f =>
        if (!f.exists() || overwrite) JsonUtil.write[Frame](Frame.identity, f)
      }
      var frame       = startFrame + 1
      val progWeight  = 1.0 / numFrames
      val b           = Vector.newBuilder[Frame]

      while (frame <= endFrame) {
        val inFrameA  = frame - 1
        val inFrameB  = frame
        val outputF   = mkOutput(inFrameB)
        val progOff   = (frame - startFrame).toDouble * progWeight
        val prod0     = outputF.collect {
          case f if f.exists() && !overwrite => JsonUtil.read[Frame](f)
        }
        val prod    = prod0.getOrElse {
          val c = PhaseCorrelation.Config(pathA = mkInput(inFrameA), pathB = mkInput(inFrameB), settings = settings)
          val pCorr = PhaseCorrelation(c)
          pCorr.start()
          val res = await(pCorr, offset = progOff, weight = progWeight)
          outputF.foreach { f =>
            JsonUtil.write[Frame](res, f)
          }
          res
        }
        b += prod

        frame += 1
        checkAborted()
        progress = progOff + progWeight
      }
      b.result()
    }
  }
}
trait EstimateVideoMotion extends ProcessorLike[EstimateVideoMotion.Product, EstimateVideoMotion] {
  def config: EstimateVideoMotion.Config
}