/*
 *  MoorMotionStudy1b.scala
 *  (Unlike)
 *
 *  Copyright (c) 2015-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.unlike

import com.jhlabs.image.{GammaFilter, NoiseFilter}
import de.sciss.file._
import de.sciss.unlike.PhaseCorrelation.{Product => Frame}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, blocking}
import scala.util.Failure

object MoorMotionStudy1b {
  val moorBase: File = userHome / "Documents" / "projects" / "Unlike"

  val moorStudyConfig: Config = Config(
    inputTemp   = moorBase / "moor_8024" / "moor_8024-%05d.jpg",
    jsonDir     = moorBase / "moor_8024_json",
    outputTemp  = moorBase / "moor_8024_out" / "moor_8024-out-%05d.jpg",
    startFrame  = 61,
    endFrame    = 11601,
    gamma       = 0.5,
    noise       = 10
  )

  object Mode {
    case object Analyze extends Mode
    case object Write   extends Mode
    case object Both    extends Mode
  }
  sealed trait Mode

  case class Config(inputTemp : File    = file("input-%d.jpg"),
                    outputTemp: File    = file("output-%d.jpg"),
                    jsonDir   : File    = file("json"),
                    startFrame: Int     = 1,
                    endFrame  : Int     = 1000,
                    mode      : Mode    = Mode.Both,
                    twoStep   : Boolean = false,
                    gamma     : Double  = 1.0,
                    noise     : Int     = 0,
                    jpgQuality: Int     = 95,
                    verbose   : Boolean = false,
                    maxDispX  : Int     = 0,
                    maxDispY  : Int     = 0
                   )

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Moor-Study") {
      opt[File]("input")
        .text ("Input template - use %d as place-holder for frame number")
        .required()
        .action { (f, c) => c.copy(inputTemp = f) }

      opt[File]("output")
        .text ("Output template - use %d as place-holder for frame number")
        .required()
        .action { (f, c) => c.copy(outputTemp = f) }

      opt[File]("json")
        .text ("Directory for json motion files")
        .required()
        .action { (f, c) => c.copy(jsonDir = f) }

      opt[Int] ("start-frame")
        .text (s"First frame in input template (inclusive; default: ${default.startFrame})")
        .action   { (v, c) => c.copy(startFrame = v) }

      opt[Int] ("end-frame")
        .text ("Last frame in input template (inclusive)")
        .required()
        .action   { (v, c) => c.copy(endFrame = v) }

      opt[Unit] ('a', "analyze-only")
        .text ("Only perform analysis step")
        .action   { (_, c) => c.copy(mode = Mode.Analyze) }

      opt[Unit] ('w', "write-only")
        .text ("Only perform write step")
        .action   { (_, c) => c.copy(mode = Mode.Write) }

      opt[Int] ("noise")
        .text (s"Amount of noise to add (default: ${default.noise})")
        .action   { (v, c) => c.copy(noise = v) }

      opt[Double] ("gamma")
        .text (s"Gamma correction (default: ${default.gamma})")
        .action   { (v, c) => c.copy(gamma = v) }

      opt[Unit] ("verbose")
        .text ("Print additional information such as min/max translations")
        .action   { (_, c) => c.copy(verbose = true) }

      opt[Int] ("jpg-quality")
        .text (s"JPEG quality (default: ${default.jpgQuality})")
        .action   { (v, c) => c.copy(jpgQuality = v) }

      opt[Int] ("max-tx")
        .text (s"Maximum horizontal displacement per frame (default: ${default.maxDispX})")
        .action   { (v, c) => c.copy(maxDispX = v) }

      opt[Int] ("max-ty")
        .text (s"Maximum vertical displacement per frame (default: ${default.maxDispY})")
        .action   { (v, c) => c.copy(maxDispY = v) }
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      run(config)
    }
  }

  def run(config: Config): Unit = {
    import config._

//    lazy val base             = userHome / "Documents" / "projects" / "Unlike"
//    lazy val startFrame       =     1 + 60
//    lazy val endFrame         = 11601 // 11945 - 60
//    lazy val jsonDir          = base / "moor_8024_json"
    val renderDir        = outputTemp.parent

    if (!jsonDir  .exists()) jsonDir  .mkdir()
    if (!renderDir.exists()) renderDir.mkdir()

    val c1 = EstimateVideoMotion.Config(
      input     = inputTemp, // base / "moor_8024" / "moor_8024-%05d.jpg",
      output    = Some(jsonDir / "moor_8024-%05d-%05d.json"),
      frames    = startFrame to endFrame,
      settings  = PhaseCorrelation.Settings(txMax = maxDispX, tyMax = maxDispY)
    )
    val c2 = c1.copy(frames = startFrame     to endFrame by 2)
    val c3 = c1.copy(frames = startFrame + 1 to endFrame by 2)

    if (mode == Mode.Analyze || mode == Mode.Both) {
      val p1 = EstimateVideoMotion(c1)
      println("Analyze adjacent...")
      runAndMonitor(p1, exit = mode != Mode.Both && !twoStep, printResult = false)

      val lastProc = if (!twoStep) p1 else {
        Await.result(p1, Duration.Inf)

        val p2 = EstimateVideoMotion(c2)
        println("Analyze two-step even...")
        runAndMonitor(p2, exit = false, printResult = false)
        Await.result(p2, Duration.Inf)

        val p3 = EstimateVideoMotion(c3)
        println("Analyze two-step odd...")
        runAndMonitor(p3, exit = true, printResult = false)
        p3
      }

      if (mode == Mode.Both) Await.result(lastProc, Duration.Inf)

    }
    if (mode == Mode.Write || mode == Mode.Both) {

      val framesFut = Future[Vec[(Int, Frame)]](blocking {
        import PhaseCorrelation.{Product => Frame}
        def read(c: EstimateVideoMotion.Config): Map[(Int, Int), Frame] = {
          val seq = EstimateVideoMotion.read(c)
          c.frames.sliding(2).zip(seq.iterator).map {
            case (Seq(a, b), f) => (a, b) -> f
          } .toMap // (breakOut)
        }

        if (twoStep) {
          val map = read(c1) ++ read(c2) ++ read(c3)
          RenderVideoMotion.twoStepOptimization(c1.frames, map, weight = _ max _)
        } else {
          val seq = EstimateVideoMotion.read(c1)
          c1.frames zip (Frame.identity +: seq)
        }
      })

      println("Read JSON...")
      val frames  = Await.result(framesFut, Duration.Inf)

      val input   = c1.input

      val filters0  = if (gamma == 1.0) Nil else new GammaFilter(gamma.toFloat) :: Nil
      val filters   = if (noise <= 0) filters0 else {
        val fltNoise = new NoiseFilter
        fltNoise.setAmount(10)
        filters0 :+ fltNoise
      }

      val fmt = if (outputTemp.extL == "png") ImageFormat.PNG else ImageFormat.JPG(jpgQuality)
      val renCfg  = RenderVideoMotion.Config(input = input, output = outputTemp, format = fmt,
        frames = frames, filtersOut = filters, verbose = verbose /* , missing = RenderVideoMotion.Missing.Truncate */)
      val p = RenderVideoMotion(renCfg)
      println("Render...")
      p.onComplete {
        case Failure(e) => e.printStackTrace()
        case _ =>
      }
      runAndMonitor(p, exit = true, printResult = false)
    }
  }
}