/*
 *  Unlike.scala
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
import de.sciss.unlike.PhaseCorrelation.{Product => Frame}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, blocking}

object Unlike extends App {
  lazy val mode             = "VIDEO"   // either "WRITE" or "ANALYZE" or "BOTH", or "VIDEO"

  lazy val base             = userHome / "Documents" / "projects" / "Unlike"
  lazy val startFrame       = 9227
  lazy val endFrame         = 9335
  lazy val skipFrames       = Vec(3, 15, 28, 44, 45, 47, 48, 50, 52, 55, 57, 58, 60, 63, 65, 67, 69) // re -out
  lazy val inputDir         = base / "unlike_in"
  lazy val jsonDir          = base / "unlike_json"
  lazy val renderDir        = base / "unlike_out"
  lazy val videoDir         = base / "unlike_vid"
  lazy val inputTemplate    = inputDir  / "DSC_%04d.JPG"
  lazy val jsonTemplate     = jsonDir   / "unlike-out-%04d-%04d.json"
  lazy val outputTemplate   = renderDir / "unlike-out-%04d.jpg"
  lazy val videoTemplate    = videoDir  / "unlike-vid-%04d.jpg"

  if (!jsonDir  .exists()) jsonDir  .mkdir()
  if (!renderDir.exists()) renderDir.mkdir()

  lazy val c1 = EstimateVideoMotion.Config(
    input     = inputTemplate,
    output    = Some(jsonTemplate),
    frames    = startFrame to endFrame,
    settings  = PhaseCorrelation.Settings(downSample = 2.0)
  )

  require (mode == "ANALYZE" || mode == "WRITE" || mode == "BOTH" || mode == "VIDEO")

  if (mode == "ANALYZE" || mode == "BOTH") {
    val p1 = EstimateVideoMotion(c1)
    println("Analyze adjacent...")
    runAndMonitor(p1, exit = mode != "BOTH", printResult = false)
    val lastProc = p1

    if (mode == "BOTH") Await.result(lastProc, Duration.Inf)
  }

  if (mode == "WRITE" || mode == "BOTH") {

    val framesFut = Future[Vec[(Int, Frame)]](blocking {
      import PhaseCorrelation.{Product => Frame}
      val seq = EstimateVideoMotion.read(c1)
      c1.frames zip (Frame.identity +: seq)
    })

    println("Read JSON...")
    val frames  = Await.result(framesFut, Duration.Inf)

    val input   = c1.input

    //    val fltGamma  = new GammaFilter(0.5f)
    //    val fltNoise  = new NoiseFilter
    //    fltNoise.setAmount(10)

    val renCfg  = RenderVideoMotion.Config(input = input, output = outputTemplate, format = ImageFormat.JPG(),
      frames = frames, filters = /* fltGamma :: fltNoise :: */ Nil, missing = RenderVideoMotion.Missing.Truncate,
      downSample = 2.0)
    val p = RenderVideoMotion(renCfg)
    println("Render...")
    p.onFailure {
      case e => e.printStackTrace()
    }
    runAndMonitor(p, exit = true, printResult = false)
  }

  // `avconv -r 1 -i unlike_vid/unlike-vid-%04d.jpg unlike.mp4`
  if (mode == "VIDEO") {
    val numIn     = (startFrame to endFrame).size
    val framesIn  = (1 to numIn) diff skipFrames
    framesIn.zipWithIndex.foreach { case (frameIn, frameOut0) =>
      val frameOut  = frameOut0 + 1
      val fIn       = outputTemplate.parent / outputTemplate.name.format(frameIn )
      val fOut      = videoTemplate .parent / videoTemplate .name.format(frameOut)
      if (!fOut.exists()) {
        import sys.process._
        val cmd = Seq("ln", "-s", fIn.path, fOut.path)
        val res = cmd.!
        require(res == 0, cmd.mkString("Failed: ", " ", ""))
      }
    }
  }
}
