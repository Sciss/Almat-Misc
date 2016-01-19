/*
 *  MoorMotionStudy1b.scala
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

import com.jhlabs.image.{NoiseFilter, GammaFilter}
import de.sciss.file._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, blocking}

object MoorMotionStudy1b extends App {
  val base        = userHome / "Documents" / "projects" / "Unlike"
  val mode        = "WRITE"
  val startFrame  =     1 + 60
  val endFrame    = 11601 // 11945 - 60
  val jsonDir     = base / "moor_8024_json"
  val renderDir   = base / "moor_8024_out"

  val c1 = EstimateVideoMotion.Config(
    input   = base / "moor_8024" / "moor_8024-%05d.jpg",
    output  = Some(jsonDir / "moor_8024-%05d-%05d.json"),
    frames  = startFrame to endFrame
  )
  val c2 = c1.copy(frames = startFrame     to endFrame by 2)
  val c3 = c1.copy(frames = startFrame + 1 to endFrame by 2)

  if (mode == "ANALYZE") {
    val p1 = EstimateVideoMotion(c1)
    println("Analyze adjacent...")
    runAndMonitor(p1, exit = false, printResult = false)
    Await.result(p1, Duration.Inf)

    val p2 = EstimateVideoMotion(c2)
    println("Analyze two-step even...")
    runAndMonitor(p2, exit = false, printResult = false)
    Await.result(p2, Duration.Inf)

    val p3 = EstimateVideoMotion(c3)
    println("Analyze two-step odd...")
    runAndMonitor(p3, exit = true, printResult = false)

  } else if (mode == "WRITE") {

    val framesFut = Future(blocking {
      import PhaseCorrelation.{Product => Frame}
      def read(c: EstimateVideoMotion.Config): Map[(Int, Int), Frame] = {
        val seq = EstimateVideoMotion.read(c)
        c.frames.sliding(2).zip(seq.iterator).map {
          case (Seq(a, b), f) => (a, b) -> f
        } .toMap // (breakOut)
      }

      val map = read(c1) ++ read(c2) ++ read(c3)
      RenderVideoMotion.twoStepOptimization(c1.frames, map)
    })
    println("Read JSON...")
    val frames  = Await.result(framesFut, Duration.Inf)

    val input   = c1.input
    val output  = renderDir / "moor_8024-out-%05d.jpg"

    val fltGamma  = new GammaFilter(0.5f)
    val fltNoise  = new NoiseFilter
    fltNoise.setAmount(10)

    val renCfg  = RenderVideoMotion.Config(input = input, output = output, format = ImageFormat.JPG(),
      frames = frames, filters = fltGamma :: fltNoise :: Nil /* , missing = RenderVideoMotion.Missing.Truncate */)
    val p = RenderVideoMotion(renCfg)
    println("Render...")
    p.onFailure {
      case e => e.printStackTrace()
    }
    runAndMonitor(p, exit = true, printResult = false)

  } else {
    throw new UnsupportedOperationException(mode)
  }
}
