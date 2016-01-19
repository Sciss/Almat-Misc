/*
 *  MoorMotionStudy1.scala
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

import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Await, blocking}

object MoorMotionStudy1 extends App {
  val base        = userHome / "Documents" / "projects" / "Unlike"
  val mode        = "WRITE"
  val startFrame  =     1 + 60
  val endFrame    = 11945 - 60
  val jsonDir     = base / "moor_8024_json"
  val renderDir   = base / "moor_8024_out"

  val estConf = EstimateVideoMotion.Config(
    input   = base / "moor_8024" / "moor_8024-%05d.jpg",
    output  = Some(jsonDir / "moor_8024-%05d-%05d.json"),
    frames  = startFrame to endFrame
  )

  if (mode == "ANALYZE") {
    val p = EstimateVideoMotion(estConf)
    println("Analyze...")
    runAndMonitor(p, exit = true, printResult = false)

  } else if (mode == "WRITE") {
    val read    = Future(blocking(EstimateVideoMotion.read(estConf)))
    println("Read JSON...")
    val prod    = PhaseCorrelation.Product.identity +: Await.result(read, Duration.Inf)

    val input   = estConf.input
    val output  = renderDir / "moor_8024-out-%05d.jpg"
    val renCfg  = RenderVideoMotion.Config(input = input, output = output, format = ImageFormat.JPG(),
      frames = (startFrame to endFrame) zip prod /* , missing = RenderVideoMotion.Missing.Truncate */)
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
