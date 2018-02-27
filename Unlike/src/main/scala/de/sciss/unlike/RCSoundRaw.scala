/*
 *  RCSoundRaw.scala
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
import de.sciss.unlike.Morass.Config

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object RCSoundRaw extends App {
  val inputDir  = userHome / "Music" / "work"
  val outputDir = userHome / "Documents" / "projects" / "Unlike" / "audio_work"
  val in1       = inputDir / "mentasm-e8646341.aif"
  val in2       = inputDir / "mentasm-63dcf8a8.aif"
  val out       = outputDir / "mentasm-e8646341-63dcf8a8-RAW.aif"

  if (out.exists()) sys.exit()

  val config = Config(input = in1, template = in2, output = out,
    synthesizeWinType = WindowFunction.Rectangle,
    inputWinSize = 4096, templateWinSize = 32768, stepSize = 16, ampModulation = 0 /* 0.0675 */ /* 1.0 */,
    synthesizeWinAmt = 1.0 /* 0.0625 */)
  val proc = Morass(config)
  println("_" * 33)
  proc.monitor(printResult = false)
  proc.start()
  Await.result(proc, Duration.Inf)
  sys.exit()
}
