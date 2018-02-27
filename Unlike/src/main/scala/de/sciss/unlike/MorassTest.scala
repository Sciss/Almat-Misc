/*
 *  MorassTest.scala
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
import de.sciss.synth.io.{AudioFileType, SampleFormat}
import de.sciss.unlike.Morass.Config

object MorassTest {
  def main(args: Array[String]): Unit = {
    def findWindow(name: String): Option[WindowFunction] = WindowFunction.all.find(_.name == name)

    val p = new scopt.OptionParser[Config]("Morass") {
      private def validateWindow(v: String): Either[String, Unit] = {
        if (findWindow(v).isDefined) success
        else failure(WindowFunction.all.map(_.name).mkString("window type must be one of: ", ", ", ""))
      }

      arg[File]("input")
        .text ("Input audio file to spread")
        .action { (f, c) => c.copy(input = f) }

      arg[File]("template")
        .text ("Template audio file to correlate input with")
        .action { (f, c) => c.copy(template = f) }

      arg[File]("output")
        .text ("Output audio file to write")
        .action { (f, c) => c.copy(output   = f) }

      opt[Int] ('f', "sample-format")
        .text ("Output sample format, one of 'int16', 'int24', 'int32', 'float', 'double', (default: float")
        .action   { (v, c) => c.copy(outputSampleFormat = SampleFormat.fromInt16.find(_.id == v).get) }
        .validate { v =>
          val ok = SampleFormat.fromInt16.exists(_.id == v)
          if (ok) success else failure(s"Unsupported sample format $v")
        }

      opt[Int] ('v', "in-window")
        .text ("Window size in sample frames for input in phase correlation (default: 16384")
        .action   { (v, c) => c.copy(inputWinSize = v) }
        .validate {  v     => if (v >= 2) success else failure("window size must be > 1") }

      opt[Int] ('w', "temp-window")
        .text ("Window size in sample frames for template in phase correlation (default: 16384")
        .action   { (v, c) => c.copy(templateWinSize = v) }
        .validate {  v     => if (v >= 2) success else failure("window size must be > 1") }

      opt[String] ('a', "analysis-window")
        .text ("Window type for template in phase correlation (default: 'hanning'")
        .action   { (v, c) => c.copy(analyzeWinType = findWindow(v).get) }
        .validate(validateWindow)

      opt[String] ('y', "synthesis-window")
        .text ("Window type for template in phase correlation (default: 'hanning'")
        .action   { (v, c) => c.copy(synthesizeWinType = findWindow(v).get) }
        .validate(validateWindow)

      opt[Double] ('z', "synthesis-window-amount")
        .text ("Proportional length of synthesis-window function (1.0 = full, 0.0 = rectangle, default: 1.0")
        .action   { (v, c) => c.copy(synthesizeWinAmt = v) }
        .validate { v => if (v >= 0 & v <= 1) success else failure("Synthesis-window-amount must be >= 0 and <= 1") }

      opt[Double] ('m', "amplitude")
        .text ("Amount of amplitude modulation (0.0 to 1.0, default: 0.0)")
        .action   { (v, c) => c.copy(ampModulation = v) }

      opt[Int] ('s', "step")
        .text ("Step size in sample frames between successive correlations (default: 16)")
        .action   { (v, c) => c.copy(stepSize = v) }
        .validate {  v     => if (v >= 1) success else failure("step size must be > 0") }

      opt[Double] ('r', "radius")
        .text ("Maximum phase shift (0.0 to 1.0, default: 1.0)")
        .action   { (v, c) => c.copy(radius = v) }
        .validate {  v     => if (v >= 0.0 && v <= 1.0) success else failure("radius must be >= 0 and <= 1") }
    }
    p.parse(args, Config()).fold(sys.exit(1)) { config0 =>
      val outputExt = config0.output.ext.toLowerCase
      val config    = AudioFileType.writable.find(_.extensions.contains(outputExt))
        .fold(config0)(tpe => config0.copy(outputFileType = tpe))
      val proc = Morass(config)
      runAndMonitor(proc, exit = true)
    }
  }
}
