package de.sciss.unlike

import de.sciss.file._
import de.sciss.unlike.Morass.Config

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Random

object SoundPermutations extends App {
  val inputs    = (userHome / "Music" / "work").children(f => f.name.startsWith("mentasm-") && f.ext == "aif")
  val outputDir = userHome / "Documents" / "projects" / "Unlike" / "audio_work"

  println(s"There are ${inputs.size} input files.")
  outputDir.mkdir()

  Random.shuffle(inputs.combinations(2)).foreach {
    case Seq(inA, inB) =>
      val idA = inA.base.substring(8)
      val idB = inB.base.substring(8)
      val output = outputDir / s"mentasm-$idA-$idB.aif"
      if (!output.exists()) {
        val config = Config(input = inA, template = inB, output = output,
          synthesizeWinType = WindowFunction.Rectangle,
          inputWinSize = 4096, templateWinSize = 32768, stepSize = 16, ampModulation = 0.0675 /* 1.0 */, synthesizeWinAmt = 0.0625 )
        val proc = Morass(config)
        runAndMonitor(proc)
        Await.result(proc, Duration.Inf)
      }
  }

  println("All done.")
  sys.exit()
}
