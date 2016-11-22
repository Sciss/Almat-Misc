package de.sciss.unlike

import de.sciss.file._
import de.sciss.fscape.FScapeJobs
import de.sciss.fscape.FScapeJobs.{Gain, OutputSpec}
import de.sciss.unlike.Morass.Config

import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Await, Promise}

object SoundPermutations extends App {
  val inputs    = (userHome / "Music" / "work").children(f => f.name.startsWith("mentasm-") && f.ext == "aif")
  val outputDir = userHome / "Documents" / "projects" / "Unlike" / "audio_work"

  println(s"There are ${inputs.size} input files.")
  // println(s"There are ${inputs.combinations(2).size} combinations.")
  outputDir.mkdir()

  /* val sync = */ mkBlockTread()

  val fscape = FScapeJobs()
  fscape.connect(5.0) {
    case true =>
//      fscape.verbose = true
//      fscape.dumpOSC(true)
      run()
    case false => 
      println("Failed to connect to FScape.")
      sys.exit(1)
  }

  def run(): Unit = {
    val fut = Future {
      val inA = inputs.find(_.name.contains("b1269fa6")).get
      // val inB = inputs.find(_.name.contains("65929a65")).get
      val inB = inputs.find(_.name.contains("b1269fa6")).get
//      scala.util.Random.shuffle(inputs.combinations(2)).foreach { case Seq(inA, inB) =>
        run(inA, inB)
//        run(inB, inA)
//      }
    }

    fut.onSuccess  { case _ => println("All done.") }
    fut.onComplete { case _ => sys.exit()           }
  }
  
  def run(inA: File, inB: File): Unit = {
    val idA = inA.base.substring(8)
    val idB = inB.base.substring(8)
    val output = outputDir / s"mentasm-$idA-$idB.aif"
    if (!output.exists()) {
//      val specA   = AudioFile.readSpec(inA)
//      val specB   = AudioFile.readSpec(inB)
//      import numbers.Implicits._
//      val fftSzA  = specA.numFrames.toInt.nextPowerOfTwo
//      val fftSzB  = specB.numFrames.toInt.nextPowerOfTwo
//      val (fftInA, fftInB) = if (fftSzA == fftSzB) (inA, inB) else {
//        ...
//      }

      println(s"Processing $idA - $idB...")
      
      val fftReA    = File.createTemp(suffix = ".aif")
      val fftImA    = File.createTemp(suffix = ".aif")
      val fftReB    = File.createTemp(suffix = ".aif")
      val fftImB    = File.createTemp(suffix = ".aif")
      val fftOutRe  = File.createTemp(suffix = ".aif")
      val fftOutIm  = File.createTemp(suffix = ".aif")

//      println(fftReA)
//      println(fftImA)
//      println(fftReB)
//      println(fftImB)

      val fftFwd1 = FScapeJobs.Fourier(in = inA.path, out = fftReA.path, imagOut = Some(fftImA.path),
        spec = OutputSpec.aiffFloat, gain = Gain.normalized, inverse = false, trunc = false)
      val fftFwd2 = fftFwd1.copy(in = inB.path, out = fftReB.path, imagOut = Some(fftImB.path))
      val fscapeFwdP = Promise[Unit]()
      fscape.processChain("FFT forward", fftFwd1 :: fftFwd2 :: Nil) {
        case true  => fscapeFwdP.success(())
        case false =>
          println(s"FFT forward for $inA / $inB failed")
          fscapeFwdP.failure(new Exception("FScape failed"))
      }
      Await.result(fscapeFwdP.future, Duration.Inf)

      val configRe = Config(input = fftReA, template = fftReB, output = fftOutRe,
        synthesizeWinType = WindowFunction.Rectangle,
        inputWinSize = 4096, templateWinSize = 4096 /* 32768 */, stepSize = 1024 /* 16 */, ampModulation = 0.0675 /* 1.0 */,
        synthesizeWinAmt = 0.0625 )
      val procRe = Morass(configRe)
      println("_" * 33)
      procRe.monitor(printResult = false)
      procRe.start()
      Await.result(procRe, Duration.Inf)

      val configIm = configRe.copy(input = fftReA, template = fftReB, output = fftOutIm)
      val procIm = Morass(configIm)
      println("_" * 33)
      procIm.monitor(printResult = false)
      procIm.start()
      Await.result(procIm, Duration.Inf)

      val fftInv = FScapeJobs.Fourier(in = fftOutRe.path, imagIn = Some(fftOutIm.path), out = output.path,
        spec = OutputSpec.aiffInt, gain = Gain.normalized, inverse = true, trunc = false)
      val fscapeBwdP = Promise[Unit]()
      fscape.process("FFT backward", fftInv) {
        case true  => fscapeBwdP.success(())
        case false =>
          println(s"FFT backward failed")
          fscapeBwdP.failure(new Exception("FScape failed"))
      }
      Await.result(fscapeBwdP.future, Duration.Inf)

      fftReA  .delete()
      fftImA  .delete()
      fftReB  .delete()
      fftImB  .delete()
      fftOutRe.delete()
      fftOutIm.delete()
    }
  }
}