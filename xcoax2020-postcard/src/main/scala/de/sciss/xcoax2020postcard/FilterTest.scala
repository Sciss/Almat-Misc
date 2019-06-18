/*
 *  ConvTest.scala
 *  (xCoAx2020-Postcard)
 *
 *  Copyright (c) 2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.xcoax2020postcard

import de.sciss.file._
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{GE, Graph, graph}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object FilterTest {
  case class Config(
                   fIn: File, fOut: File, f1N: Double
                   )

  def any2stringadd: Any = ()

  def main(args: Array[String]): Unit = {
    for (idx <- 1 to 1) {
      val tempIn  = file("/data/projects/Almat/events/xcoax2020/postcard/centered/outc%04d.png")
      val tempOut = file("/data/projects/Almat/events/xcoax2020/postcard/composed/outcc%04d.png")
      val fIn     = formatTemplate(tempIn , idx)
      val fOut    = formatTemplate(tempOut, idx)
      require (fIn.exists() && !fOut.exists())

      println(s":::::::: compose $idx ::::::::")
      val fut = run(Config(fIn = fIn, fOut = fOut, f1N = 0.02))
      Await.result(fut, Duration.Inf)
    }
  }

  def formatTemplate(temp: File, idx: Int): File = {
    temp.parent / temp.name.format(idx)
  }

  def run(c: Config): Future[Unit] = {
    import c._

  //  var gui: SimpleGUI = _
    val cfg       = Control.Config()
    cfg.useAsync  = false
  //  cfg.progressReporter = p => Swing.onEDT(gui.progress = p.total)
    cfg.blockSize = 2048 // 512 // 2048

    val g = Graph {
      import graph._

      val width   = 3744  // 3732 // 3488 // 2048 // 3496 // 512
      val height  = 2720  // 2464 // 2048 // 2480 // 512
      val frameSize = width * height
      val i1: GE  = -ImageFileIn(fIn, numChannels = 1).take(frameSize) + (1.0: GE)
      val i2: GE  = {
  //      val seq = (1 to 4 /* 8 */).map { idx =>
  //        ImageFileIn(formatTemplate(tempIn, idx), numChannels = 1): GE
  //      }
        val mix = i1 // -seq.reduce(_ + _).take(frameSize) + (1.0: GE)

        val fftSize = (math.max(width, height): GE).nextPowerOfTwo
        val mixE = AffineTransform2D.translate(mix, widthIn = width, heightIn = height, widthOut = fftSize, heightOut = fftSize,
          tx = 0, ty = 0, zeroCrossings = 0)

//        val f1N = 0.001
        def sinc10 = GenWindow(fftSize /* width */, shape = GenWindow.Sinc, param = 0.5) * 0.5 -
          GenWindow(fftSize /* width */, shape = GenWindow.Sinc, param = f1N) * f1N

        def sinc11: GE = sinc10 * GenWindow(fftSize, shape = GenWindow.Blackman)

        def sinc1: GE = RotateWindow(sinc11, fftSize, fftSize/2)

        val sinc2 = RepeatWindow(sinc1, 1, fftSize /* width */)
        val sinc  = (sinc1 * sinc2).take(frameSize)
        val mixF  = Real2FFT(mixE , rows = fftSize, columns = fftSize)
        val sincF = Real2FFT(sinc, rows = fftSize, columns = fftSize)
        val cv    = mixF.complex * sincF
        val ret   = Real2IFFT(cv, rows = fftSize, columns = fftSize)
        val retS  = AffineTransform2D.translate(ret, widthIn = fftSize, heightIn = fftSize, widthOut = width, heightOut = height,
          tx = 0, ty = 0, zeroCrossings = 0)

        retS // mix
      }

      val flt = i2


      val i3        = flt
      //    val frameTr1  = Metro(frameSize)
      //    val frameTr2  = Metro(frameSize)
      val max      =  5.0e-9: GE // RunningMax(i3 /*, trig = frameTr1*/).last // .drop(frameSize - 1)
      val min      = -5.0e-9: GE // RunningMin(i3 /*, trig = frameTr1*/).last // .drop(frameSize - 1)
      //    val max       = Gate(maxR, gate = frameTr2)
      //    val min       = Gate(minR, gate = frameTr2)
      val mul       = (max - min).reciprocal
      val add       = -min

      min.poll(0, "min")
      max.poll(0, "max")
      mul.poll(0, "mul")
      add.poll(0, "add")

      //    val i3e       = i3.elastic(frameSize / cfg.blockSize + 1)
      val i3e       = BufferMemory(i3, frameSize) // .elastic((frameSize / cfg.blockSize + 1) * 2)
      //    m3.poll(0, "m3-0")
      //    i3.poll(0, "i3-0")
      //
      val noise     = {
        val amp = 0.1
        val n = Seq.fill(3)(WhiteNoise(amp): GE).reduce(_ + _) // / 3
        n.take(frameSize)
      }
      val i4        = (i3e + add) * mul + noise
      i4.poll(0, "i4-0")
      Length(i4).poll(0, "i4-length")

      ////    Progress(Frames(i4) / (2 * frameSize), Metro(width))
      //    ProgressFrames(i4, frameSize)

      val sig     = i4.clip(0.0, 1.0)  // BufferMemory(i4, frameSize) // .clip(0.0, 1.0)
      sig.poll(0, "sig-0")

      val specOut = ImageFile.Spec(width = width, height = height, numChannels = 1,
        sampleFormat = ImageFile.SampleFormat.Int16)
      ImageFileOut(file = fOut, spec = specOut, in = sig)

      //    val specOut = AudioFileSpec(numChannels = 1, sampleRate = 44100)
      //    AudioFileOut(file = fOut, spec = specOut, in = sig)
    }

    val ctl = Control(cfg)
  //  Swing.onEDT {
  //    gui = SimpleGUI(ctl)
  //  }
    ctl.run(g)
    ctl.status
  }
}