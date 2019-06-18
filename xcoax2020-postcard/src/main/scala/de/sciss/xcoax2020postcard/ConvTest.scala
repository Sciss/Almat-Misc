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
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{GE, Graph, graph}

import scala.swing.Swing

object ConvTest extends App {
  def any2stringadd: Any = ()

  var gui: SimpleGUI = _
  val cfg       = Control.Config()
  cfg.useAsync  = false
  cfg.progressReporter = p => Swing.onEDT(gui.progress = p.total)
  cfg.blockSize = 2048 // 512 // 2048

  def formatTemplate(temp: File, idx: Int): File = {
    temp.parent / temp.name.format(idx)
  }

  val g = Graph {
    import graph._
    val tempIn  = file("/data/projects/Almat/events/xcoax2020/postcard/centered/outc%04d.png")
    val baseDir = tempIn.parent.parent
    val fOut    = baseDir / "test-conv.png"
    require (baseDir.canWrite)

    val width   = 3744  // 3732 // 3488 // 2048 // 3496 // 512
    val height  = 2720  // 2464 // 2048 // 2480 // 512
    val frameSize = width * height
    val i1: GE  = -ImageFileIn(formatTemplate(tempIn, 9), numChannels = 1).take(frameSize) + (1.0: GE)
    val i2: GE  = {
      val seq = (1 to 4 /* 8 */).map { idx =>
        ImageFileIn(formatTemplate(tempIn, idx), numChannels = 1): GE
      }
      val mix = -seq.reduce(_ + _).take(frameSize) + (1.0: GE)

      val fftSize = (math.max(width, height): GE).nextPowerOfTwo
      val mixE = AffineTransform2D.translate(mix, widthIn = width, heightIn = height, widthOut = fftSize, heightOut = fftSize,
        tx = 0, ty = 0, zeroCrossings = 0)

      val f1N = 0.1
      def sinc1 = GenWindow(fftSize /* width */, shape = GenWindow.Sinc, param = 0.5) * 0.5 -
                  GenWindow(fftSize /* width */, shape = GenWindow.Sinc, param = f1N) * f1N

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
//    val i1      = ImageFileIn(fIn1, numChannels = 1).take(frameSize)
//    val i2      = ImageFileIn(fIn2, numChannels = 1).take(frameSize)
    val kernel  = 32
    val kernelS = kernel * kernel
    val m1      = MatrixInMatrix(i1, rowsOuter = height, columnsOuter = width, rowsInner = kernel, columnsInner = kernel)
    val m2      = MatrixInMatrix(i2, rowsOuter = height, columnsOuter = width, rowsInner = kernel, columnsInner = kernel)
     Length(i1).poll(0, "i1-len")
    //    val p       = Metro(kernelS)
    //    val avg     = RunningSum(m1, p) / kernelS
    //    val flt     = ResizeWindow(avg, size = kernelS, start = kernelS - 1)
    //    Length(flt).poll(0, "flt")

    val m1f       = Real2FFT(m1, rows = kernel, columns = kernel)
    val m2f       = Real2FFT(m2, rows = kernel, columns = kernel)
    val m3f       = m1f.complex * m2f
    val m3        = Real2IFFT(m3f, rows = kernel, columns = kernel)
    val flt       = ResizeWindow(m3, size = kernelS, start = kernelS - 1)
//    val flt       = ResizeWindow(m3, size = kernelS, start = kernelS/2, stop = -(kernelS/2 - 1))
//    i1.poll(Metro(1000), "bla")

//    Progress(Frames(flt) / (2 * frameSize), Metro(width))
    ProgressFrames(flt, frameSize)
//    Length(flt).poll(0, "flt-len")

    val i3        = flt
//    val frameTr1  = Metro(frameSize)
//    val frameTr2  = Metro(frameSize)
    val max      = RunningMax(i3 /*, trig = frameTr1*/).last // .drop(frameSize - 1)
    val min      = RunningMin(i3 /*, trig = frameTr1*/).last // .drop(frameSize - 1)
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
    val noise     = WhiteNoise(0.1).take(frameSize)
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
  Swing.onEDT {
    gui = SimpleGUI(ctl)
  }
  ctl.run(g)
}