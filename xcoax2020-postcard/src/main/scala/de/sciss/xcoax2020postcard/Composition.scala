/*
 *  Composition.scala
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

/*

I0 = filter with f1N, min/max fixed at 5.0e-9
M = centered

I = (I0 - 0.25 / 0.5).pow(0.5).pow(0.5)

levels --> (we find a gaussian); left/right (64, 191), gamma 2.0; another gamma 2.0

grain extract with layer in top
"E = I - M + 128" ; m = upper, i = lower

E = (I - M + 0.5).clip(0.0, 1.0)

invert

In = 1 - E

auto-level

(Q1, Q2) = getAutoLevels

Al = In - Q1 / (Q2 - Q1)

thresh: 84 // 143

  Th = Al >= (0.333)

 */

object Composition {
  case class Config(
                     fIn: File, fOut: File, f1N: Double = 0.02, rangeFix: Double = 5.0e-9, rotateFilter: Boolean = false
                   )

  val part1 = Config(
    fIn   = file("in"),
    fOut  = file("out")
  )

  val part2: Config = part1.copy(
    f1N           = 0.022,
    rangeFix      = 4.0e-9,
    rotateFilter  = true
  )

  def any2stringadd: Any = ()

  def main(args: Array[String]): Unit = {
    for (idx <- 901 to 1000) {
      val tempIn  = file("/data/projects/Almat/events/xcoax2020/postcard/centered/outc%04d.png")
      val tempOut = file("/data/projects/Almat/events/xcoax2020/postcard/composed/outcc%04d.png")
      val fIn     = formatTemplate(tempIn , idx)
      val fOut    = formatTemplate(tempOut, idx)
      require ( fIn .exists(), fIn  .path)
      require (!fOut.exists(), fOut .path)

      println(s":::::::: compose $idx ::::::::")
      val base  = if (idx <= 500) part1 else part2
      val cfg   = base.copy(
        fIn = fIn, fOut = fOut
      )
      val fut = run(cfg)
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

      def mkIn() = ImageFileIn(fIn, numChannels = 1) // .take(frameSize)

      val i1: GE  = -mkIn() + (1.0: GE)
      val i2: GE  = {
        //      val seq = (1 to 4 /* 8 */).map { idx =>
        //        ImageFileIn(formatTemplate(tempIn, idx), numChannels = 1): GE
        //      }
        val mix = i1 // -seq.reduce(_ + _).take(frameSize) + (1.0: GE)

        val fftSize = (math.max(width, height): GE).nextPowerOfTwo
        val mixE = AffineTransform2D.translate(mix, widthIn = width, heightIn = height, widthOut = fftSize, heightOut = fftSize,
          tx = 0, ty = 0, zeroCrossings = 0)

        val fftSizeSq = fftSize * fftSize

        def sinc10 = GenWindow(fftSize /* width */, shape = GenWindow.Sinc, param = 0.5) * 0.5 -
          GenWindow(fftSize /* width */, shape = GenWindow.Sinc, param = f1N) * f1N

        def sinc11: GE = sinc10 * GenWindow(fftSize, shape = GenWindow.Blackman)

        def sinc12: GE = RotateWindow(sinc11, fftSize, fftSize/2)

        val (sinc1, sinc2) = (sinc12, RepeatWindow(sinc12, 1, fftSize /* width */))
        val sincM = sinc1 * sinc2

        val sinc = if (rotateFilter) {
          val d1 = fftSize - width // /2
          val t0 = ResizeWindow(sincM , fftSize, stop = -d1)  // "truncate" horizontally
//          val t0 = ResizeWindow(sincM , fftSize, start = d1, stop = -d1)  // "truncate" horizontally
//          val d2 = d1 + Impulse(0.25) * 2 /* glitch */
          val d2 = d1 + Metro(2) // * 2 /* glitch */
//          val d2 = d1 + LFSaw(0.5) * 1.1 // 1.0/height) * 1.5
          val t1 = ResizeWindow(t0    , fftSize - d2, stop = d1)
//          val t1 = ResizeWindow(t0    , fftSize - (d2 + d1), start = -d1, stop = d1)
          t1.take(fftSizeSq)
        } else {
          sincM.take(frameSize) // "truncate" vertically
        }

        val mixF  = Real2FFT(mixE, rows = fftSize, columns = fftSize)
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
      val max      =  rangeFix: GE // RunningMax(i3 /*, trig = frameTr1*/).last // .drop(frameSize - 1)
      val min      = -rangeFix: GE // RunningMin(i3 /*, trig = frameTr1*/).last // .drop(frameSize - 1)
      //    val max       = Gate(maxR, gate = frameTr2)
      //    val min       = Gate(minR, gate = frameTr2)
      val mul       = (max - min).reciprocal
      val add       = -min

//      min.poll(0, "min")
//      max.poll(0, "max")
//      mul.poll(0, "mul")
//      add.poll(0, "add")

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
      val I0        = (i3e + add) * mul + noise
//      i4.poll(0, "i4-0")
//      Length(i4).poll(0, "i4-length")

      val I = ((I0 - 0.25) / 0.5).max(0.0).pow(0.25)
      val M = mkIn()
      val E = (I - M + (0.5: GE)).clip(0.0, 1.0)
      val In = 1.0 - BufferMemory(E, frameSize)

      val (q1, q2) = AutoLevels.getAutoLevels(E, frameSize)

      q1.poll(0, "q1")
      q2.poll(0, "q2")

      val Al      = In - q1 / (q2 - q1)

      val threshLvl = SlidingPercentile(Al, len = frameSize, frac = 0.12).last
      threshLvl.poll(0, "thresh")

      val thresh  = BufferMemory(Al, frameSize) > threshLvl //  >= 0.6666666 // 0.333333333

      val sig     = thresh // .clip(0.0, 1.0)  // BufferMemory(i4, frameSize) // .clip(0.0, 1.0)
//      sig.poll(0, "sig-0")

      val specOut = ImageFile.Spec(width = width, height = height, numChannels = 1,
        sampleFormat = ImageFile.SampleFormat.Int8)
      ImageFileOut(file = fOut, spec = specOut, in = sig)
    }

    val ctl = Control(cfg)
    ctl.run(g)
    ctl.status
  }
}