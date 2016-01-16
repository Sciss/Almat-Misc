/*
 *  PhaseCorrelation.scala
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

import java.util

import de.sciss.file._
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{ProcessorFactory, ProcessorLike}
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_2D

import scala.concurrent.blocking

object PhaseCorrelation extends ProcessorFactory {
  type Repr = PhaseCorrelation

  case class Config(pathA: File, pathB: File, downSample: Double = 1.0,
                    rotateMin: Double = 0.0, rotateMax: Double = 0.0, rotateSteps: Int = 0,
                    scaleMin : Double = 1.0, scaleMax : Double = 1.0, scaleSteps : Int = 0)

  object Product {
    def identity: Product = Product(translateX = 0.0, translateY = 0.0, rotate = 0.0, scale = 1.0)
  }
  case class Product(translateX: Double, translateY: Double, rotate: Double, scale: Double) {
    override def toString =
      f"Product(translate = ($translateX%1.1f, $translateY%1.1f), rotate = $rotate%1.1f, scale = $scale%1.3f)"
  }

  protected def prepare(config: Config): Prepared = new Impl(config)

  private final class Impl(val config: Config) extends ProcessorImpl[Product, Repr] with Repr {
    import config._

    private def readImage(path: File): Image = blocking {
      val i = Image.read(path)
      if (downSample > 1.0) resample(i, 1.0/downSample) else i
    }

    protected def body(): Product = {
      val imgA  = readImage(pathA)
      val imgB  = readImage(pathB)

      require(imgA sameSize imgB, "Images must have the same size")
      val w     = imgA.width
      val h     = imgA.height

      // println(s"w = $w, h = $h, size = ${imgA.data.length}")

      val fft   = new DoubleFFT_2D(/* rows = */ h, /* columns = */ w)

      applyWindow(imgA)
      applyWindow(imgB)

      val dataA = blocking(realToComplex(imgA.data))
      val dataB = blocking(realToComplex(imgB.data))

      fft.complexForward(dataA)
      fft.complexForward(dataB)

      // Note: for the magnitudes it doesn't matter whether
      // you multiply with the conjugate matrix or the non-conjugate matrix.
      // Therefore, the formula (3) in Thomas et al. becomes:
      // F1 F2∗ / | F1 F2∗ |

      complexConj(dataB)
      elemMul(dataA, dataB)
      elemNorm(dataA)

      blocking(fft.complexInverse(dataA, true))

      val dataC   = complexToReal(dataA)
      assert(dataC.length == w * h)
      val imgC    = new Image(dataC, width = w, height = h)

      // val pp      = findPeak(imgC)
      val (ppx, ppy) = findPeakCentroid(imgC)
      // val pp      = IntPoint2D(pp0.x << 1, pp0.y << 1)  // XXX TODO --- do we loose factor 2 precision?
      // println(s"Peak at (${pp.x}, ${pp.y})")
      val shiftX  = if (ppx > imgC.width /2) ppx - imgC.width  else ppx
      val shiftY  = if (ppy > imgC.height/2) ppy - imgC.height else ppy

      // println(s"Peak at ($shiftX, $shiftY)")
      Product(translateX = shiftX * downSample, translateY = shiftY * downSample, rotate = 0.0, scale = 1.0)
    }
  }

  //  normalize(dataC)
  //  val view = imgC.plot(zoom = 0.25) // mul = 0.5 / math.Pi, add = 0.5
  //  Swing.onEDT { view.zoom = 1.0 }

  /** Element-wise multiplication, replacing the contents of `a`. */
  def elemMul(a: Array[Double], b: Array[Double]): Unit = {
    val n = a.length
    var i = 0
    while (i < n) {
      val ar = a(i)
      val br = b(i)
      val ai = a(i + 1)
      val bi = b(i + 1)
      val re = ar * br - ai * bi
      val im = ar * bi + ai * br
      a(i)      = re
      a(i + 1)  = im
      i += 2
    }
  }

  /** Element-wise normalization, dividing by magnitude */
  def elemNorm(data: Array[Double]): Unit = {
    val n = data.length
    var i = 0
    while (i < n) {
      val re  = data(i)
      val im  = data(i + 1)
      val mag = math.sqrt(re * re + im * im)
      if (mag > 0) {
        data(i)     /= mag
        data(i + 1) /= mag
      }
      i += 2
    }
  }

  def findPeak(image: Image): IntPoint2D = {
    val data = image.data
    val w = image.width
    val n = data.length
    var i = 0
    var j = 0
    var max = Double.NegativeInfinity
    while (i < n) {
      val d = data(i)
      if (d > max) {
        max = d
        j   = i
      }
      i += 1
    }
    val x = j % w
    val y = j / w
    // println(s"j = $j")
    IntPoint2D(x, y)
  }

  def findPeakCentroid(image: Image): (Double, Double) = {
    val data = image.data
    val w = image.width
    val h = image.height
    val n = data.length
    var i = 0
    var j = 0
    var max = Double.NegativeInfinity
    while (i < n) {
      val d = data(i)
      if (d > max) {
        max = d
        j   = i
      }
      i += 1
    }
    val px = j % w
    val py = j / w

    def calc(nSz: Int): (Double, Double) = {
      var cx = 0.0
      var cy = 0.0
      var cs = 0.0

      val x0  = math.max(0, px - nSz)
      val xs  = math.min(w, px + nSz + 1)
      val y0  = math.max(0, py - nSz)
      val ys  = math.min(h, py + nSz + 1)
      var x   = x0
      while (x < xs) {
        var y = y0
        while (y < ys) {
          val q = image.pixel(x, y)
          cx += q * x
          cy += q * y
          cs += q
          y += 1
        }
        x += 1
      }

      cx /= cs
      cy /= cs

      (cx, cy)
    }

//    var cx = max * px
//    var cy = max * py
//    var cs = max
//
//    @tailrec
//    def move(x: Int, y: Int, p: Double, dx: Int, dy: Int, c: Int = 0): Unit = if (c < 1) {
//      val nx = x + dx
//      val ny = y + dy
//      if (nx >= 0 && nx < w && ny >= y && ny < h) {
//        val q = image.pixel(nx, ny)
//        if (q <= p) {
//          cx += q * nx
//          cy += q * ny
//          cs += q
//          move(x = nx, y = ny, p = q, dx = dx, dy = dy, c = c + 1)
//        }
//      }
//    }
//
//    move(x = px, y = py, p = max, dx = -1, dy =  0)
//    move(x = px, y = py, p = max, dx =  1, dy =  0)
//    move(x = px, y = py, p = max, dx =  0, dy = -1)
//    move(x = px, y = py, p = max, dx =  0, dy =  1)
//
//    move(x = px, y = py, p = max, dx = -1, dy = -1)
//    move(x = px, y = py, p = max, dx =  1, dy = -1)
//    move(x = px, y = py, p = max, dx = -1, dy =  1)
//    move(x = px, y = py, p = max, dx =  1, dy =  1)

    // somehow running this with two different even/odd
    // radiuses and averaging the result seems the most precise...
    val (ex1, ey1) = calc(1)
    val (ex2, ey2) = calc(2)
    ((ex1 + ex2) / 2, (ey1 + ey2) / 2)
  }

  /** Creates a new array double the size with real from input and zero imag. */
  def realToComplex(in: Array[Double]): Array[Double] = {
    val n   = in.length
    val out = new Array[Double](n << 1)
    var i   = 0
    var j   = 0
    while (i < n) {
      val re = in(i)
      out(j) = re
      i += 1
      j += 2
    }
    out
  }

  /** Creates a new array double half the size with real from input. */
  def complexToReal(in: Array[Double]): Array[Double] = {
    val n   = in.length
    require(n % 2 == 0)
    val out = new Array[Double](n >> 1)
    var i   = 0
    var j   = 0
    while (i < n) {
      val re = in(i)
      out(j) = re
      i += 2
      j += 1
    }
    out
  }

  def normalize(data: Array[Double]): Unit = {
    val n = data.length
    var i = 0
    var min = Double.PositiveInfinity
    var max = Double.NegativeInfinity
    while (i < n) {
      val d = data(i)
      if (d < min) min = d
      if (d > max) max = d
      i += 1
    }
    if (min < max) {
      val add = -min
      val mul = 1.0 / (max - min)
      i = 0
      while (i < n) {
        data(i) = (data(i) + add) * mul
        i += 1
      }

    } else if (min == max) {
      util.Arrays.fill(data, 0.0)
    }
  }

  /** Taking the complex conjugate and replacing the input. */
  def complexConj(data: Array[Double]): Unit = {
    val n = data.length
    var i = 1
    while (i < n) {
      data(i) = -data(i)
      i += 2
    }
  }

  def applyWindow(image: Image): Unit = {
    val data  = image.data
    val w     = image.width
    val h     = image.height
    val winH  = WindowFunction.Hanning.create(w)
    val winV  = WindowFunction.Hanning.create(h)
    var y = 0
    while (y < h) {
      val ys = winV(y)
      var x  = 0
      while (x < w) {
        val xs = winH(x)
        data(y * w + x) *= xs * ys
        x += 1
      }
      y += 1
    }
  }

  def resample(image: Image, factor: Double): Image = {
    val r       = Resample(Resample.Quality.High)
    val dataIn  = image.data
    val wi      = image.width
    val hi      = image.height
    val wr      = (wi  * factor + 0.5).toInt
    val hr      = (hi * factor + 0.5).toInt
    val rowIn   = new Array[Double](wi + 1)
    val colIn   = new Array[Double](hi + 1)
    val dataTmp = new Array[Double](wr * wi)
    val colOut  = new Array[Double](wr)
    val dataOut = new Array[Double](wr * hr)

    var y = 0
    while (y < hi) {
      System.arraycopy(dataIn, y * wi, rowIn, 0, wi)
      rowIn(wi) = rowIn(wi - 1) // padding
      r.process(src = rowIn, srcOff = 0, dest = dataTmp, destOff = y * wr, length = wr, factor = factor)
      y += 1
    }
    var x = 0
    while (x < wr) {
      y = 0
      while (y < hi) {
        colIn(y) = dataTmp(y * wr + x)
        y += 1
      }
      r.process(src = colIn, srcOff = 0, dest = colOut, destOff = 0, length = hr, factor = factor)
      y = 0
      while (y < hr) {
        dataOut(y * wr + x) = colOut(y)
        y += 1
      }
      x += 1
    }
    new Image(dataOut, width = wr, height = hr)
  }
}
trait PhaseCorrelation extends ProcessorLike[PhaseCorrelation.Product, PhaseCorrelation] {
  def config: PhaseCorrelation.Config
}