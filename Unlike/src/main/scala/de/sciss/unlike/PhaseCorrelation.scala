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
import de.sciss.play.json.AutoFormat
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{ProcessorFactory, ProcessorLike}
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_2D
import play.api.libs.json.Format

import scala.concurrent.blocking

object PhaseCorrelation extends ProcessorFactory {
  type Repr = PhaseCorrelation

  case class Config(pathA: File, pathB: File, settings: Settings = Settings())

  case class Settings(downSample: Double = 1.0,
    rotateMin: Double = 0.0, rotateMax: Double = 0.0, rotateSteps: Int = 0,
    scaleMin : Double = 1.0, scaleMax : Double = 1.0, scaleSteps : Int = 0)

  object Product {
    def identity: Product = Product(peak = 1.0)

    implicit val format: Format[Product] = AutoFormat[Product]
  }
  case class Product(translateX: Double = 0.0, translateY: Double = 0.0, rotate: Double = 0.0, scale: Double = 1.0,
                     peak: Double) {
    override def toString =
      f"Product(translate = ($translateX%1.1f, $translateY%1.1f), rotate = $rotate%1.1f, scale = $scale%1.3f)"
  }

  protected def prepare(config: Config): Prepared = new Impl(config)

  def prepareImage(path: File, settings: Settings): Image = blocking {
    import settings.downSample
    val image0 = Image.read(path)
    val image  = if (downSample > 1.0) resample(image0, 1.0/downSample) else image0
    applyWindow(image)
    image
  }

  /** @param image  as returned by `prepareImage`. */
  def prepareFFT(image: Image): DoubleFFT_2D =
    new DoubleFFT_2D(/* rows = */ image.height, /* columns = */ image.width)

  /** @param image  as returned by `prepareImage`
    * @param fft    as returned by `prepareFFT`
    * @return data to go into `process`
    */
  def prepareData(image: Image, fft: DoubleFFT_2D): Image = {
    val data = realToComplex(image.data)
    blocking(fft.complexForward(data))
    new Image(data, width = image.width << 1, height = image.height)
  }

  /** @param imgTA  as returned by `prepareData`
    * @param imgTB  as returned by `prepareData`
    * @param fft    as returned by `prepareFFT`
    */
  def process(imgTA: Image, imgTB: Image, fft: DoubleFFT_2D, settings: Settings): Product = {
    import settings.downSample

    val dataTmp = imgTB.data.clone()
    complexConj(dataTmp)
    elemMul(dataTmp, imgTA.data)
    elemNorm(dataTmp)

    blocking(fft.complexInverse(dataTmp, true))

    complexToRealInPlace(dataTmp)
    val w = imgTA.width >> 1
    val h = imgTA.height
    // assert(dataC.length == w * h)
    val imgC = new Image(dataTmp, width = w, height = h)

    val peak = findPeakCentroid(imgC)
    if (downSample == 1.0) peak else
      peak.copy(translateX = peak.translateX * downSample, translateY = peak.translateY * downSample,
        rotate = 0.0, scale = 1.0)
  }

  private final class Impl(val config: Config) extends ProcessorImpl[Product, Repr] with Repr {
    import config._

    protected def body(): Product = {
      val imgA  = prepareImage(pathA, settings)
      val imgB  = prepareImage(pathB, settings)

      require(imgA sameSize imgB, "Images must have the same size")

      val fft   = prepareFFT(imgA)
      val dataA = prepareData(imgA, fft)
      val dataB = prepareData(imgB, fft)

      // Note: for the magnitudes it doesn't matter whether
      // you multiply with the conjugate matrix or the non-conjugate matrix.
      // Therefore, the formula (3) in Thomas et al. becomes:
      // F1 F2∗ / | F1 F2∗ |

      process(imgTA = dataA, imgTB = dataB, fft = fft, settings = settings)
    }
  }

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

  def findPeakCentroid(image: Image): Product = {
    val data = image.data
    val w = image.width
    val h = image.height
    val n = w * h // NOT: data.length - as we re-use a larger array
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

    def calc(nSz: Int): Product = {
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

      Product(translateX = cx, translateY = cy, peak = cs)
    }

    // somehow running this with two different even/odd
    // radiuses and averaging the result seems the most precise...
    val e1 = calc(1)
    val e2 = calc(2)

    val ppx     = (e1.translateX + e2.translateX)/2
    val ppy     = (e1.translateY + e2.translateY)/2
    val shiftX  = if (ppx > w/2) ppx - w else ppx
    val shiftY  = if (ppy > h/2) ppy - h else ppy

    Product(translateX = shiftX,
            translateY = shiftY,
            peak       = (e1.peak + e2.peak)/2)
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

  /** Replaces complex with real in place. */
  def complexToRealInPlace(data: Array[Double]): Unit = {
    val n   = data.length
    require(n % 2 == 0)
    var i   = 0
    var j   = 0
    while (i < n) {
      val re = data(i)
      data(j) = re
      i += 2
      j += 1
    }
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