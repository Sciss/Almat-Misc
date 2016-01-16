/*
 *  CoarseRegistration.scala
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
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_2D

object CoarseRegistration extends App {
  val pathA = file("_creation") / "test_image1_move30_-30.jpg"
  val pathB = file("_creation") / "test_image1.jpg"

  val imgA  = Image.read(pathA)
  val imgB  = Image.read(pathB)

  require(imgA sameSize imgB)
  val w     = imgA.width
  val h     = imgA.height

  println(s"w = $w, h = $h, size = ${imgA.data.length}")

  val fft   = new DoubleFFT_2D(/* rows = */ w, /* columns = */ h)

  val dataA = realToComplex(imgA.data)
  val dataB = realToComplex(imgB.data)

  fft.complexForward(dataA)
  fft.complexForward(dataB)

  // Note: for the magnitudes it doesn't matter whether
  // you multiply with the conjugate matrix or the non-conjugate matrix.
  // Therefore, the formula (3) in Thomas et al. becomes:
  // F1 F2∗ / | F1 F2∗ |

  complexConj(dataB)
  elemMul(dataA, dataB)
  elemNorm(dataA)

  fft.complexInverse(dataA, true)

  val dataC   = complexToReal(dataA)
  assert(dataC.length == w * h)
  val imgC    = new Image(dataC, width = w, height = h)

  imgC.plot() // mul = 0.5 / math.Pi, add = 0.5)

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

  /** Taking the complex conjugate and replacing the input. */
  def complexConj(data: Array[Double]): Unit = {
    val n = data.length
    var i = 1
    while (i < n) {
      data(i) = -data(i)
      i += 2
    }
  }
}
