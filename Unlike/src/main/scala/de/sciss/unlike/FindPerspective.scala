package de.sciss.unlike

import javax.imageio.ImageIO

import de.sciss.file.File
import de.sciss.numbers
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{ProcessorFactory, ProcessorLike}

import scala.annotation.switch
import scala.concurrent.blocking

object FindPerspective extends ProcessorFactory {

  /** @param pathA          path of the 'original' image
    * @param pathB          path of the image to find the transform for
    * @param maxDistance    maximum 'radius' for corners to move in perspective distortion
    * @param initCoarse     maximum coarseness (step-size)
    * @param rounds         number of iterations to refine results
    */
  case class Config(pathA: File, pathB: File, maxDistance: Int = 100, initCoarse: Int = 48,
                    rounds: Int = 2, decimStep: Int = 1, log: Boolean = false)

  case class Product(topLeft: IntPoint2D, topRight: IntPoint2D, bottomRight: IntPoint2D, bottomLeft: IntPoint2D,
                     error: Double)

  type Repr = FindPerspective

  protected def prepare(config: Config): Prepared = new Impl(config)

  private final class Image(val data: Array[Float], val width: Int, val height: Int) {
    def pixel(x: Int, y: Int): Float = data(y * width + x)

    def pixelCheck(x: Int, y: Int): Float = {
      val idx = y * width + x
      if (idx < 0 || idx >= data.length) {
        println(s"For an image of ($width, $height) and data.length ${data.length}, x = $x, y = $y")
      }
      data(idx)
    }

    def quarter(): Image = {
      val wh  = width  >> 1
      val hh  = height >> 1
      val arr = new Array[Float](wh * hh)
      var i = 0
      var y = 0
      while (y < hh) {
        var x = 0
        while (x < wh) {
          val nw = pixel( x << 1     ,  y << 1     )
          val ne = pixel((x << 1) + 1,  y << 1     )
          val se = pixel((x << 1) + 1, (y << 1) + 1)
          val sw = pixel( x << 1     , (y << 1) + 1)
          val m  = (nw + ne + se + sw) / 4
          arr(i) = m
          x += 1
          i += 1
        }
        y += 1
      }
      new Image(arr, width = wh, height = hh)
    }
  }

  private object Image {
    def read(fIn: File): Image = {
      val imgIn     = ImageIO.read(fIn)
      val imgCrop   = imgIn // cropImage2(config, imgIn)
      val w         = imgCrop.getWidth
      val h         = imgCrop.getHeight

      val arr       = new Array[Float](w * h)

      var y = 0
      var t = 0
      while (y < h) {
        var x = 0
        while (x < w) {
          val rgbIn = imgCrop.getRGB(x, y)
          val vIn = (((rgbIn & 0xFF0000) >> 16) + ((rgbIn & 0x00FF00) >> 8) + (rgbIn & 0x0000FF)) / 765f // it's gray anyway
          arr(t) = vIn
          x += 1
          t += 1
        }
        y += 1
      }

      new Image(arr, width = w, height = h)
    }
  }

  private final class Impl(val config: Config) extends ProcessorImpl[Product, Repr] with Repr {
    protected def body(): Product = {
      import config._

      def decimFor(coarse: Int): Int = {
        require(coarse >= 1)
        var i = 0
        while (1 << i <= coarse) i += 1
        i - 1
      }

      val maxDecim = decimFor(initCoarse)

      def mkDecim(path: File): Array[Image] = blocking {
        val res = new Array[Image](maxDecim + 1)
        res(0) = Image.read(path)
        for (i <- 1 to maxDecim) res(i) = res(0).quarter()
        res
      }

      val decimA = mkDecim(pathA)
      val decimB = mkDecim(pathB)

      val numCoarse = {
        var coarse = initCoarse
        var i = 0
        do {
          i += 1
          for (_ <- 1 to decimStep) coarse = (coarse + 1) >> 1
        } while (coarse > 1)
        i
      }

      val ZeroPoint   = IntPoint2D(0, 0)
      val bestOffsets = Array.fill[IntPoint2D](4)(ZeroPoint)
      val bestErrors  = Array.fill[Array[Double]](numCoarse)(Array.fill(4)(Double.PositiveInfinity))

      val progNum = rounds * numCoarse * 4
      var progOff = 0

      var coarse    = initCoarse
      var coarseIdx = numCoarse - 1
      var distance  = maxDistance

      do {
        if (log) println(s"---- coarse = $coarse")
        val decimIdx = decimFor(coarse)
        // val decimIdx = 0
        val decim    = 1 << decimIdx
        val imgA     = decimA(decimIdx)
        val imgB     = decimB(decimIdx)
        if (log) println(s"   ${-distance} to $distance by $coarse")
        val corners = bestOffsets.clone()

        val distance2 = distance + distance
        
        for (r <- 0 until rounds) {
          // if (log) println(s"\n==== ROUND ${r + 1} ====\n")

          for (cornerIdx <- 0 until 4) {
            val off0  = /* if (coarse == initCoarse) ZeroPoint else */ corners(cornerIdx)
            import numbers.Implicits._
            val rxMin  = (off0.x - distance).clip(-maxDistance, maxDistance - distance2)
            val rxMax  = rxMin + distance2
            val xRange = rxMin to rxMax by coarse
            val ryMin  = (off0.y - distance).clip(-maxDistance, maxDistance - distance2)
            val ryMay  = ryMin + distance2
            val yRange = ryMin to ryMay by coarse
            for {
              dx <- xRange
              dy <- yRange
            } {
              // assert(dx >= -maxDistance && dx <= maxDistance, s"off0.x = ${off0.x}, rxMin = $rxMin, rxMax = $rxMax")
              corners(cornerIdx) = /* off0 + */ IntPoint2D(dx, dy)
              val err = evaluate(imgA, imgB, decim = decim, corners = corners, cornerIdx = cornerIdx)
              if (err < bestErrors(coarseIdx)(cornerIdx)) {
                if (log) println(s"! best = $err")
                bestOffsets(cornerIdx) = corners(cornerIdx)
                bestErrors(coarseIdx)(cornerIdx) = err
              }
            }

            checkAborted()
            progOff += 1
            progress = progOff.toDouble / progNum
          }
        }

        for (_ <- 1 to decimStep) {
          coarse   = (coarse   + 1) >> 1
          distance = (distance + 1) >> 1
        }
        coarseIdx -= 1

        if (log) {
          println(s"\n TL = ${bestOffsets(0)}\n TR = ${bestOffsets(1)}\n BR = ${bestOffsets(2)}\n BL = ${bestOffsets(3)}")
          println(s" err = ${bestErrors(coarseIdx)}")
        }

      } while (coarse > 1)

      Product(topLeft     = bestOffsets(0), topRight   = bestOffsets(1),
              bottomRight = bestOffsets(2), bottomLeft = bestOffsets(3),
              error       = bestErrors(0).sum)
    }
  }

  private def evaluate(imgA: Image, imgB: Image, decim: Int, corners: Array[IntPoint2D], cornerIdx: Int): Double = {
    val imgW = imgB.width
    val imgH = imgB.height
    // require (imgA.width  == imgW)
    // require (imgA.height == imgH)

    // cf. JHLabs PerspectiveFilter

    val decimR = 1.0f / decim

    val x0 = corners(0).x * decimR
    val y0 = corners(0).y * decimR
    val x1 = corners(1).x * decimR + imgW
    val y1 = corners(1).y * decimR
    val x2 = corners(2).x * decimR + imgW
    val y2 = corners(2).y * decimR + imgH
    val x3 = corners(3).x * decimR
    val y3 = corners(3).y * decimR + imgH

    val dx1 = x1 - x2
    val dy1 = y1 - y2
    val dx2 = x3 - x2
    val dy2 = y3 - y2
    val dx3 = x0 - x1 + x2 - x3
    val dy3 = y0 - y1 + y2 - y3

    var a11 = 0f
    var a12 = 0f
    var a13 = 0f
    var a21 = 0f
    var a22 = 0f
    var a23 = 0f
    var a31 = 0f
    var a32 = 0f

    if (dx3 == 0 && dy3 == 0) {
      a11 = x1 - x0
      a21 = x2 - x1
      a31 = x0
      a12 = y1 - y0
      a22 = y2 - y1
      a32 = y0
      a13 = 0f
      a23 = 0f
    } else {
      a13 = (dx3 * dy2 - dx2 * dy3) / (dx1 * dy2 - dy1 * dx2)
      a23 = (dx1 * dy3 - dy1 * dx3) / (dx1 * dy2 - dy1 * dx2)
      a11 = x1 - x0 + a13 * x1
      a21 = x3 - x0 + a23 * x3
      a31 = x0
      a12 = y1 - y0 + a13 * y1
      a22 = y3 - y0 + a23 * y3
      a32 = y0
    }

    val A = a22 - a32 * a23
    val B = a31 * a23 - a21
    val C = a21 * a32 - a31 * a22
    val D = a32 * a13 - a12
    val E = a11 - a31 * a13
    val F = a31 * a12 - a11 * a32
    val G = a12 * a23 - a22 * a13
    val H = a21 * a13 - a11 * a23
    val I = a11 * a22 - a21 * a12

    @inline
    def transformInverse(x: Float, y: Float, out: Array[Float]): Unit = {
      out(0) = imgW * (A * x + B * y + C) / (G * x + H * y + I)
      out(1) = imgH * (D * x + E * y + F) / (G * x + H * y + I)
    }

    @inline
    def bilinearInterpolate(xw: Float, yw: Float, nw: Float, ne: Float, sw: Float, se: Float): Float = {
      val xw1 = 1f - xw
      val yw1 = 1f - yw
      val top = xw1 * nw + xw * ne
      val bot = xw1 * sw + xw * se
      top * yw1 + bot * yw
    }

    val imgW1 = imgW - 1
    val imgH1 = imgH - 1

    def calcPixel(img: Image, x: Float, y: Float): Float = {
      val srcX0   = math.floor(x).toInt
      val srcY0   = math.floor(y).toInt
      val xWeight = x - srcX0
      val yWeight = y - srcY0

      // clamp
      val srcX = if (srcX0 >= 0 && srcX0 < imgW1) srcX0 else math.max(0, math.min(imgW1 - 1, srcX0))
      val srcY = if (srcY0 >= 0 && srcY0 < imgH1) srcY0 else math.max(0, math.min(imgH1 - 1, srcY0))

      val nw = img.pixel(srcX, srcY)
      val ne = img.pixel(srcX + 1, srcY)
      val sw = img.pixel(srcX, srcY + 1)
      val se = img.pixel(srcX + 1, srcY + 1)

      bilinearInterpolate(xWeight, yWeight, nw, ne, sw, se)
    }

    //    val transX  =      math.min(math.min(x0, x1), math.min(x2, x3)).toInt
    //    val transY  =      math.min(math.min(y0, y1), math.min(y2, y3)).toInt
    //    val originX = x0 - transX
    //    val originY = y0 - transY
    //
    //    transX + originX == x0
    //    transY + originY == y0

    val out = new Array[Float](2)
    var iy  = 0
    var err = 0.0
    while (iy < imgH) {
      var ix = 0
      while (ix < imgW) {
        transformInverse(/* transX + */ ix /* + x0 */, /* transY + */ iy /* + y0 */, out)
        val pixelA  = calcPixel(imgA, out(0), out(1))
        val pixelB  = calcPixel(imgB, ix    , iy    )
        val d       = pixelA - pixelB

        val xw      = ix.toDouble / imgW1
        val yw      = iy.toDouble / imgH1

        val w = (cornerIdx: @switch) match {
          case 0 => (1.0 - xw) * (1.0 - yw)
          case 1 => xw * (1.0 - yw)
          case 2 => xw * yw
          case 3 => (1.0 - xw) * yw
        }

        err += d * d * w
        ix += 1
      }
      iy += 1
    }

    err / (imgH * imgW)
  }
}

/** A processor that estimates the perspective transform needed to equalize
  * an image `b` so that it maximally matches an image `a`. The transform
  * is returned as offsets of the four corners of the perspective frame.
  */
trait FindPerspective extends ProcessorLike[FindPerspective.Product, FindPerspective] {
  def config: FindPerspective.Config
}