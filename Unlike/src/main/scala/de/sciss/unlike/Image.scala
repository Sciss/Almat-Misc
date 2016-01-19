/*
 *  Image.scala
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

import java.awt.Color
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.numbers

/** A simple image representation for gray scale, using a one dimensional array.
  * Data is sorted with `pixel(x, y) == data[y * width + x]`
  */
final class Image(val data: Array[Double], val width: Int, val height: Int) {
  def pixel(x: Int, y: Int): Double = data(y * width + x)

  def pixelCheck(x: Int, y: Int): Double = {
    val idx = y * width + x
    if (idx < 0 || idx >= data.length) {
      println(s"For an image of ($width, $height) and data.length ${data.length}, x = $x, y = $y")
    }
    data(idx)
  }

  def quarter(): Image = {
    val wh  = width  >> 1
    val hh  = height >> 1
    val arr = new Array[Double](wh * hh)
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

  def sameSize(that: Image): Boolean = this.width == that.width && this.height == that.height

  def toAwt(mul: Double, add: Double, invert: Boolean = false): BufferedImage = {
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g   = img.createGraphics()

    import numbers.Implicits._

    var yi = 0
    while (yi < height) {
      var xi = 0
      while (xi < width) {
        val lambda0 = (pixel(xi, yi) * mul + add).clip(0.0, 1.0) // data(yi)(xi)
        val lambda  = if (invert) 1.0 - lambda0 else lambda0
        val amt     = (lambda * 255 + 0.5).toInt
        g.setColor(new Color(amt, amt, amt))
        g.fillRect(xi, yi, 1, 1)
        xi += 1
      }
      yi += 1
    }
    g.dispose()
    img
  }
}

object Image {
  /** Reads an image file and returns a gray scale version. */
  def read(fIn: File): Image = {
    val imgIn     = ImageIO.read(fIn)
    val imgCrop   = imgIn // cropImage2(config, imgIn)
    val w         = imgCrop.getWidth
    val h         = imgCrop.getHeight

    val arr       = new Array[Double](w * h)

    var y = 0
    var t = 0
    while (y < h) {
      var x = 0
      while (x < w) {
        val rgbIn = imgCrop.getRGB(x, y)
        val vIn = (((rgbIn & 0xFF0000) >> 16) + ((rgbIn & 0x00FF00) >> 8) + (rgbIn & 0x0000FF)) / 765.0
        arr(t) = vIn
        x += 1
        t += 1
      }
      y += 1
    }

    new Image(arr, width = w, height = h)
  }
}

