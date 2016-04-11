/*
 *  RCThumbnail.scala
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

import java.awt.{Color, RenderingHints}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import de.sciss.file._

object RCThumbnail {
  import MoorMotionStudy1b.{base, outputTemplate, startFrame, endFrame}

  def main(args: Array[String]): Unit = run()

  var fOut      = base / "imperfect_thumb.jpg"
  var frames    = startFrame to endFrame
  var columns   = 7
  var rows      = 11
  var pad       = 2
  var scale     = 0.15

  def grid      = columns * rows
  def step      = frames.size.toDouble / grid
  def selected  = Vector.tabulate(grid)(i => frames((i * step).toInt))

  val wIn       = 1920
  val hIn       = 1080

  def width0    = (wIn * scale + 0.5).toInt + pad
  def width     = width0 * columns + pad
  def height0   = (hIn * scale + 0.5).toInt + pad
  def height    = height0 * rows  + pad

  def run(): Unit = {
    if (fOut.exists()) {
      println(s"$fOut already exists.")
      sys.exit(1)
    }
    val img       = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val format    = ImageFormat.JPG()

    val g         = img.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING , RenderingHints.VALUE_ANTIALIAS_ON         )
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
    g.setRenderingHint(RenderingHints.KEY_RENDERING    , RenderingHints.VALUE_RENDER_QUALITY       )
    val atScale = AffineTransform.getScaleInstance(scale, scale)
    val atOrig  = g.getTransform
    g.setColor(Color.darkGray)
    g.fillRect(0, 0, width, height)

    for {
      r <- 0 until rows
      c <- 0 until columns
    } {
      val idx   = r * columns + c
      val frame = selected(idx)
      val fIn   = outputTemplate.parent / outputTemplate.name.format(frame)
      val thumb = ImageIO.read(fIn)
      g.translate(c * width0 + pad, r * height0 + pad)
      g.drawImage(thumb, atScale, null)
      g.setTransform(atOrig)
    }

    format.write(fOut, img)
  }
}