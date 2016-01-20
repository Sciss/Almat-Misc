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

object RCThumbnail extends App {
  import MoorMotionStudy1b.{base, outputTemplate, startFrame, endFrame}

  val fOut = base / "imperfect_thumb.jpg"
  if (fOut.exists()) {
    println(s"$fOut already exists.")
    sys.exit(1)
  }

  val frames    = startFrame to endFrame
  val columns   = 7
  val rows      = 11
  val grid      = columns * rows
  val step      = frames.size.toDouble / grid
  val selected  = Vector.tabulate(grid)(i => frames((i * step).toInt))
  val wIn       = 1920
  val hIn       = 1080

  val pad       = 2
  val scale     = 0.15
  val width0    = (wIn * scale + 0.5).toInt + pad
  val width     = width0 * columns + pad
  val height0   = (hIn * scale + 0.5).toInt + pad
  val height    = height0 * rows  + pad

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
