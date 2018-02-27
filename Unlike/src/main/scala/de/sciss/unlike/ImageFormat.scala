/*
 *  ImageFormat.scala
 *  (Unlike)
 *
 *  Copyright (c) 2015-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.unlike

import java.awt.image.BufferedImage
import java.io.{FileOutputStream, OutputStream}
import javax.imageio.{IIOImage, ImageWriteParam, ImageIO}

import de.sciss.file.File

object ImageFormat {
  case class JPG(quality: Int = 95) extends ImageFormat {
    val extension = "jpg"

    def write(os: OutputStream, img: BufferedImage): Unit = {
      val ios     = ImageIO.createImageOutputStream(os)
      val iter    = ImageIO.getImageWritersByFormatName("jpeg")
      val writer  = iter.next()
      try {
        val iwp     = writer.getDefaultWriteParam
        iwp.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
        iwp.setCompressionQuality(quality * 0.01f)
        writer.setOutput(ios)
        writer.write(null, new IIOImage(img, null, null), iwp)
      } finally {
        writer.dispose()
      }
    }
  }
  case object PNG extends ImageFormat {
    val extension = "png"

    def write(os: OutputStream, img: BufferedImage): Unit =
      ImageIO.write(img, "png", os)
  }
}
sealed trait ImageFormat {
  /** File extension without leading period. */
  def extension: String

  def write(os: OutputStream, img: BufferedImage): Unit

  def write(f: File, img: BufferedImage): Unit = {
    val fOut = new FileOutputStream(f)
    try {
      write(fOut, img)
    } finally {
      fOut.close()
    }
  }
}