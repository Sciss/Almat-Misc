/*
 *  CheckerBackground.scala
 *  (Unlike)
 *
 *  Copyright (c) 2015-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.unlike

import java.awt.image.BufferedImage
import java.awt.{Dimension, Graphics2D, Paint, Rectangle, TexturePaint}
import scala.swing.Component

class CheckerBackground(sizeH: Int = 32)
  extends Component
  with Zoomable {

  private[this] val pntBg: Paint = {
    val img = new BufferedImage(sizeH << 1, sizeH << 1, BufferedImage.TYPE_INT_ARGB)

    for (x <- 0 until img.getWidth) {
      for (y <- 0 until img.getHeight) {
        img.setRGB(x, y, if (((x / sizeH) ^ (y / sizeH)) == 0) 0xFF9F9F9F else 0xFF7F7F7F)
      }
    }

    new TexturePaint(img, new Rectangle(0, 0, img.getWidth, img.getHeight))
  }

  opaque = false

  def screenSizeUpdated(d: Dimension): Unit = {
    preferredSize = d
    peer.setSize(d)
    revalidate()
  }

  override protected def paintComponent(g2: Graphics2D): Unit = {
    val atOrig  = g2.getTransform
    g2.scale(_zoom, _zoom)
    g2.translate(-clipLeftPx, -clipTopPx)
    g2.setPaint(pntBg)
    g2.fillRect(0, 0, virtualRect.width, virtualRect.height)
    g2.setTransform(atOrig)
  }
}
