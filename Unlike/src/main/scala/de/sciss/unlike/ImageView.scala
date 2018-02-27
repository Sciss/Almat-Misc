/*
 *  ImageView.scala
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

import java.awt.EventQueue
import java.awt.image.BufferedImage

import scala.swing.{ScrollPane, Dimension, Graphics2D, Component, Swing}

object ImageView {
  def apply(i: Image): ImageView = new Impl(i)

  private final class Impl(img: Image) extends ImageView {
    Swing.onEDT(init())

    private[this] var _zoom  : Double   = 1.0
    private[this] var _mul   : Double   = 1.0
    private[this] var _add   : Double   = 0.0
    private[this] var _invert: Boolean  = false

    def zoom: Double = {
      requireEDT()
      _zoom
    }

    def zoom_=(value: Double): Unit = {
      requireEDT()
      if (_zoom != value) {
        _zoom = value
        invalidateAwt()
        updateSize()
      }
    }

    def mul: Double = {
      requireEDT()
      _mul
    }

    def mul_=(value: Double): Unit = {
      requireEDT()
      if (_mul != value) {
        _mul = value
        updateAndRepaint()
      }
    }

    def add: Double = {
      requireEDT()
      _add
    }

    def add_=(value: Double): Unit = {
      requireEDT()
      if (_add != value) {
        _add = value
        updateAndRepaint()
      }
    }

    def invert: Boolean = {
      requireEDT()
      _invert
    }

    def invert_=(value: Boolean): Unit = {
      requireEDT()
      if (_invert != value) {
        _invert = value
        updateAndRepaint()
      }
    }

    private[this] var awtImage: BufferedImage = _

    private[this] def updateAndRepaint(): Unit = {
      invalidateAwt()
      comp.repaint()
    }

    private[this] var _awtValid = false

    private[this] def invalidateAwt(): Unit = _awtValid = false

    private[this] def updateAwt(): Unit =
      awtImage = img.toAwt(mul = _mul, add = _add, invert = _invert)

    private[this] lazy val comp: Component = new Component {
      opaque = true
      override protected def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        if (!_awtValid) {
          updateAwt()
          _awtValid = true
        }

        val atOrig = g.getTransform
        if (_zoom != 1.0) g.scale(_zoom, _zoom)
        // g.setPaint(pntChecker)
        // g.fillRect(0, 0, math.ceil(peer.getWidth / zoomFactor).toInt, math.ceil(peer.getHeight / zoomFactor).toInt)
        g.drawImage(awtImage, 0, 0, null)
        g.setTransform(atOrig)
      }
    }

    private[this] lazy val scroll = new ScrollPane(comp)

    private[this] def updateSize(): Unit = {
      val wi  = (img.width  * _zoom).toInt
      val hi  = (img.height * _zoom).toInt
      val dim = new Dimension(wi, hi)
      comp  .preferredSize = dim
      comp.peer.setSize     (dim)
      scroll.preferredSize = dim
    }

    private[this] def init(): Unit = {
      updateSize()
      comp
    }

    @inline
    private[this] def requireEDT(): Unit = require(EventQueue.isDispatchThread, "Must be executed on the EDT")

    def component: Component = {
      requireEDT()
      scroll
    }
  }
}
trait ImageView {
  var zoom: Double
  var mul: Double
  var add: Double
  var invert: Boolean
  def component: Component
}