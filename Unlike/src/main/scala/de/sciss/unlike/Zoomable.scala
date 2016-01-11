/*
 * Zoomable.scala
 * (Unlike)
 *
 * Copyright (c) 2015 Hanns Holger Rutz. All rights reserved.
 *
 * This software and music is published under the
 * Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License
 * (CC BY-NC-ND 4.0)
 *
 * For further information, please contact Hanns Holger Rutz at
 * contact@sciss.de
 */

package de.sciss.unlike

import java.awt.{Dimension, Point, Rectangle}

trait Zoomable {
  protected     var _zoom       = 1.0f
  private[this] var clipLeftAmt = 0f
  protected     var clipLeftPx  = 0
  private[this] var clipTopAmt  = 0f
  protected     var clipTopPx   = 0
  protected     val virtualRect = new Rectangle(0, 0, 400, 400)
  private[this] var children    = List.empty[Zoomable]

  final def zoom: Float = _zoom

  final def zoom_=(value: Float): Unit =
    if (_zoom != value) {
      _zoom = value
      updateScreenSize()
      children.foreach(_.zoom = value)
    }

  // stupid fix for >16384 px problem
  final def clipLeft(amount: Float): Unit = {
    if (clipLeftAmt != amount) {
      clipLeftAmt = amount
      clipLeftPx = (virtualRect.width * clipLeftAmt).toInt
      updateScreenSize()
    }
    children.foreach(_.clipLeft(amount))
  }

  // stupid fix for >16384 px problem
  final def clipTop(amount: Float): Unit = {
    if (clipTopAmt != amount) {
      clipTopAmt = amount
      clipTopPx = (virtualRect.height * clipTopAmt).toInt
      updateScreenSize()
    }
    children.foreach(_.clipTop(amount))
  }

  final def addChild(z: Zoomable): Unit =
    children ::= z

  final def removeChild(z: Zoomable): Unit =
    children = children.diff(List(z))

  private[this] def updateScreenSize(): Unit = {
    val scrW  = (virtualRect.width  * _zoom * (1.0f - clipLeftAmt)).toInt
    val scrH  = (virtualRect.height * _zoom * (1.0f - clipTopAmt )).toInt
    val d     = new Dimension(scrW, scrH)
    screenSizeUpdated(d)
  }

  final def setVirtualBounds(x: Int, y: Int, w: Int, h: Int): Unit = {
    if ((virtualRect.x == x) && (virtualRect.y == y) &&
      (virtualRect.width == w) && (virtualRect.height == h)) return

    virtualRect.setBounds(x, y, w, h)
    clipLeftPx = (virtualRect.width * clipLeftAmt).toInt
    clipTopPx = (virtualRect.width * clipTopAmt).toInt
    updateScreenSize()

    children.foreach(_.setVirtualBounds(x, y, w, h))
  }

  def screenSizeUpdated(d: Dimension): Unit

  final def screenToVirtual(scrPt: Point): Point =
    new Point((scrPt.x / _zoom).toInt + virtualRect.x + clipLeftPx,
      (scrPt.y / _zoom).toInt + virtualRect.y + clipTopPx)

}
