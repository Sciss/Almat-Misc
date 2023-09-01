/*
 *  IntPoint2D.scala
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

import play.api.libs.json.{Format, Json}

trait IntPoint2DLike {
  def x: Int
  def y: Int

  final def left: Int = x
  final def top : Int = y

  def distanceSq(that: IntPoint2DLike): Long = {
    val dx = that.x.toLong - x.toLong
    val dy = that.y.toLong - y.toLong
    dx * dx + dy * dy
  }

  /**
    * Queries the overlap of this shape with a given
    * `IntPoint2D p`. The point is considered to have
    * a side length of 1!
    *
    * @return  `true` if this shape contains or partly overlaps
    *          the given point
    */
  final def contains(p: IntPoint2DLike): Boolean = p.x == this.x && p.y == this.y

  /**
    * Returns the orientation of the given point wrt this point, according
    * to the following scheme:
    *
    * 5   4    6
    *   +---+
    * 1 | 0 |  2
    *   +---+
    * 9   8   10
    *
    * Therefore the horizontal orientation can be extracted
    * with `_ & 3`, and the vertical orientation with `_ >> 2`,
    * where orientation is 0 for 'parallel', 1 for 'before' and
    * '3' for 'after', so that if the orient is before or
    * after, the sign can be retrieved via `_ - 2`
    *
    * For example, if this is `IntPoint2D(4, 4)` and the query
    * point is `IntPoint2D(4, 5)`, the result is `12`. If the
    * query is `IntPoint2D(0, 0)`, the result is `5`, etc.
    */
  final def orient(b: IntPoint2DLike): Int = {
    val ax = x
    val ay = y
    val bx = b.x
    val by = b.y
    val dx = if (bx < ax) 1 else if (bx > ax) 2 else 0
    val dy = if (by < ay) 4 else if (by > ay) 8 else 0
    dx | dy
  }
}

object IntPoint2D {
  implicit val format: Format[IntPoint2D] = Json.format[IntPoint2D] // AutoFormat[IntPoint2D]
}
final case class IntPoint2D(x: Int, y: Int) extends IntPoint2DLike {
  def +(p: IntPoint2D) = IntPoint2D(x + p.x, y + p.y)
  def -(p: IntPoint2D) = IntPoint2D(x - p.x, y - p.y)
}