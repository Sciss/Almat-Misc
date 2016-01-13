package de.sciss.unlike

import de.sciss.play.json.AutoFormat
import play.api.libs.json.Format

object Situation {
  implicit val format: Format[Situation] = AutoFormat[Situation]
}
//case class Situation(index: Int, translate: IntPoint2D = new IntPoint2D(0, 0), scale: Double = 100.0,
//                     rotate: Int = 0)
case class Situation(index: Int,
                     pTopLeftDx    : Int = 0, pTopLeftDy    : Int = 0,
                     pTopRightDx   : Int = 0, pTopRightDy   : Int = 0,
                     pBottomLeftDx : Int = 0, pBottomLeftDy : Int = 0,
                     pBottomRightDx: Int = 0, pBottomRightDy: Int = 0) {

  def hasTransform: Boolean = pTopLeftDx     != 0 || pTopLeftDy     != 0 ||
                              pTopRightDx    != 0 || pTopRightDy    != 0 ||
                              pBottomRightDx != 0 || pBottomRightDy != 0 ||
                              pBottomRightDx != 0 || pBottomRightDy != 0
}
