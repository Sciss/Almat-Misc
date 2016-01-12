package de.sciss.unlike

import de.sciss.play.json.AutoFormat
import play.api.libs.json.Format

object Situation {
  implicit val format: Format[Situation] = AutoFormat[Situation]
}
case class Situation(index: Int, translate: IntPoint2D = new IntPoint2D(0, 0), scale: Double = 100.0,
                     rotate: Double = 0.0)