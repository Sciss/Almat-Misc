package de.sciss.unlike

case class Situation(index: Int, translate: IntPoint2D = new IntPoint2D(0, 0), scale: Double = 1.0,
                     config: Config = Config())