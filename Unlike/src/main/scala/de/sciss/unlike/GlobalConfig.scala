/*
 * GlobalConfig.scala
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

import de.sciss.play.json.AutoFormat
import play.api.libs.json.Format

object GlobalConfig {
  implicit val format: Format[GlobalConfig] = AutoFormat[GlobalConfig]
}
case class GlobalConfig(width: Int = 7360, height: Int = 4912, noise: Int = 0)

//object FrameConfig {
//  implicit val format: Format[FrameConfig] = AutoFormat[FrameConfig]
//}
//case class FrameConfig()

//  case class Config(sizeIn: Int = 430, sizeOut: Int = 1080, noise: Int = 32, thresh: Int = 160,
//                    resampleWindow: Int = 29, dropFrame: Int = 16, dropRate: Double = 6.032)

