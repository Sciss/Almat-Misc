package de.sciss.unlike

import de.sciss.play.json.AutoFormat
import play.api.libs.json.Format

object Config {
  implicit val format: Format[Config] = AutoFormat[Config]
}
case class Config(noise: Int = 32)
//  case class Config(sizeIn: Int = 430, sizeOut: Int = 1080, noise: Int = 32, thresh: Int = 160,
//                    resampleWindow: Int = 29, dropFrame: Int = 16, dropRate: Double = 6.032)

