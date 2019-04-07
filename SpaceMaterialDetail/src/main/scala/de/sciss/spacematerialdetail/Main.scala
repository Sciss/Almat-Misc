/*
 *  Main.scala
 *  (SpaceMaterialDetail)
 *
 *  Copyright (c) 2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.spacematerialdetail

import de.sciss.file._
import de.sciss.fscape.modules.MakeWorkspace.Config
import de.sciss.fscape.modules.{MakeWorkspace, Module}

object Main {
//  final case class Config(modules: List[Module] = list, target: File = file("SpaceMaterialDetail.mllt"))

  def main(args: Array[String]): Unit = {
    val default = Config(modules = list)
    val p = new scopt.OptionParser[Config]("SpaceMaterialDetail") {
      arg[File]("target")
        .required()
        .text ("Target .mllt Mellite workspace.")
        .action { (f, c) => c.copy(target = f) }
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      run(config)
    }
  }

  def run(config: Config): Unit = {
    MakeWorkspace.run(config)
  }

  val list: List[Module] =
    List(
      ModPhaseDiff,
      ModImageSynth,
      ModLogMap,
      ModLogMapRGB,
      ModDirectImageToSound,
      ModDirectSoundToImage
    ) .sortBy(_.name)
}
