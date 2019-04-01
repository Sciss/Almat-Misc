/*
 *  Main.scala
 *  (SpaceMaterialDesign)
 *
 *  Copyright (c) 2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.spacematerialdesign

import de.sciss.file._
import de.sciss.fscape.modules.MakeWorkspace

object Main {
  def main(args: Array[String]): Unit = {
    val target  = userHome / "mellite" / "sessions" / "SpaceMaterialDesign.mllt"
    val modules = List(ModPhaseDiff, ModImageSynth).sortBy(_.name)
    val c = MakeWorkspace.Config(modules, target = target)
    MakeWorkspace.run(c)
  }
}
