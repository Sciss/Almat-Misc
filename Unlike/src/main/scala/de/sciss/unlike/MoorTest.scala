/*
 *  MoorTest.scala
 *  (Unlike)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.unlike

import de.sciss.file._

object MoorTest extends App {
  val base = userHome / "Documents" / "projects" / "Unlike"

  val conf = EstimateVideoMotion.Config(
    input       = base / "moor_8024" / "moor_8024-%05d.jpg",
    outputDir   = Some(base / "moor_8024_json"),
    startFrame  =     1 + 60,
    endFrame    = 11945 - 60
  )
  val p = EstimateVideoMotion(conf)
  runAndMonitor(p, exit = true, printResult = false)
}
