/*
 *  PerspectiveTest.scala
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
import de.sciss.processor._

object PerspectiveTest extends App {
  // import Unlike.mkFIn
  // val config  = FindPerspective.Config(pathA = mkFIn(9227), pathB = mkFIn(9228), rounds = 2)
  val config  = FindPerspective.Config(pathA = file("_creation") / "test_image1_move30_-30.jpg",
                                       pathB = file("_creation") / "test_image1.jpg", initCoarse = 16, rounds = 32)
  val proc    = FindPerspective(config)
  waitForProcessor(proc)
  println(new java.util.Date)
  println("_" * 33)
  proc.monitor()
  proc.start()
  proc.onSuccess {
    case _ =>
      println(new java.util.Date)
  }
}
