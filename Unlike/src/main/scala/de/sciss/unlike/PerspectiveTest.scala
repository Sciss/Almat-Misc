/*
 *  PerspectiveTest.scala
 *  (Unlike)
 *
 *  Copyright (c) 2015-2018 Hanns Holger Rutz. All rights reserved.
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

  //  val config  = FindPerspectiveOLD.Config(pathA = file("_creation") / "test_image1_move30_-30.jpg",
  //    pathB = file("_creation") / "test_image1.jpg", initCoarse = 16, rounds = 32)
  //  val proc    = FindPerspectiveOLD(config)
  //  waitForProcessor(proc)
  //  println(new java.util.Date)
  //  println("_" * 33)
  //  proc.monitor()
  //  proc.start()
  //  proc.onSuccess {
  //    case _ =>
  //      println(new java.util.Date)
  //  }

//  val config  = PhaseCorrelation.Config(pathA = file("_creation") / "test_image1_move30_-30.jpg",
//    pathB = file("_creation") / "test_image1.jpg",
//    downSample = 1.0)

//  val config  = PhaseCorrelation.Config(pathA = Unlike.mkFIn(9227),
//    pathB = Unlike.mkFIn(9228),
//    downSample = 2.0)

  val config  = PhaseCorrelation.Config(pathA = mkBird(1001),
                                        pathB = mkBird(1002))

//  val config  = PhaseCorrelation.Config(
//    pathA = file("_creation") / "test_image3.png",
//    pathB = file("_creation") / "test_image3_move17_-11.png",
//    downSample = 1.0)

  val proc    = PhaseCorrelation(config)
  waitForProcessor(proc)
  println(new java.util.Date)
  println("_" * 33)
  proc.monitor()
  proc.start()
  proc.onSuccess {
    case _ =>
      println(new java.util.Date)
      Thread.sleep(200)
      sys.exit()
  }

  def mkBird(frame: Int): File = file("_creation") / "bird" / f"$frame%04d.jpg"
}