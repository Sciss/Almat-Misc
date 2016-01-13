package de.sciss.unlike

import de.sciss.file._
import de.sciss.processor._

object PerspectiveTest extends App {
  // import Unlike.mkFIn
  // val config  = FindPerspective.Config(pathA = mkFIn(9227), pathB = mkFIn(9228), rounds = 2)
  val config  = FindPerspective.Config(pathA = file("_creation") / "test_image1_move30_-30.jpg",
                                       pathB = file("_creation") / "test_image1.jpg", initCoarse = 16, rounds = 3)
  val proc    = FindPerspective(config)
  waitForProcessor(proc)
  proc.monitor()
  proc.start()
}
