package de.sciss.unlike

import de.sciss.processor._

object PerspectiveTest extends App {
  import Unlike.mkFIn
  val config  = FindPerspective.Config(pathA = mkFIn(9227), pathB = mkFIn(9228), rounds = 2)
  val proc    = FindPerspective(config)
  waitForProcessor(proc)
  proc.monitor()
  proc.start()
}
