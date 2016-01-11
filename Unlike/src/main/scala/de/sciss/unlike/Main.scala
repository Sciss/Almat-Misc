package de.sciss.unlike

import scala.swing.Swing

object Main {
  def main(args: Array[String]): Unit = {
    Swing.onEDT(Unlike.mkFrame())
  }
}
