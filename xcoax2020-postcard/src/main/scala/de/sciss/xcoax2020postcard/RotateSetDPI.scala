/*
 *  RotateSetDPI.scala
 *  (xCoAx2020-Postcard)
 *
 *  Copyright (c) 2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.xcoax2020postcard

import de.sciss.file._

object RotateSetDPI {
  def main(args: Array[String]): Unit = {
    val tempIn  = file("/data/projects/Almat/events/xcoax2020/postcard/composed/outcc%04d.png")
    val tempOut = file("/data/projects/Almat/events/xcoax2020/postcard/rotated/outccr%04d.png")

    for (idx <- 401 to 500) {
      val fIn     = formatTemplate(tempIn , idx)
      val fOut    = formatTemplate(tempOut, idx)
      require (fIn.exists() && !fOut.exists())
      import sys.process._
      val cmd = List("convert", fIn.path, "-monochrome", "-density", "600", "-rotate", "-90", fOut.path)
      println(s":::::::: rotating $idx ::::::::")
      cmd.!!
    }
  }

  def formatTemplate(temp: File, idx: Int): File = {
    temp.parent / temp.name.format(idx)
  }
}
