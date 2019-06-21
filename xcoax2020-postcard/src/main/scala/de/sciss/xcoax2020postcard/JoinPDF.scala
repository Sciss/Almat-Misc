/*
 *  JoinPDF.scala
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

object JoinPDF {
  def main(args: Array[String]): Unit = {
    val workDir   = file("/data/projects/Almat/events/xcoax2020/postcard/back-pdf")
    val tempName  = "xcoax2020-back%04d.pdf"
    val fOut      = workDir.parent / "xcoax2020-back.pdf"
    val indices   = 1 to 500
    val names     = indices.map(tempName.format(_))

    names.foreach { name =>
      val fIn   = workDir / name
      require (fIn.isFile)
    }

    require (!fOut.exists())

    val args: Seq[String] = names ++ List("cat", "output", fOut.path)
    val cmd               = "pdftk"

    import sys.process._
    println(s":::::::: joining pdf ::::::::")
    Process(command = cmd +: args, cwd = Some(workDir)).!!
  }
}