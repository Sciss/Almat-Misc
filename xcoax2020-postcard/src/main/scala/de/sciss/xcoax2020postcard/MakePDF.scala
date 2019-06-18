/*
 *  MakePDF.scala
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

import java.io.{FileInputStream, FileOutputStream}

import de.sciss.file._

object MakePDF {
  def readAllString(f: File): String = {
    val fis = new FileInputStream(f)
    try {
      val arr = new Array[Byte](fis.available())
      fis.read(arr)
      new String(arr, "UTF-8")
    } finally {
      fis.close()
    }
  }

  def writeAllString(s: String, f: File): Unit = {
    val fos = new FileOutputStream(f)
    try {
      fos.write(s.getBytes("UTF-8"))
    } finally {
      fos.close()
    }
  }

  def main(args: Array[String]): Unit = {
    val tempName  = "outccr%04d.png"
    val fInOld    = file("/data/projects/Almat/events/xcoax2020/postcard/postal_xcoax2020almat_link.svg")
    val tempOut   = file("/data/projects/Almat/events/xcoax2020/postcard/back-pdf/xcoax2020-back%04d.pdf")
    val svgTemp   = readAllString(fInOld)
    val nameOld   = tempName.format(1)
    val nameIndices = {
      var res = List.empty[Int]
      var i   = 0
      while ({
        i = svgTemp.indexOf(nameOld, i)
        i >= 0 && {
          res ::= i
          i += 1
          true
        }
      }) ()

      res.reverse
    }
    require (nameIndices.size >= 2)

    for (idx <- 1 to 150) {
      val nameNew = tempName.format(idx)
      val svgNew  = nameIndices.foldLeft(svgTemp) { case (svgT, i) =>
        svgT.patch(i, nameNew, nameOld.length)
      }
      val fInNew  = File.createTemp(suffix = ".svg")
      writeAllString(svgNew, fInNew)
      val fOut    = formatTemplate(tempOut, idx)
      require (fInNew.exists() && !fOut.exists())
      import sys.process._
      val cmd = List("inkscape", "-A", fOut.path, fInNew.path)
      println(s":::::::: generating pdf $idx ::::::::")
      cmd.!!
      fInNew.delete()
    }
  }

  def formatTemplate(temp: File, idx: Int): File = {
    temp.parent / temp.name.format(idx)
  }
}