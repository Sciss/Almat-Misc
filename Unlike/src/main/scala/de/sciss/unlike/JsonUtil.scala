/*
 *  JsonUtil.scala
 *  (Unlike)
 *
 *  Copyright (c) 2015-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.unlike

import de.sciss.file._
import play.api.libs.json.{Json, Reads, Writes}

import java.io.{FileInputStream, FileOutputStream}

object JsonUtil {
  def write[A](value: A, f: File)(implicit format: Writes[A]): Unit = {
    val json  = format.writes(value)
    val bytes = json.toString.getBytes("UTF-8")
    val fOut  = new FileOutputStream(f)
    try {
      fOut.write(bytes)
    } finally {
      fOut.close()
    }
  }

  def read[A](f: File)(implicit format: Reads[A]): A = {
    val fIn = new FileInputStream(f)
    try {
      val bytes = new Array[Byte](fIn.available())
      fIn.read(bytes)
      val json  = Json.parse(new String(bytes, "UTF-8"))
      format.reads(json).get
    } finally {
      fIn.close()
    }
  }
}
