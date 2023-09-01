/*
 *  TransferTest.scala
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

import java.util.Locale

// requires g'mic -- `sudo apt install gmic`
object TransferTest {
  def main(args: Array[String]): Unit =
    run()

  def run(): Unit = {
    val baseDir     = file("/data/projects/Unlike")
    val inputDir    = baseDir / "unlike_out"
    val outputDir   = baseDir / "image_transfer"
    val numFrames   = 109
    val imgRefF     = inputDir / "unlike-out-0025.jpg"

    for (frameIdx <- 1 to numFrames) {
      val imgInF    = inputDir / "unlike-out-%04d.jpg".formatLocal(Locale.US, frameIdx)
      val imgOutF   = outputDir / "transfer-%d.jpg"   .formatLocal(Locale.US, frameIdx)
      println(imgOutF.name)
      if (!imgOutF.exists()) {
        val cmd = Seq("gmic",
          "-input", imgRefF.path, "-input", imgInF.path, "-transfer_histogram[1]", "[0]", "-output[1]", imgOutF.path
        )
        import sys.process._
        val code = cmd.!
        require (code == 0, s"${cmd.mkString(" ")} failed with code $code")
      }
    }

    // snap-210422_060305-crop.jpg
  }
}
