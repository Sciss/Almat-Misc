/*
 *  DetermineBoundaries.scala
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
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{GE, Graph, graph}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object CenterImages {
  case class Config(fIn: File, fOut: File)

  def main(args: Array[String]): Unit = {
    for (idx <- 401 to 500) {
      val c = Config(
        fIn   = file(f"/data/projects/Almat/events/xcoax2020/postcard/render/out$idx%04d.png"),
        fOut  = file(f"/data/projects/Almat/events/xcoax2020/postcard/centered/outc$idx%04d.png"),
      )

      require (c.fIn.exists() && !c.fOut.exists())

      println(s":::::::: idx $idx ::::::::")
      val fut = run(c)
      Await.result(fut, Duration.Inf)
    }
  }

  def any2stringadd: Any = ()

  case class Insets(top: GE, left: GE, bottom: GE, right: GE)

  val wIn       : Int = 3488  // divisible by 32
  val hIn       : Int = 2464  // divisible by 32
  val frameSzIn : Int = wIn * hIn

  val wOut      : Int = 3744  // 3732  // 158 mm at 600 dpi
  val hOut      : Int = 2720  // 2716  // 115 mm at 600 dpi
  val frameSzOut: Int = wOut * hOut

  def getInsets(in: => GE): Insets = {
    import graph._

    def test(in: GE): GE =
      (Length(in.takeWhile(in sig_== 1.0)) / wIn).floor

    val top     = test(in)
    val bottom  = test(ReverseWindow(in, frameSzIn))
    val left    = test(TransposeMatrix(in, rows = hIn, columns = wIn))
    val right   = test(ReverseWindow(TransposeMatrix(in, rows = hIn, columns = wIn), frameSzIn))
    Insets(top = top, left = left, bottom = bottom, right = right)
  }

  def run(c: Config): Future[Unit] = {
    import c._

    val g = Graph {
      import graph._
//      val fIn    = file("/data/projects/Almat/events/xcoax2020/postcard/render/out0001.png")

      def mkIn(): GE = ImageFileIn(fIn, numChannels = 1).take(frameSzIn)

      val insets = getInsets(mkIn())
      import insets._

      top   .poll(0, "top   ")
      bottom.poll(0, "bottom")
      left  .poll(0, "left  ")
      right .poll(0, "right ")

      val wInner  = wIn - (left + right)
      val hInner  = hIn - (top + bottom)
      val tx      = (((wOut - wInner) / 2) - left).roundTo(1)
      val ty      = (((hOut - hInner) / 2) - top ).roundTo(1)

      val at = AffineTransform2D.translate(mkIn(), widthIn = wIn, heightIn = hIn, widthOut = wOut, heightOut = hOut,
        tx = tx, ty = ty, zeroCrossings = 0, wrap = 0)

      val sig = at

      val specOut = ImageFile.Spec(width = wOut, height = hOut, numChannels = 1,
        sampleFormat = ImageFile.SampleFormat.Int16)
      ImageFileOut(file = fOut, spec = specOut, in = sig)
    }

//    var gui: SimpleGUI = null
    val cfg       = Control.Config()
    cfg.useAsync  = false
//    cfg.progressReporter = p => Swing.onEDT(gui.progress = p.total)
    cfg.blockSize = 2048 // 512 // 2048

    val ctl = Control(cfg)
//    Swing.onEDT {
//      gui = SimpleGUI(ctl)
//    }
    ctl.run(g)
    ctl.status
  }
}