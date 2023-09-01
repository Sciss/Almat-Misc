/*
 *  ModDirectSoundToImage.scala
 *  (SpaceMaterialDetail)
 *
 *  Copyright (c) 2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.spacematerialdetail

import de.sciss.fscape.GE
import de.sciss.fscape.lucre.FScape
import de.sciss.fscape.modules.Module
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.Widget

import scala.Predef.{any2stringadd => _}

object ModDirectSoundToImage extends Module {
  val name = "Direct Sound to Image"

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): FScape[S] = {
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, ImageFileIn => _, ImageFileOut => _, ImageFileSeqIn => _, ImageFileSeqOut => _, _}
    import de.sciss.fscape.lucre.graph._
    val f = FScape[S]()
    import de.sciss.fscape.lucre.MacroImplicits._
    f.setGraph {
      // version 07-Apr-2019 - Mellite 2.33.0
      // written by Hanns Holger Rutz
      // license: CC BY-SA 4.0

      val in0         = AudioFileIn("in")
      val w           = "width"     .attr(640)
      val h           = "height"    .attr(480)
      val framesOut   = w * h

      val gainType    = "gain-type" .attr(0)
      val gainDb      = "gain-db"   .attr(0.0)
      val gainAmt     = gainDb.dbAmp
      val fileType    = "out-type"  .attr(0)
      val smpFmt      = "out-format".attr(1)
      val quality     = "out-quality".attr(90)
      val thresh      = "threshold" .attr(0.0)
      val rotate      = "rotate"    .attr(0)

      def mkProgress(x: GE, n: GE, label: String): Unit =
        ProgressFrames(x, n, label)

      // LeakDC to Biquad translation:
      //b0 = 1
      //b1 = -1
      //b2 = 0
      //a1 = -coef
      //a2 = 0

      val in1 = in0.take(framesOut)
      val in2 = If (rotate) Then {
        TransposeMatrix(in1, rows = w, columns = h)
      } Else {
        in1
      }

      val sig0        = (in2 - thresh).max(0.0)

      val sig = If (gainType sig_== 0) Then {
        val sig0Buf   = BufferDisk(sig0)
        val rMax      = RunningMax(Reduce.max(sig0.abs))
        val read      = Frames(rMax)
        mkProgress(read, framesOut, "analyze")
        val maxAmp    = rMax.last
        val div       = maxAmp + (maxAmp sig_== 0.0)
        val gainAmtN  = gainAmt / div
        sig0Buf * gainAmtN

      } Else {
        sig0 * gainAmt
      }

      ImageFileOut("out", in = sig, width = w, height = h, fileType = fileType,
        sampleFormat = smpFmt, quality = quality)
      mkProgress(sig, framesOut, "written")
    }
    f
  }

  def ui[S <: Sys[S]]()(implicit tx: S#Tx): Widget[S] = {
    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    import de.sciss.lucre.swing.graph._
    val w = Widget[S]()
    import de.sciss.synth.proc.MacroImplicits._
    w.setGraph {
      // version 24-Jun-2020
      val r = Runner("run")
      val m = r.messages
      m.changed.filter(m.nonEmpty) ---> PrintLn(m.mkString("\n"))

      val in = AudioFileIn()
      in.value <--> Artifact("run:in")
      val out = ImageFileOut()
      out.value <--> Artifact("run:out")
      out.fileType      <--> "run:out-type"   .attr(0)
      out.sampleFormat  <--> "run:out-format" .attr(0)
      out.quality       <--> "run:out-quality".attr(90)

      val ggWidth = IntField()
      ggWidth.unit = "px"
      ggWidth.min  = 2
      ggWidth.max  = 16384
      ggWidth.value <--> "run:width".attr(640)

      val ggHeight = IntField()
      ggHeight.unit = "px"
      ggHeight.min  = 2
      ggHeight.max  = 16384
      ggHeight.value <--> "run:height".attr(480)

      val ggGain = DoubleField()
      ggGain.unit = "dB"
      ggGain.min  = -180.0
      ggGain.max  = +180.0
      ggGain.value <--> "run:gain-db".attr(0.0)

      val ggGainType = ComboBox(
        List("Normalized", "Immediate")
      )
      ggGainType.index <--> "run:gain-type".attr(0)

      val ggThresh = DoubleField()
      ggThresh.min  = 0.0
      ggThresh.max  = 1.0
      ggThresh.value <--> "run:threshold".attr(0.0)

      val ggRotate = CheckBox()
      ggRotate.selected <--> "run:rotate".attr(false)

      def mkLabel(text: String) = {
        val l = Label(text)
        l.hAlign = Align.Trailing
        l
      }

      def left(c: Component*): Component = {
        val f = FlowPanel(c: _*)
        f.align = Align.Leading
        f.vGap = 0
        f
      }
      
      val p = GridPanel(
        mkLabel("Sound Input:" ), in,
        mkLabel("Input Output:"), out,
        //  Label(" "), Empty(),
        mkLabel("Width:"), left(ggWidth),
        mkLabel("Height:"), left(ggHeight),
        mkLabel("Gain:"), left(ggGain, ggGainType),
        Label(" "), Label(""),
        mkLabel("Threshold/Clip:"), left(ggThresh),
        mkLabel("Rotate:"), left(ggRotate)
      )
      p.columns = 2
      p.hGap = 8
      p.compact = true

      val ggRender  = Button(" Render ")
      val ggCancel  = Button(" X ")
      ggCancel.tooltip = "Cancel Rendering"
      val pb        = ProgressBar()
      ggRender.clicked ---> r.run
      ggCancel.clicked ---> r.stop
      val stopped = (r.state sig_== 0) || (r.state sig_== 4)
      ggRender.enabled = stopped
      ggCancel.enabled = !stopped
      pb.value = (r.progress * 100).toInt
      val bot = BorderPanel(
        center  = pb,
        east    = {
          val f = FlowPanel(ggCancel, ggRender)
          f.vGap = 0
          f
        }
      )
      bot.vGap = 0
      val bp = BorderPanel(
        north = p,
        south = bot
      )
      bp.vGap = 8
      bp.border = Border.Empty(8, 8, 0, 4)
      bp
    }
    w
  }
}
