/*
 *  ModDirectImageToSound.scala
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

object ModDirectImageToSound extends Module {
  val name = "Direct Image to Sound"

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): FScape[S] = {
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, ImageFileIn => _, ImageFileOut => _, ImageFileSeqIn => _, ImageFileSeqOut => _, _}
    import de.sciss.fscape.lucre.graph._
    val f = FScape[S]()
    import de.sciss.fscape.lucre.MacroImplicits._
    f.setGraph {
      // version 06-Apr-2019 - Mellite 2.33.0
      // written by Hanns Holger Rutz
      // license: CC BY-SA 4.0

      val in0         = ImageFileIn("in")
      val w           = in0.width
      val h           = in0.height
      val framesOut   = w * h

      val gainType    = "gain-type" .attr(0)
      val gainDb      = "gain-db"   .attr(0.0)
      val gainAmt     = gainDb.dbAmp
      val fileType    = "out-type"  .attr(0)
      val smpFmt      = "out-format".attr(1)
      val sampleRate  = "sampleRate".attr(44100.0)
      val removeDC    = "remove-dc" .attr(0)
      val rotate      = "rotate"    .attr(0)

      def mkProgress(x: GE, n: GE, label: String): Unit =
        ProgressFrames(x, n, label)

      // LeakDC to Biquad translation:
      //b0 = 1
      //b1 = -1
      //b2 = 0
      //a1 = -coef
      //a2 = 0

      val in1 = If (rotate) Then {
        TransposeMatrix(in0, rows = h, columns = w)
      } Else {
        in0
      }

      val sig0        = If (removeDC) Then {
        val coef = 0.995
        Biquad(in1, b0 = +1.0, b1 = -1.0, a1 = -coef)
      } Else {
        in1
      }

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

      val written     = AudioFileOut("out", sig, fileType = fileType,
        sampleFormat = smpFmt, sampleRate = sampleRate)
      mkProgress(written, framesOut, "written")
    }
    f
  }

  def ui[S <: Sys[S]]()(implicit tx: S#Tx): Widget[S] = {
    import de.sciss.lucre.expr.ExOps._
    import de.sciss.lucre.expr.graph._
    import de.sciss.lucre.swing.graph._
    val w = Widget[S]()
    import de.sciss.synth.proc.MacroImplicits._
    w.setGraph {
      // version 06-Apr-2019
      val r = Runner("run")
      val m = r.messages
      m.changed.filter(m.nonEmpty) ---> Println(m.mkString("\n"))

      val in = ImageFileIn()
      in.value <--> Artifact("run:in")
      val out = AudioFileOut()
      out.value <--> Artifact("run:out")
      out.fileType <--> "run:out-type".attr(0)
      out.sampleFormat <--> "run:out-format".attr(1)
      out.sampleRate   <--> "run:sampleRate".attr(44100.0)

      val ggGain = DoubleField()
      ggGain.unit = "dB"
      ggGain.min  = -180.0
      ggGain.max  = +180.0
      ggGain.value <--> "run:gain-db".attr(0.0)

      val ggGainType = ComboBox(
        List("Normalized", "Immediate")
      )
      ggGainType.index <--> "run:gain-type".attr(0)

      val ggRemoveDC = CheckBox()
      ggRemoveDC.selected <--> "run:remove-dc".attr(false)

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
        mkLabel("Image Input:" ), in,
        mkLabel("Sound Output:"), out,
        //  Label(" "), Empty(),
        mkLabel("Gain:"), left(ggGain, ggGainType),
        Label(" "), Label(""),
        mkLabel("Remove DC:"), left(ggRemoveDC,
          mkLabel("  Rotate:"), ggRotate)
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
      val stopped = r.state sig_== 0
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
