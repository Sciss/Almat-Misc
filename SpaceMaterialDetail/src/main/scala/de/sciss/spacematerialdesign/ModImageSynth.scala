/*
 *  ModImageSynth.scala
 *  (SpaceMaterialDesign)
 *
 *  Copyright (c) 2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.spacematerialdesign

import de.sciss.fscape.GE
import de.sciss.fscape.lucre.FScape
import de.sciss.fscape.modules.Module
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.Widget

import scala.Predef.{any2stringadd => _}

object ModImageSynth extends Module {
  val name = "Image Synthesizer"

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): FScape[S] = {
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, ImageFileIn => _, ImageFileOut => _, ImageFileSeqIn => _, ImageFileSeqOut => _, _}
    import de.sciss.fscape.lucre.graph._
    val f = FScape[S]()
    import de.sciss.fscape.lucre.MacroImplicits._
    f.setGraph {
      // version 02-Apr-2019 - Mellite 2.33.0
      // written by Hanns Holger Rutz
      // license: CC BY-SA 4.0

      val in0         = ImageFileIn("in")
      val w           = in0.width
      val h           = in0.height
      val hm          = h - 1
      val in1         = in0.out(0)
      val dur         = "dur"       .attr(10)
      val numVoices   = "maxVoices" .attr(128).min(h)

      val gainType    = "gain-type" .attr(0)
      val gainDb      = "gain-db"   .attr(0.0)
      val gainAmt     = gainDb.dbAmp
      val fileType    = "out-type"  .attr(0)
      val smpFmt      = "out-format".attr(1)
      val sampleRate  = "sampleRate".attr(44100.0)

      val minFreq     = "minFreq"   .attr(100)
      val maxFreq     = "maxFreq"   .attr(18000)
      val fadeTime    = "fadeTime"  .attr(0.1)
      val numFrames   = (dur * sampleRate).floor.max(2)
      val fadeFrames  = (fadeTime * sampleRate).floor.max(1)
      val inverted    = "inverted".attr(0) > 0
      val in = in1 * (1 - inverted) + (-in1 + (1: GE)) * inverted
      val minAmp      = "minLevel".attr(-60.0).dbAmp
      val maxAmp      = "maxLevel".attr(  0.0).dbAmp

      val framesXY    = numFrames * numVoices
      val x0          = LFSaw(1.0/numFrames).linLin(-1, 1, 0, numFrames)
      val frame       = x0.take(framesXY)
      val xs          = w / numFrames
      val x           = frame * xs
      val voiceIdx    = (Frames(x) / numFrames).floor
      val ys          = h / numVoices
      val y0          = voiceIdx * ys
      val y           = y0 + WhiteNoise(0.5)
      val freq        = y .linExp(0, hm, maxFreq, minFreq)

      val amp0        = ScanImage(in, width = w, height = h, x = x, y = y,
        wrap = 0, zeroCrossings = 0)
      val amp1        = amp0.linExp(0, 1, minAmp, maxAmp)
      val fdIn        = frame.clip(0, fadeFrames) / fadeFrames
      // N.B.: this is still a problem in FScape 2.16: BinaryOp is
      // not commutative with respect to signal lengths!
      val fdOut       = (-frame + numFrames).clip(0, fadeFrames) / fadeFrames
      val fade        = fdIn * fdOut
      val amp         = amp1 * fade
      val osc         = (SinOsc(freq/sampleRate) * amp).take(framesXY)

      val synthesized = Frames(osc)

      def mkProgress(frames: GE, n: GE, label: String): Unit =
        Progress(frames / n, Metro(sampleRate) | Metro(n - 1),
          label)

      mkProgress(synthesized, framesXY, "synthesized")

      // XXX TODO --- this is a limitation of using overlap-add,
      // we have to shift at least one frame per window...
      val mix         = OverlapAdd(osc, size = numFrames, step = 1)
      val framesOut   = numFrames + (numVoices - 1)
      val sig0        = mix

      val sig = If (gainType sig_== 0) Then {
        val rsmpBuf   = BufferDisk(sig0)
        val rMax      = RunningMax(Reduce.max(sig0.abs))
        val read      = Frames(rMax)
        mkProgress(read, framesOut, "analyze")
        val maxAmp    = rMax.last
        val div       = maxAmp + (maxAmp sig_== 0.0)
        val gainAmtN  = gainAmt / div
        rsmpBuf * gainAmtN

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
      // version 02-Apr-2019
      val r = Runner("run")
      val m = r.messages
      m.changed.filter(m.nonEmpty) ---> Println(m.mkString("\n"))

      val in = ImageFileIn()
      in.value <--> Artifact("run:in")
      val out = PathField()
      out.mode = PathField.Save
      out.value <--> Artifact("run:out")

      val ggInverted = ComboBox(
        List("Black is silent | White is loud",
          "Black is loud | White is silent")
      )
      val attrInverted = "run:inverted".attr(false)
      (ggInverted.index() > 0) ---> attrInverted
      ggInverted.index() = attrInverted.toInt

      val ggOutType = ComboBox(
        List("AIFF", "Wave", "Wave64", "IRCAM", "Snd")
      )
      ggOutType.index <--> "run:out-type".attr(0)

      val ggOutFmt = ComboBox(List(
        "16-bit int",
        "24-bit int",
        "32-bit float",
        "32-bit int",
        "64-bit float"
      ))
      ggOutFmt.index <--> "run:out-format".attr(1)

      val ggGain = DoubleField()
      ggGain.unit = "dB"
      ggGain.min  = -180.0
      ggGain.max  = +180.0
      ggGain.value <--> "run:gain-db".attr(0.0)

      val ggGainType = ComboBox(
        List("Normalized", "Immediate")
      )
      ggGainType.index <--> "run:gain-type".attr(0)

      val ggSR  = ComboBox(
        List(44100, 48000, 88200, 96000, 176400, 192000)
      )
      val attrSR = "run:sampleRate".attr[Int]
      ggSR.valueOption() = attrSR
      ggSR.valueOption().getOrElse(44100) ---> attrSR

      val ggDur  = DoubleField()
      ggDur.min  =    1.0
      ggDur.max  = 3600.0
      ggDur.unit = "s  "
      ggDur.value <--> "run:dur".attr(10.0)

      val ggVoices   = IntField()
      ggVoices.min   = 1
      ggVoices.max   = 16384
      ggVoices.unit = "    "
      ggVoices.value <--> "run:maxVoices".attr(128)
      val ggMaxFreq  = DoubleField()
      ggMaxFreq.min  =    10.0
      ggMaxFreq.max  = 96000.0
      ggMaxFreq.unit = "Hz"
      ggMaxFreq.value <--> "run:maxFreq".attr(18000.0)
      val ggMinFreq  = DoubleField()
      ggMinFreq.min  =    10.0
      ggMinFreq.max  = 96000.0
      ggMinFreq.unit = "Hz"
      ggMinFreq.value <--> "run:minFreq".attr(100.0)

      val ggMaxLevel = DoubleField()
      ggMaxLevel.min  = -120.0
      ggMaxLevel.max  =    0.0
      ggMaxLevel.unit = "dB"
      ggMaxLevel.value <--> "run:maxLevel".attr(0.0)
      val ggMinLevel = DoubleField()
      ggMinLevel.min  = -120.0
      ggMinLevel.max  =   0.0
      ggMinLevel.unit = "dB"
      ggMinLevel.value <--> "run:minLevel".attr(-60.0)

      val ggFade = DoubleField()
      ggFade.min  =    0.01
      ggFade.max  = 1800.0
      ggFade.unit = "s  "
      ggFade.value <--> "run:fadeTime".attr(0.1)

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
        mkLabel("Output Format:"),
        left(ggOutType, ggOutFmt, ggSR, Label("Hz")),
        //  Label(" "), Empty(),
        mkLabel("Gain:"), left(ggGain, ggGainType),
        Label(" "), Label(""),
        mkLabel("Colors:"), left(ggInverted),
        mkLabel("Duration:"), ggDur, // FlowPanel(ggDur),
        mkLabel("Max.freq:"), ggMaxFreq,
        mkLabel("Min.freq:"), ggMinFreq,
        mkLabel("Max.level:"), ggMaxLevel,
        mkLabel("Min.level:"), ggMinLevel,
        mkLabel("Max.voices:"), ggVoices,
        mkLabel("Fade in/out:"), ggFade
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
