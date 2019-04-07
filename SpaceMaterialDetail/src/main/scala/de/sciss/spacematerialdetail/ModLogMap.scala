/*
 *  ModLogMap.scala
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

object ModLogMap extends Module {
  val name = "Logistic Map"

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): FScape[S] = {
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, ImageFileIn => _, ImageFileOut => _, ImageFileSeqIn => _, ImageFileSeqOut => _, _}
    import de.sciss.fscape.lucre.graph._
    val fsc = FScape[S]()
    import de.sciss.fscape.lucre.MacroImplicits._
    fsc.setGraph {
      // version 06-Apr-2019 - Mellite 2.33.0
      // written by Hanns Holger Rutz
      // license: CC BY-SA 4.0
      val maxIt   = 400 // "max-iter".attr(100).max(2)
      val w       = "width"   .attr(640).max(2)
      val h       = "height"  .attr(480).max(2)
      val ra      = "r-min"   .attr(0.0)
      val rb      = "r-max"   .attr(4.0)
      val invert  = "invert"  .attr(1).clip()
      // val pow     = "pow"     .attr(1.0)
      val y0      = "y-min"   .attr(0.0)
      val y1      = "y-max"   .attr(1.0)

      val fileType      = "out-type"   .attr(0)
      val smpFmt        = "out-format" .attr(0)
      val quality       = "out-quality".attr(90)

      // def bentFun(p: GE = 0.96): (GE, GE) => GE =
      //  (x, r) => (r * x * (1.0 - x)).pow(p)

      def stdFun: (GE, GE) => GE =
        (x, r) => r * x * (1.0 - x)

      val fun = stdFun // bentFun(pow)

      val itH   = maxIt/2 // * 2/3
      val itSkip = maxIt - itH
      val wm  = w - 1
      val hm  = h - 1

      val x0  = ArithmSeq(0, length = w)  // x coordinates
      val r   = x0.linLin(0, wm, ra, rb)  // ... become `r` in the logistic function

      // the `fold` operation on a sequence can be used to
      // apply a function recursively. We thread two values,
      // `f` and `g`. `f` is the recursive variable (or `x` in the formula),
      // beginning at 0.5. `g` is accumulating the results of the
      // recursion, keeping only the second half of all iterations.

      val (_, fSeq) = (0 until maxIt).foldLeft[(GE, GE)]((0.5, Empty())) {
        case ((f, g), it) =>
          val fN = fun(f, r)                       // recursion
          val gN = if (it < itSkip) g else {
            g ++ BufferMemory(fN, w) // collect second half of iterations
          }
          (fN, gN)
      }

      val x     = RepeatWindow(x0, size = w, num = itH)
      val y     = fSeq.linLin(y1, y0, 0, hm).roundTo(1)

      val cFg   = invert      // foreground "color"
      val cBg   = 1.0 - cFg   // background "color"

      // apply the pixels
      val sig = PenImage(
        width   = w,
        height  = h,
        src     = DC(cFg).take(itH * w),
        dst     = cBg,
        x       = x,
        y       = y
      )
      ProgressFrames(sig, w * h)
      ImageFileOut("out", sig, width = w, height = h,
        fileType = fileType, sampleFormat = smpFmt, quality = quality)
    }
    fsc
  }

  def ui[S <: Sys[S]]()(implicit tx: S#Tx): Widget[S] = {
    import de.sciss.lucre.expr.ExOps._
    import de.sciss.lucre.expr.graph._
    import de.sciss.lucre.swing.graph._
    val widget = Widget[S]()
    import de.sciss.synth.proc.MacroImplicits._
    widget.setGraph {
      // version 05-Apr-2019
      val r = Runner("run")
      val m = r.messages
      m.changed.filter(m.nonEmpty) ---> Println(m.mkString("\n"))

      def mkLabel(text: String) = {
        val l = Label(text)
        l.hAlign = Align.Trailing
        l
      }

      val lbOut  = mkLabel("Output Image:")
      val pfOut = ImageFileOut()
      pfOut.value         <--> Artifact("run:out")
      pfOut.fileType      <--> "run:out-type"   .attr(0)
      pfOut.sampleFormat  <--> "run:out-format" .attr(0)
      pfOut.quality       <--> "run:out-quality".attr(90)

      val lbWidth = mkLabel("Width:")
      val ggWidth = IntField()
      ggWidth.min = 2
      ggWidth.value <--> "run:width".attr(640)

      val lbHeight = mkLabel("Height:")
      val ggHeight = IntField()
      ggHeight.min = 2
      ggHeight.value <--> "run:height".attr(480)

      val lbRMin = mkLabel("r Min:")
      val ggRMin = DoubleField()
      ggRMin.step = 0.01
      ggRMin.decimals = 3
      //ggRMin.min = 0.0
      //ggRMin.max = 1.0
      ggRMin.value <--> "run:r-min".attr(0.0)

      val lbRMax = mkLabel("r Max:")
      val ggRMax = DoubleField()
      ggRMax.step = 0.01
      ggRMax.decimals = 3
      //ggRMax.min = 0.0
      //ggRMax.max = 1.0
      ggRMax.value <--> "run:r-max".attr(4.0)

      val lbYMin = mkLabel("y Min:")
      val ggYMin = DoubleField()
      ggYMin.step = 0.01
      ggYMin.decimals = 3
      //ggYMin.min = 0.0
      //ggYMin.max = 1.0
      ggYMin.value <--> "run:y-min".attr(0.0)

      val lbYMax = mkLabel("y Max:")
      val ggYMax = DoubleField()
      ggYMax.step = 0.01
      ggYMax.decimals = 3
      //ggYMax.min = 0.0
      //ggYMax.max = 1.0
      ggYMax.value <--> "run:y-max".attr(1.0)

      // val lbPow = mkLabel("Pow (Bend):")
      // val ggPow = DoubleField()
      // ggPow.min = 0.0
      // ggPow.step = 0.01
      // ggPow.decimals = 3
      // //ggPow.max = 1.0
      // ggPow.value <--> "run:pow".attr(1.0)

      val lbInvert = mkLabel("Invert Colors:")
      val ggInvert = CheckBox()
      ggInvert.selected <--> "run:invert".attr(true)

      // def left(c: Component*): Component = {
      //   val f = FlowPanel(c: _*)
      //   f.align = Align.Leading
      //   f.vGap = 0
      //   f
      // }

      val pTop = GridPanel(
        lbOut         , pfOut,
        Label(" ")    , Empty(),
        lbWidth       , ggWidth,
        lbHeight      , ggHeight,
        lbRMin        , ggRMin,
        lbRMax        , ggRMax,
        lbYMin        , ggYMin,
        lbYMax        , ggYMax,
        // lbPow         , ggPow,
        lbInvert      , ggInvert
      )
      pTop.compact = true
      pTop.columns = 2


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
        north = pTop,
        south = bot
      )
      bp.vGap = 8
      bp.border = Border.Empty(8, 8, 0, 4)
      bp
    }
    widget
  }
}
