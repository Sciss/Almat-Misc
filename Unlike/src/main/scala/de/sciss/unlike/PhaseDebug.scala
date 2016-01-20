package de.sciss.unlike

import java.awt.RenderingHints
import java.awt.geom.AffineTransform
import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.guiflitz.AutoView
import de.sciss.swingplus.CloseOperation
import de.sciss.swingplus.Implicits._
import de.sciss.unlike.PhaseCorrelation.{Product => Frame}

import scala.concurrent.{blocking, Future}
import scala.swing.Swing._
import scala.swing.{Frame => SFrame, Button, BorderPanel, Component, Graphics2D, Swing}

object PhaseDebug {
  val fBase     = userHome / "Documents" / "projects" / "Unlike"
  val fBaseIn   = fBase / "moor_8024"
  val fBaseJson = fBase / "moor_8024_json"

  val indices = (4414 + 60) to (4418 + 60)

  def main(args: Array[String]): Unit = Swing.onEDT(run())

  def mkFIn(index: Int): File = fBaseIn / s"moor_8024-%05d.jpg".format(index)

  case class Select(frame: Int, transform: Boolean = true)

  def run(): Unit = {
    val images = indices.map { idx =>
      val f = mkFIn(idx)
      println(f)
      ImageIO.read(f)
    }

    val transforms0 = Frame.identity +: EstimateVideoMotion.read(EstimateVideoMotion.Config(
      input = fBaseIn /* unused */, output = Some(fBaseJson / "moor_8024-%05d-%05d.json"), frames = indices))
    val transforms = RenderVideoMotion.integrate(transforms0)

    val avCfg           = AutoView.Config()
    avCfg.small         = true

    val frameView    = AutoView(transforms.head , avCfg)
    val selectView   = AutoView(Select(indices.head), avCfg)

    selectView.cell.addListener {
      case Select(index0, _) =>
        val index = index0 - indices.head
        frameView.cell() = transforms(index)
        comp.repaint()
    }

    lazy val ggCalc: Button = Button("Calc") {
      import PhaseCorrelation._
      val fut = Future(blocking {
        val settings  = Settings()
        val frame     = selectView.cell().frame
        // val index     = frame - indices.head
        val imgA      = prepareImage(mkFIn(frame - 1), settings)
        val imgB      = prepareImage(mkFIn(frame    ), settings)
        val fft       = prepareFFT(imgA)
        val pred      = prepareData(imgA, fft)
        val succ      = prepareData(imgB, fft)
        val matrix    = preparePeak(pred, succ, fft)
        val min       = matrix.data.min
        val max       = matrix.data.max
        val buf       = matrix.toAwt(mul = 1.0 / (max - min), add = -min)
        ImageIO.write(buf, "png", userHome / "Documents" / "temp" / "_killme.png")
        val peak1     = findPeakCentroidOLD(matrix)
        val peak2     = findPeakCentroid   (matrix)
        println(peak1)
        println(peak2)
      })
//      fut.foreach { _ =>
//        println("Done.")
////        onEDT {
////        }
//      }
    }

    lazy val comp: Component = new Component {
      preferredSize = (960, 540)

      override protected def paintComponent(g: Graphics2D): Unit = {
        val sel = selectView.cell()
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING , RenderingHints.VALUE_ANTIALIAS_ON)
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
        g.setRenderingHint(RenderingHints.KEY_RENDERING    , RenderingHints.VALUE_RENDER_QUALITY)
        g.scale(0.5, 0.5)
        val index = sel.frame - indices.head
        if (sel.transform) {
          val t = transforms(index)
          val at = AffineTransform.getTranslateInstance(t.translateX, t.translateY)
          g.drawImage(images(index), at, null)
        } else {
          g.drawImage(images(index), 0, 0, null)
        }
      }
    }

    val f = new SFrame {
      contents = new BorderPanel {
        add(comp, BorderPanel.Position.Center)
        add(new BorderPanel {
          add(selectView.component, BorderPanel.Position.North)
          add(frameView .component, BorderPanel.Position.South)
        }, BorderPanel.Position.East)
        add(ggCalc, BorderPanel.Position.South)
      }
    }
    f.defaultCloseOperation = CloseOperation.Exit
    f.pack().centerOnScreen()
    f.open()
  }
}
