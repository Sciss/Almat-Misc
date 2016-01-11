/*
 * Unlike.scala
 * (Unlike)
 *
 * Copyright (c) 2015 Hanns Holger Rutz. All rights reserved.
 *
 * This software and music is published under the
 * Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License
 * (CC BY-NC-ND 4.0)
 *
 * For further information, please contact Hanns Holger Rutz at
 * contact@sciss.de
 */

package de.sciss.unlike

import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.{FileInputStream, FileOutputStream}
import javax.imageio.ImageIO
import javax.swing.{KeyStroke, SpinnerNumberModel}

import com.jhlabs.image.NoiseFilter
import de.sciss.desktop.{FileDialog, OptionPane}
import de.sciss.file._
import de.sciss.guiflitz.AutoView
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.swingplus.Implicits._
import de.sciss.swingplus.{CloseOperation, ListView, Spinner}
import play.api.libs.json.Json

import scala.collection.mutable
import scala.concurrent.blocking
import scala.swing.Swing._
import scala.swing.{Action, BorderPanel, BoxPanel, Button, Component, FlowPanel, Frame, Graphics2D, Menu, MenuBar, MenuItem, Orientation, ScrollPane}

object Unlike {
  private val fBase = userHome / "Pictures" /"2015"/"12"/"07"

  def mkFrame(): Unit = {
//    val hAxis1  = new Axis(Orientation.Horizontal)
//    val vAxis1  = new Axis(Orientation.Vertical  )
//    val hAxis2  = new Axis(Orientation.Horizontal)
//    val vAxis2  = new Axis(Orientation.Vertical  )

    val mFrame  = new SpinnerNumberModel(1, 1, 3297, 1)
    val ggFrame = new Spinner(mFrame)

    val iw      = 640
    val ih      = 640
    val img     = new BufferedImage(iw, ih, BufferedImage.TYPE_INT_ARGB)

    val avCfg   = AutoView.Config()
    avCfg.small = true

    val cfg0    = Config() // (id = 1, firstFrame = 231, lastFrame = 1006 - 1 /* 1780 */, thresh = 127, sizeIn = 320)

    val cfgView  = AutoView(cfg0, avCfg)

    def mkSituation(): Config = cfgView.value

//    def updateAxes(cfg: Config): Unit = {
//      hAxis1.minimum  = cfg.aMin
//      hAxis1.maximum  = cfg.aMax
//      vAxis1.minimum  = cfg.bMin
//      vAxis1.maximum  = cfg.bMax
//      hAxis2.minimum  = hAxis1.minimum
//      hAxis2.maximum  = hAxis1.maximum
//      vAxis2.minimum  = vAxis1.minimum
//      vAxis2.maximum  = vAxis1.maximum
//    }

    val comp: Component = new Component {
      preferredSize = (iw, ih)
      override protected def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        g.drawImage(img, 0, 0, peer)
      }
    }

    def updateImage(): Unit = {
      val img1  = mkImage(cfgView.value, 9227 /* mFrame.getNumber.intValue() */)
      val g     = img.createGraphics()
      val sx    = img.getWidth .toDouble / img1.getWidth
      val sy    = img.getHeight.toDouble / img1.getHeight
      g.drawImage(img1, AffineTransform.getScaleInstance(sx, sy), null)
      g.dispose()
      comp.repaint()
    }

    cfgView.cell.addListener {
      case _ => updateImage()
    }

    mFrame.addChangeListener(ChangeListener(_ => updateImage()))

    val bp = new BorderPanel {
      add(comp, BorderPanel.Position.Center)
    }

    def setSituation(sit: Situation): Unit = {
      cfgView.cell()    = sit.config
    }

    val mb = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(new Action("Load Settings...") {
          accelerator = Some(KeyStroke.getKeyStroke("ctrl O"))
          def apply(): Unit = {
            val dlg = FileDialog.open()
            dlg.setFilter(_.ext.toLowerCase == "json")
            dlg.show(None).foreach { f =>
              val fin = new FileInputStream(f)
              val arr = new Array[Byte](fin.available())
              fin.read(arr)
              fin.close()
              val jsn = Json.parse(new String(arr, "UTF-8"))
              val res = Json.fromJson[Config](jsn).get
              cfgView.cell() = res // .lya
            }
          }
        })

        contents += new MenuItem(new Action("Export Image...") {
          accelerator = Some(KeyStroke.getKeyStroke("ctrl S"))
          def apply(): Unit = {
            val sit = mkSituation()
            FileDialog.save(init = Some(userHome / s"collat_${sit.hashCode.toHexString}.png")).show(None).foreach { f =>
              val pFull = renderImage(sit, frame = mFrame.getNumber.intValue(), f = f.replaceExt("png"))
              val futTail = pFull.map { _ =>
                val json = Config.format.writes(sit).toString()
                blocking {
                  val jsonOut = new FileOutputStream(f.replaceExt("json"))
                  jsonOut.write(json.getBytes("UTF-8"))
                  jsonOut.close()
                }
              }
              mkProgressDialog("Exporting...", pFull, futTail)
            }
          }
        })

        contents += new MenuItem(new Action("Export Image Sequence...") {
          accelerator = Some(KeyStroke.getKeyStroke("ctrl shift S"))

          def apply(): Unit = {
            val cfg = mkSituation()
            val initName = cfg.hashCode.toHexString
            FileDialog.save(init = Some(userHome / s"collat_$initName.png")).show(None).foreach { f =>
              val pFull = renderImageSequence(cfg, fOut = f)
              mkProgressDialog("Exporting...", pFull, pFull)
            }
          }
        })
      }
    }

    lazy val mSnapshots: ListView.Model[Situation] with mutable.Buffer[Situation] = ListView.Model.empty
    lazy val ggSnapshots: ListView[Situation] = new ListView(mSnapshots)
    ggSnapshots.visibleRowCount = 6
    lazy val ggAddSnapshot: Button = Button("Add") {
      // val sit       = mkSituation()
      val initFrame = mSnapshots.lastOption.map(_.index + 250).getOrElse(0)
      val opt = OptionPane.textInput("Index:", initial = initFrame.toString)
      opt.show(None, title = "Add Snapshot").foreach { str =>
        val index = str.trim.toInt
        val i0    = mSnapshots.indexWhere(_.index >= index)
        val i     = if (i0 < 0) mSnapshots.size else i0
        mSnapshots.insert(i, Situation(index))
      }
    }
//    lazy val ggMoveSnapshot: Button = Button("Move") {
//      ggSnapshots.selection.indices.headOption.foreach { row =>
//        val old @ Situation(initFrame, sit) = mSnapshots(row)
//        val opt = OptionPane.textInput("Key Frame:", initial = initFrame.toString)
//        opt.show(None, title = "Move Snapshot").foreach { str =>
//          val frame = str.trim.toInt
//          assert(row == mSnapshots.indexOf(old))
//          mSnapshots.remove(row)
//          val i0    = mSnapshots.indexWhere(_.index >= frame)
//          val i     = if (i0 < 0) mSnapshots.size else i0
//          mSnapshots.insert(i, Situation(frame, sit))
//        }
//      }
//    }
    lazy val ggRemoveSnapshot: Button = Button("Remove") {
      ggSnapshots.selection.indices.toList.sorted.reverse.foreach { row =>
        mSnapshots.remove(row)
      }
    }
    lazy val ggRecallSnapshot: Button = Button("Recall") {
      ggSnapshots.selection.indices.headOption.foreach { row =>
        val sit = mSnapshots(row) // .situation
        setSituation(sit)
      }
    }

    lazy val pSnapshots: Component = new ScrollPane(ggSnapshots)

    val fr = new Frame { self =>
      title = "Collateral"
      contents = new BorderPanel {
        add(bp, BorderPanel.Position.Center)
        add(new BoxPanel(Orientation.Vertical) {
          contents += cfgView .component
          contents += new FlowPanel(ggFrame)
        }, BorderPanel.Position.East)
      }
      resizable = false
      menuBar = mb
      pack().centerOnScreen()
      open()
    }
    fr.defaultCloseOperation = CloseOperation.Exit

    // ggRender.doClick()
    updateImage()
  }

  def renderImage(config: Config, frame: Int, f: File): Processor[Unit] = {
    val res = new RenderImage(config, frame = frame, f = f)
    res.start()
    res
  }

  private final class RenderImage(config: Config, frame: Int, f: File)
    extends ProcessorImpl[Unit, Processor[Unit]] with Processor[Unit] {

    protected def body(): Unit = blocking {
      val fOut  = f.replaceExt("png")
      if (!fOut.exists()) {
        val img = mkImage(config, index = frame)
        ImageIO.write(img, "png", fOut)
      }
      progress = 1.0
    }
  }

  def renderImageSequence(config: Config, fOut: File): Processor[Unit] = {
    ???
//    import config.resampleWindow
//    require(resampleWindow % 2 == 1, s"resampleWindow ($resampleWindow) must be odd")
//    val res = new RenderImageSequence(config = config, fOut = fOut)
//    res.start()
//    res
  }

  def mkImage(config: Config, index: Int): BufferedImage = {
    val imgCrop   = readFrame(config, index)
    val imgUp     = imgCrop // mkResize (config, imgCrop)
    val imgNoise  = mkNoise  (config, imgUp)
    imgNoise // mkThresh(config, imgNoise)
  }

  def cropImage2(config: Config, in: BufferedImage): BufferedImage = {
    ???
//    import config.sizeIn
//    cropImage(in, 145 + (430 - sizeIn)/2, 20 + (430 - sizeIn)/2, sizeIn, sizeIn)
  }

  def mkFIn(config: Config, index: Int): File = fBase / s"DSC_$index.JPG"

  def readFrame(config: Config, index: Int): BufferedImage = {
    val fIn1      = mkFIn(config, index)
    val imgIn     = ImageIO.read(fIn1)
    imgIn
//    val imgCrop   = cropImage2(config, imgIn)
//    imgCrop
  }
  
  def mkResize(config: Config, in: BufferedImage): BufferedImage = {
    ???
//    import config.sizeOut
//    val resizeOp  = new ResampleOp(sizeOut, sizeOut)
//    resizeOp.filter(in, null)
  }
  
  def mkNoise(config: Config, in: BufferedImage): BufferedImage = if (config.noise <= 0) in else {
    val noiseOp = new NoiseFilter
    noiseOp.setAmount(config.noise)
    noiseOp.setMonochrome(true)
    noiseOp.filter(in, null)
  }

//  def mkThresh(config: Config, in: BufferedImage, out: BufferedImage = null): BufferedImage =
//    if (config.thresh <= 0) in else {
//      import config.{sizeOut, thresh}
//      val threshOp  = new ThresholdFilter(thresh)
//      val out1      = if (out != null) out else new BufferedImage(sizeOut, sizeOut, BufferedImage.TYPE_BYTE_BINARY)
//      threshOp.filter(in, out1)
//    }

  private final class RenderImageSequence(config: Config, fOut: File)
    extends ProcessorImpl[Unit, RenderImageSequence] with Processor[Unit] {

    protected def body(): Unit = blocking {
      val jsonF = fOut.replaceExt("json")
      if (!jsonF.exists()) blocking {
        val json    = Config.format.writes(config).toString()
        val jsonOut = new FileOutputStream(jsonF)
        jsonOut.write(json.getBytes("UTF-8"))
        jsonOut.close()
      }

//      val dirOut        = fOut.parent
//      val childOut      = fOut.base
//      val frameInMul    = if (lastFrame >= firstFrame) 1 else -1
//      val frameSeq0     = firstFrame to lastFrame by frameInMul
//      val frameSeq      = if (dropRate <= 0) frameSeq0 else {
//        frameSeq0.filterNot { frame =>
//          val x     = ((frame - dropFrame) / dropRate + 0.5).toInt
//          val drop  = (x * dropRate + dropFrame + 0.5).toInt
//          frame == drop
//        }
//      }
//
//      val numInFrames   = frameSeq.size // math.abs(lastFrame - firstFrame + 1)
//      // val frameOff      = firstFrame // if (lastFrame >= firstFrame) firstFrame else lastFrame
//      val numOutFrames  = numInFrames * 2
//      val imgOut        = new BufferedImage(sizeOut, sizeOut, BufferedImage.TYPE_BYTE_BINARY)
//
//      def mkFOut(frame: Int): File = dirOut / s"$childOut-$frame.png"
//
//      // e.g. resampleWindow = 5, winH = 2 ; LLLRR
//      val winH = resampleWindow / 2
//
//      var frame0      = readFrame(config, frameSeq(0) /* frameOff */)
//      val widthIn     = frame0.getWidth
//      val heightIn    = frame0.getHeight
//
//      assert (widthIn == sizeIn && heightIn == sizeIn)
//
//      val frameWindow = Array.tabulate(resampleWindow) { i =>
//        val j = i - winH
//        if (j <= 0) frame0 else readFrame(config, frameSeq(j) /* j * frameInMul + frameOff*/)
//      }
//
//      frame0 = null // let it be GC'ed
//
//      val resample    = Resample(Resample.Quality.Medium /* Low */)
//      val imgRsmp     = Array.fill(2)(new BufferedImage(widthIn, heightIn, BufferedImage.TYPE_BYTE_GRAY))
//      val bufRsmpIn   = new Array[Float](resampleWindow)
//      val bufRsmpOut  = new Array[Float](2)
//
//      def performResample(): Unit = {
//        var y = 0
//        while (y < heightIn) {
//          var x = 0
//          while (x < widthIn) {
//            var t = 0
//            while (t < resampleWindow) {
//              val rgbIn = frameWindow(t).getRGB(x, y)
//              val vIn = (((rgbIn & 0xFF0000) >> 16) + ((rgbIn & 0x00FF00) >> 8) + (rgbIn & 0x0000FF)) / 765f // it's gray anyway
//              bufRsmpIn(t) = vIn
//              t += 1
//            }
//            resample.process(src = bufRsmpIn, srcOff = winH, dest = bufRsmpOut, destOff = 0, length = 2, factor = 2)
//            var off = 0
//            while (off < 2) {
//              // note: gain factor 2 here!
//              val vOut    = (math.max(0f, math.min(1f, bufRsmpOut(off) * 2)) * 255 + 0.5f).toInt
//              val rgbOut  = 0xFF000000 | (vOut << 16) | (vOut << 8) | vOut
//              imgRsmp(off).setRGB(x, y, rgbOut)
//              off += 1
//            }
//            x += 1
//          }
//          y += 1
//        }
//      }
//
//      var frameIn  = resampleWindow - winH
//      var frameOut = 0
//      while (frameOut < numOutFrames) {
//        val fOut1 = mkFOut(frameOut + 1)
//        val fOut2 = mkFOut(frameOut + 2)
//
//        if (!fOut1.exists() || !fOut2.exists()) {
//          performResample()
//          var off = 0
//          while (off < 2) {
//            val imgCrop   = imgRsmp(off)
//            val imgUp     = mkResize(config, imgCrop)
//            val imgNoise  = mkNoise(config, imgUp)
//            mkThresh(config, imgNoise, imgOut)
//            ImageIO.write(imgOut, "png", if (off == 0) fOut1 else fOut2)
//            off += 1
//          }
//        }
//
//        // handle overlap
//        System.arraycopy(frameWindow, 1, frameWindow, 0, resampleWindow - 1)
//        if (frameIn < numInFrames) {
//          frameWindow(resampleWindow - 1) = readFrame(config, frameSeq(frameIn) /* frameIn * frameInMul + frameOff */)
//        }
//
//        frameIn  += 1
//        frameOut += 2
//        progress = frameIn.toDouble / numInFrames
//        checkAborted()
//      }
    }

//    println("_" * 33)
//    p.monitor(printResult = false)
//
//    waitForProcessor(p)
  }
}
