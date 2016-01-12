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
import java.awt.{Color, Paint, TexturePaint}
import java.io.{FileInputStream, FileOutputStream}
import javax.imageio.ImageIO
import javax.swing.KeyStroke

import com.jhlabs.image.NoiseFilter
import com.mortennobel.imagescaling.ResampleOp
import de.sciss.desktop.{FileDialog, OptionPane}
import de.sciss.file._
import de.sciss.guiflitz.AutoView
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.swingplus.Implicits._
import de.sciss.swingplus.{CloseOperation, ComboBox, ListView}

import scala.collection.mutable
import scala.concurrent.blocking
import scala.swing.Swing._
import scala.swing.event.SelectionChanged
import scala.swing.{ToggleButton, Action, BorderPanel, BoxPanel, Button, Component, FlowPanel, Frame, Graphics2D, Label, Menu, MenuBar, MenuItem, Orientation, Rectangle, ScrollPane}

object Unlike {
  private val fBase = userHome / "Pictures" /"2015"/"12"/"07"

  def mkFrame(): Unit = {
//    val hAxis1  = new Axis(Orientation.Horizontal)
//    val vAxis1  = new Axis(Orientation.Vertical  )
//    val hAxis2  = new Axis(Orientation.Horizontal)
//    val vAxis2  = new Axis(Orientation.Vertical  )

    // val mFrame  = new SpinnerNumberModel(1, 1, 3297, 1)
    // val ggFrame = new Spinner(mFrame)

    var zoomFactor  = 0.1

    var img     : BufferedImage = null  // updated in `zoom`

    val avCfg   = AutoView.Config()
    avCfg.small = true

    val frameCfg0   = Situation(index = 9227)
    val globalCfg0  = GlobalConfig()

    val frameCfgView    = AutoView(frameCfg0 , avCfg)
    val globalCfgView   = AutoView(globalCfg0, avCfg)

    def mkFrameConfig (): Situation    = frameCfgView .value
    def mkGlobalConfig(): GlobalConfig = globalCfgView.value

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

    // the one for which `updateImage` has been rendered
    var imageFrameConfig = mkFrameConfig()

    val pntChecker: Paint = {
      val sizeH = 32
      val img = new BufferedImage(sizeH << 1, sizeH << 1, BufferedImage.TYPE_INT_ARGB)

      for (x <- 0 until img.getWidth) {
        for (y <- 0 until img.getHeight) {
          img.setRGB(x, y, if (((x / sizeH) ^ (y / sizeH)) == 0) 0xFF9F9F9F else 0xFF7F7F7F)
        }
      }

      new TexturePaint(img, new Rectangle(0, 0, img.getWidth, img.getHeight))
    }

    var diff = Option.empty[(Situation, BufferedImage)]

    val comp: Component = new Component {
      opaque = true
      override protected def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        val atOrig = g.getTransform
        if (zoomFactor != 1.0) g.scale(zoomFactor, zoomFactor)
        g.setPaint(pntChecker)
        g.fillRect(0, 0, math.ceil(peer.getWidth / zoomFactor).toInt, math.ceil(peer.getHeight / zoomFactor).toInt)
        diff.foreach { case (sitD, imgD) =>
          val scaleD = sitD.scale * 0.01
          val atImgD = AffineTransform.getScaleInstance(scaleD, scaleD)
          atImgD.translate(sitD.translate.x, sitD.translate.y)
          g.drawRenderedImage(imgD, atImgD)
          g.setXORMode(Color.white)
        }
        val scaleI = imageFrameConfig.scale * 0.01
        val atImg = AffineTransform.getScaleInstance(scaleI, scaleI)
        // atImg.rotate()
        atImg.translate(imageFrameConfig.translate.x, imageFrameConfig.translate.y)
        g.drawRenderedImage(img, atImg)
        g.setPaintMode()
        g.setTransform(atOrig)
      }
    }

    def zoom(factor: Double): Unit = {
      zoomFactor  = factor
      val global  = mkGlobalConfig()
      val iw      = (global.width  * factor + 0.5).toInt
      val ih      = (global.height * factor + 0.5).toInt
      // img         = new BufferedImage(iw, ih, BufferedImage.TYPE_INT_ARGB)
      comp.preferredSize = (iw, ih)
      comp.peer.setSize   ((iw, ih))
      // updateImage()
      comp.repaint()
    }

    def updateZoom(): Unit = zoom(zoomFactor)

    def updateImage(): Unit = {
      val newFrameConfig  = mkFrameConfig()
      img                 = mkImage(mkGlobalConfig(), newFrameConfig)
      imageFrameConfig    = newFrameConfig
      comp.repaint()
    }

    updateImage()
    updateZoom()

    globalCfgView.cell.addListener {
      case _ =>
        updateImage()
        updateZoom()
    }

    frameCfgView.cell.addListener {
      case upd =>
        if (upd.index != imageFrameConfig.index) {
          updateImage()
        } else {
          imageFrameConfig = upd
          comp.repaint()
        }
    }

    // mFrame.addChangeListener(ChangeListener(_ => updateImage()))

    val bp = new ScrollPane(comp)
    bp.preferredSize = bp.preferredSize   // fix

    def setSituation(sit: Situation): Unit = {
      frameCfgView.cell() = sit // .config
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
              ???
//              val jsn = Json.parse(new String(arr, "UTF-8"))
//              val res = Json.fromJson[Config](jsn).get
//              cfgView.cell() = res // .lya
            }
          }
        })

        contents += new MenuItem(new Action("Export Image...") {
          accelerator = Some(KeyStroke.getKeyStroke("ctrl S"))
          def apply(): Unit = {
            val sit = mkFrameConfig()
            FileDialog.save(init = Some(userHome / s"collat_${sit.hashCode.toHexString}.png")).show(None).foreach { f =>
              ???
//              val pFull = renderImage(sit, frame = mFrame.getNumber.intValue(), f = f.replaceExt("png"))
//              val futTail = pFull.map { _ =>
//                ???
////                val json = Config.format.writes(sit).toString()
////                blocking {
////                  val jsonOut = new FileOutputStream(f.replaceExt("json"))
////                  jsonOut.write(json.getBytes("UTF-8"))
////                  jsonOut.close()
////                }
//              }
//              mkProgressDialog("Exporting...", pFull, futTail)
            }
          }
        })

        contents += new MenuItem(new Action("Export Image Sequence...") {
          accelerator = Some(KeyStroke.getKeyStroke("ctrl shift S"))

          def apply(): Unit = {
            val cfg = mkFrameConfig()
            val initName = cfg.hashCode.toHexString
            FileDialog.save(init = Some(userHome / s"collat_$initName.png")).show(None).foreach { f =>
              ???
//              val pFull = renderImageSequence(cfg, fOut = f)
//              mkProgressDialog("Exporting...", pFull, pFull)
            }
          }
        })
      }
    }

    lazy val mSnapshots: ListView.Model[Situation] with mutable.Buffer[Situation] = ListView.Model.empty
    lazy val ggSnapshots: ListView[Situation] = new ListView(mSnapshots)
    ggSnapshots.visibleRowCount = 6
    lazy val ggAddSnapshot: Button = Button("Add") {
      val sit   = mkFrameConfig()
      val i0    = mSnapshots.indexWhere(_.index >= sit.index)
      val i     = if (i0 < 0) mSnapshots.size else i0
      mSnapshots.insert(i, sit)
    }
    ggSnapshots.background = Color.darkGray
    ggSnapshots.foreground = Color.white
    lazy val ggMoveSnapshotUp: Button = Button("Up") {
      ggSnapshots.selection.indices.toList.filter(_ > 0).sorted.foreach { row =>
        val s = mSnapshots.remove(row)
        mSnapshots.insert(row - 1, s)
      }
    }
    lazy val ggMoveSnapshotDown: Button = Button("Down") {
      val szM = mSnapshots.size - 1
      ggSnapshots.selection.indices.toList.filter(_ < szM).sorted.reverse.foreach { row =>
        val s = mSnapshots.remove(row)
        mSnapshots.insert(row + 1, s)
      }
    }
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

    lazy val ggDiff: ToggleButton = new ToggleButton(null) {
      action = Action("Diff") {
        if (selected) {
          ggSnapshots.selection.indices.headOption.fold[Unit] {
            selected = false
          } { row =>
            val sit  = mSnapshots(row)
            val img1 = mkImage(mkGlobalConfig(), sit)
            diff     = Some((sit, img1))
            comp.repaint()
          }
        } else {
          diff = None
          comp.repaint()
        }
      }
    }

    lazy val pSnapshots: Component = new ScrollPane(ggSnapshots)

    lazy val ggZoom: ComboBox[Int] = new ComboBox(10 to 100 by 10) {
      listenTo(selection)
      reactions += {
        case SelectionChanged(_) => zoom(selection.item * 0.01)
      }
    }

    lazy val pBottom: Component = new BoxPanel(Orientation.Vertical) {
      contents += new FlowPanel(
        new Label("Zoom:"), ggZoom, HStrut(16),
        new Label("Key Frames:"), ggAddSnapshot, ggMoveSnapshotUp, ggMoveSnapshotDown,
        ggRecallSnapshot, HStrut(16), ggRemoveSnapshot, HStrut(16),
        ggDiff
      )
    }

    lazy val pRight: BoxPanel = new BoxPanel(Orientation.Vertical) {
      contents += VStrut(16)  // will be replaced
      contents += globalCfgView .component
      contents += frameCfgView  .component
      // contents += ggText
    }

    val split = new BorderPanel {
      add(bp        , BorderPanel.Position.North )
      add(pSnapshots, BorderPanel.Position.Center)
      add(pBottom   , BorderPanel.Position.South )
    }

    val fr = new Frame { self =>
      title = "Unlike"
      contents  = new BorderPanel {
        add(split , BorderPanel.Position.Center)
        add(pRight, BorderPanel.Position.East)
      }
      resizable = false
      menuBar = mb
      pack().centerOnScreen()
      open()
    }
    fr.defaultCloseOperation = CloseOperation.Exit

    // ggRender.doClick()
    // updateImage()
  }

//  def renderImage(config: Config, frame: Int, f: File): Processor[Unit] = {
//    val res = new RenderImage(config, frame = frame, f = f)
//    res.start()
//    res
//  }

//  private final class RenderImage(config: Config, frame: Int, f: File)
//    extends ProcessorImpl[Unit, Processor[Unit]] with Processor[Unit] {
//
//    protected def body(): Unit = blocking {
//      val fOut  = f.replaceExt("png")
//      if (!fOut.exists()) {
//        val img = mkImage(config, index = frame)
//        ImageIO.write(img, "png", fOut)
//      }
//      progress = 1.0
//    }
//  }

//  def renderImageSequence(config: Config, fOut: File): Processor[Unit] = {
//    ???
////    import config.resampleWindow
////    require(resampleWindow % 2 == 1, s"resampleWindow ($resampleWindow) must be odd")
////    val res = new RenderImageSequence(config = config, fOut = fOut)
////    res.start()
////    res
//  }

  def mkImage(global: GlobalConfig, sit: Situation): BufferedImage = {
    val imgCrop   = readFrame(sit.index)
    val imgUp     = mkResize (global, imgCrop)
    val imgNoise  = mkNoise  (global, imgUp  )
    imgNoise // mkThresh(config, imgNoise)
  }

//  def cropImage2(config: Config, in: BufferedImage): BufferedImage = {
////    import config.sizeIn
////    cropImage(in, 145 + (430 - sizeIn)/2, 20 + (430 - sizeIn)/2, sizeIn, sizeIn)
//  }

  def mkFIn(index: Int): File = fBase / s"DSC_$index.JPG"

  def readFrame(index: Int): BufferedImage = {
    val fIn1      = mkFIn(index)
    val imgIn     = ImageIO.read(fIn1)
    imgIn
//    val imgCrop   = cropImage2(config, imgIn)
//    imgCrop
  }
  
  def mkResize(global: GlobalConfig, in: BufferedImage): BufferedImage = {
    val resizeOp  = new ResampleOp(global.width, global.height)
    resizeOp.filter(in, null)
  }
  
  def mkNoise(global: GlobalConfig, in: BufferedImage): BufferedImage = if (global.noise <= 0) in else {
    val noiseOp = new NoiseFilter
    noiseOp.setAmount(global.noise)
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

  private final class RenderImageSequence(global: GlobalConfig, fOut: File)
    extends ProcessorImpl[Unit, RenderImageSequence] with Processor[Unit] {

    protected def body(): Unit = blocking {
      val jsonF = fOut.replaceExt("json")
      if (!jsonF.exists()) blocking {
        val json    = GlobalConfig.format.writes(global).toString()
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
