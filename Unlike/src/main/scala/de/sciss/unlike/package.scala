/*
 *  package.scala
 *  (Unlike)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss

import java.awt.image.BufferedImage
import javax.swing.UIManager

import de.sciss.desktop.OptionPane
import de.sciss.processor.{Processor, ProcessorLike}
import de.sciss.swingplus.CloseOperation

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.swing.Swing._
import scala.swing.event.ButtonClicked
import scala.swing.{Button, Frame, ProgressBar, Swing}
import scala.util.control.NonFatal

package object unlike {
  type Vec[+A]  = scala.collection.immutable.IndexedSeq[A]
  val  Vec      = scala.collection.immutable.IndexedSeq

  def mkBlockTread(): AnyRef = {
    val sync = new AnyRef
    val t = new Thread {
      override def run(): Unit = {
        sync.synchronized(sync.wait())
        Thread.sleep(100)
      }
    }
    t.start()
    sync
  }

  def waitForProcessor(p: ProcessorLike[Any, Any])(implicit exec: ExecutionContext): Unit = {
    val sync = mkBlockTread()
    p.onComplete {
      case _ => sync.synchronized(sync.notify())
    }
  }

  def runAndMonitor(p: ProcessorLike[Any, Any] with Processor.Prepared,
                    exit: Boolean = false, printResult: Boolean = true): Unit = {
    waitForProcessor(p)
    println("_" * 33)
    p.monitor(printResult = printResult)
    if (exit) p.onSuccess {
      case _ =>
        Thread.sleep(200)
        sys.exit()
    }
    p.start()
  }

  def runGUI(block: => Unit): Unit =
    onEDT {
      try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
      } catch {
        case NonFatal(_) => // ignore
      }
      block
    }

  def mkProgressDialog(title: String, p: Processor[Any], tail: Future[Any]): Unit = {
    val ggProg  = new ProgressBar
    val ggAbort = new Button("Abort")
    val opt     = OptionPane(message = ggProg, messageType = OptionPane.Message.Plain, entries = Seq(ggAbort))

    val optPeer = opt.peer
    val dlg = optPeer.createDialog(title)
    ggAbort.listenTo(ggAbort)
    ggAbort.reactions += {
      case ButtonClicked(_) =>
        p.abort()
    }
    tail.onComplete(_ => onEDT(dlg.dispose()))
    tail.onFailure {
      case Processor.Aborted() =>
      case ex => ex.printStackTrace()
    }
    p.addListener {
      case prog @ Processor.Progress(_, _) => onEDT(ggProg.value = prog.toInt)
    }
    dlg.setVisible(true)
  }

  def startAndReportProcessor[A](p: Processor[A] with Processor.Prepared): Processor[A] = {
    p.onFailure {
      case Processor.Aborted() =>
      case ex => ex.printStackTrace()
    }
    p.start()
    p
  }

  def cropImage(src: BufferedImage, x: Int, y: Int, width: Int, height: Int): BufferedImage =
    src.getSubimage(x, y, width, height)

  implicit val executionContext: ExecutionContextExecutor = ExecutionContext.Implicits.global

  implicit class ImageOps(private val i: Image) extends AnyVal {
    def plot(zoom: Double = 1.0, mul: Double = 1.0, add: Double = 0.0, invert: Boolean = false,
             title: String = "Plot"): ImageView = {
      val view = ImageView(i)
      import swingplus.Implicits._
      Swing.onEDT {
        view.zoom   = zoom
        view.mul    = mul
        view.add    = add
        view.invert = invert

        val f = new Frame {
          contents  = view.component
          this.defaultCloseOperation = CloseOperation.Dispose
        }
        f.title = title
        f.pack().centerOnScreen()
        f.open()
      }
      view
    }
  }
}