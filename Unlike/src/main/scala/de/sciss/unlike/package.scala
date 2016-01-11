/*
 * package.scala
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

package de.sciss

import java.awt.image.BufferedImage
import javax.swing.UIManager

import de.sciss.desktop.OptionPane
import de.sciss.processor.Processor

import scala.concurrent.{ExecutionContextExecutor, Future, ExecutionContext}
import scala.swing.event.ButtonClicked
import scala.swing.{Button, ProgressBar}
import scala.swing.Swing._
import scala.util.control.NonFatal

package object unlike {
  type Vec[+A]  = scala.collection.immutable.IndexedSeq[A]
  val  Vec      = scala.collection.immutable.IndexedSeq

  def waitForProcessor(p: Processor[Any])(implicit exec: ExecutionContext): Unit = {
    val sync = new AnyRef
    val t = new Thread {
      override def run(): Unit = {
        sync.synchronized(sync.wait())
        Thread.sleep(100)
      }
    }
    t.start()

    p.onComplete {
      case _ => sync.synchronized(sync.notify())
    }
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
}