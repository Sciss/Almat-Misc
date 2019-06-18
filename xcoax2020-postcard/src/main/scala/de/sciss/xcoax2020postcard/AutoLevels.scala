package de.sciss.xcoax2020postcard

import de.sciss.file._
import de.sciss.fscape.{GE, Graph, graph}
import de.sciss.fscape.stream.Control

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object AutoLevels {
  case class Config(
                     fIn: File, fOut: File, f1N: Double
                   )

  def any2stringadd: Any = ()

  def main(args: Array[String]): Unit = {
    for (idx <- 5 to 5) {
      val tempIn  = file("/data/projects/Almat/events/xcoax2020/postcard/centered/outc%04d.png")
      val tempOut = file("/data/projects/Almat/events/xcoax2020/postcard/composed/outcc%04d.png")
      val fIn     = formatTemplate(tempIn , idx)
      val fOut    = formatTemplate(tempOut, idx)
//      require (fIn.exists() && !fOut.exists())

      println(s":::::::: compose $idx ::::::::")
      val fut = run(Config(fIn = fIn, fOut = fOut, f1N = 0.02))
      Await.result(fut, Duration.Inf)
    }
  }

  def formatTemplate(temp: File, idx: Int): File = {
    temp.parent / temp.name.format(idx)
  }

  def getAutoLevels(in: GE, frameSize: GE): (GE, GE) = {
    import graph._
    // this is roughly the behaviour of GIMP auto levels
    val low   = SlidingPercentile(in, len = frameSize, frac = 0.006).last
    val high  = SlidingPercentile(in, len = frameSize, frac = 0.990).last
    (low, high)
  }

  def run(c: Config): Future[Unit] = {
    import c._

    val cfg       = Control.Config()
    cfg.useAsync  = false
    cfg.blockSize = 2048 // 512 // 2048

    val g = Graph {
      import graph._

      val width   = 3744  // 3732 // 3488 // 2048 // 3496 // 512
      val height  = 2720  // 2464 // 2048 // 2480 // 512
      val frameSize = width * height
      val i1: GE  = ImageFileIn(fIn, numChannels = 1).take(frameSize)

      val (low, high) = getAutoLevels(i1, frameSize)

      (low  * 255).roundTo(1).poll(0, " 1%")
      (high * 255).roundTo(1).poll(0, "99%")
    }

    val ctl = Control(cfg)
    ctl.run(g)
    ctl.status
  }
}
