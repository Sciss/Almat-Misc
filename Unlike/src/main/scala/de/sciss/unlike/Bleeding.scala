/*
 *  Bleeding.scala
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

package de.sciss.unlike

import java.awt.image.BufferedImage

import com.jhlabs.image.{AbstractBufferedImageOp, EmbossFilter}
import de.sciss.file._
import de.sciss.numbers
import de.sciss.unlike.PhaseCorrelation.{Product => Frame}

import scala.annotation.switch
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, blocking}

object Bleeding extends App {
  lazy val mode             = "WRITE"   // either "WRITE" or "ANALYZE" or "BOTH", or "VIDEO"

  lazy val base             = userHome / "Documents" / "projects" / "Anemone" / "minuten"
  lazy val startFrame       = 1
  lazy val endFrame         = 3378
  lazy val inputDir         = base / "bleeding_in"
  lazy val jsonDir          = base / "bleeding_json"
  lazy val renderDir        = base / "bleeding_out"
  lazy val videoDir         = base / "bleeding_vid"
  lazy val inputTemplate    = inputDir  / "bleeding-%d.jpg"
  lazy val jsonTemplate     = jsonDir   / "bleeding-out-%d-%d.json"
  lazy val outputTemplate   = renderDir / "bleeding-out-%d.png" // jpg
  lazy val videoTemplate    = videoDir  / "bleeding-vid-%d.jpg"

  if (!jsonDir  .exists()) jsonDir  .mkdir()
  if (!renderDir.exists()) renderDir.mkdir()
  if (!videoDir .exists()) videoDir .mkdir()

  lazy val c1 = EstimateVideoMotion.Config(
    input     = inputTemplate,
    output    = Some(jsonTemplate),
    frames    = startFrame to endFrame,
    settings  = PhaseCorrelation.Settings(downSample = 2.0)
  )

  require (mode == "ANALYZE" || mode == "WRITE" || mode == "BOTH" || mode == "VIDEO")

  if (mode == "ANALYZE" || mode == "BOTH") {
    val p1 = EstimateVideoMotion(c1)
    println("Analyze adjacent...")
    runAndMonitor(p1, exit = mode != "BOTH", printResult = false)
    val lastProc = p1

    if (mode == "BOTH") Await.result(lastProc, Duration.Inf)
  }

  if (mode == "WRITE" || mode == "BOTH") {

    val framesFut = Future[Vec[(Int, Frame)]](blocking {
      import PhaseCorrelation.{Product => Frame}
      val seq = EstimateVideoMotion.read(c1)
      c1.frames zip (Frame.identity +: seq)
    })

    println("Read JSON...")
    val frames  = Await.result(framesFut, Duration.Inf)

    val input   = c1.input

    //    val fltGamma  = new GammaFilter(0.5f)
    //    val fltNoise  = new NoiseFilter
    //    fltNoise.setAmount(10)

    val renCfg  = RenderVideoMotion.Config(input = input, output = outputTemplate, format = ImageFormat.PNG,
      frames = frames,
      filtersIn  = Filter :: /* fltGamma :: fltNoise :: */ Nil,
      filtersOut = /* fltGamma :: fltNoise :: */ Nil,
      missing = RenderVideoMotion.Missing.Fill(),
      downSample = 1.0)
    val p = RenderVideoMotion(renCfg)
    println("Render...")
    p.onFailure {
      case e => e.printStackTrace()
    }
    runAndMonitor(p, exit = true, printResult = false)
  }

  // `avconv -r 1 -i unlike_vid/unlike-vid-%04d.jpg unlike.mp4`
  if (mode == "VIDEO") {
    val numIn     = (startFrame to endFrame).size
    val framesIn  = 1 to numIn // diff skipFrames
    framesIn.zipWithIndex.foreach { case (frameIn, frameOut0) =>
      val frameOut  = frameOut0 + 1
      val fIn       = outputTemplate.parent / outputTemplate.name.format(frameIn )
      val fOut      = videoTemplate .parent / videoTemplate .name.format(frameOut)
      if (!fOut.exists()) {
        import sys.process._
        val cmd = Seq("ln", "-s", fIn.path, fOut.path)
        val res = cmd.!
        require(res == 0, cmd.mkString("Failed: ", " ", ""))
      }
    }
  }

  implicit final class FloatOps(private val peer: Float) extends AnyVal {
    def degToRad: Float = (peer * math.Pi/180).toFloat
  }

  object Filter extends AbstractBufferedImageOp {
    private[this] val emboss = {
      val res = new EmbossFilter
      // res.setEmboss    (true)
      res.setAzimuth   (30f.degToRad)
      res.setElevation (45f.degToRad)
      res.setBumpHeight(20f)
      res
    }

    def filter(src: BufferedImage, dest: BufferedImage): BufferedImage = {
      // val e   = emboss(in)
      // val c   = in divide e
      // val out = valueInvert(c)

      val e = emboss.filter(src, dest)
      val w = src.getWidth
      val h = src.getHeight
      var y = 0
      while (y < h) {
        var x = 0
        while (x < w) {
          val a   = src.getRGB(x, y)
          val b   = e  .getRGB(x, y)
          val ra  = ((a & 0xFF0000) >> 16) / 255.0
          val rb  = ((b & 0xFF0000) >> 16) / 255.0
          val ga  = ((a & 0x00FF00) >>  8) / 255.0
          val gb  = ((b & 0x00FF00) >>  8) / 255.0
          val ba  =  (a & 0x0000FF)        / 255.0
          val bb  =  (b & 0x0000FF)        / 255.0
          val rc  = math.min(1.0, ra / rb)
          val gc  = math.min(1.0, ga / gb)
          val bc  = math.min(1.0, ba / bb)
//          val rgb = new RGB(rc, gc, bc).toHSB.toRGB.toInt
          val hsb  = RGBtoHSB(r = rc, g = gc, b = bc)
          val rgb0 = HSBtoRGB(h = hsb.h, s = hsb.s, b = 1.0 - hsb.b)
          val rgb  = rgb0.gamma(2.0)
          e.setRGB(x, y, rgb.toInt)
          x += 1
        }
        y += 1
      }
      e
    }

    private final class HSB(val h: Double, val s: Double, val b: Double) {
      def toRGB: RGB = HSBtoRGB(h = h, s = s, b = b)
    }

    private final class RGB(val r: Double, val g: Double, val b: Double) {
      def toHSB: HSB = RGBtoHSB(r = r, g = g, b = b)

      def gamma(v: Double): RGB = {
        val gammaInv = 1.0 / v
        val r1 = math.pow(r, gammaInv)
        val g1 = math.pow(g, gammaInv)
        val b1 = math.pow(b, gammaInv)
        new RGB(r1, g1, b1)
      }

      def toInt: Int = {
        import numbers.Implicits._
        val ri = (r.clip(0, 1) * 0xFF + 0.5).toInt
        val gi = (g.clip(0, 1) * 0xFF + 0.5).toInt
        val bi = (b.clip(0, 1) * 0xFF + 0.5).toInt
        (ri << 16) | (gi << 8) | bi | 0xFF000000
      }
    }

    private def RGBtoHSB(r: Double, g: Double, b: Double): HSB = {
      val compMin = math.min(math.min(r, g), b)
      val compMax = math.max(math.max(r, g), b)
      val bri     = compMax
      val compD   = compMax - compMin
      val sat     = if (compMax == 0) 0.0 else compD / compMax
      val hue     = if (sat     == 0) 0.0 else {
        val rScale = (compMax - r) / compD
        val gScale = (compMax - g) / compD
        val bScale = (compMax - b) / compD
        val x = if (r == compMax) bScale - gScale
        else    if (g == compMax) rScale - bScale + 2.0
        else                      gScale - rScale + 4.0
        val y = x / 6
        if (y >= 0) y else y + 1.0
      }
      new HSB(h = hue, s = sat, b = bri)
    }

    private def HSBtoRGB(h: Double, s: Double, b: Double): RGB = {
      if (s == 0) {
        new RGB(r = b, g = b, b = b)
      } else {
        val hf  = (h - math.floor(h)) * 6.0
        val f   = hf - math.floor(hf)
        val p   = b * (1.0 -  s)
        val q   = b * (1.0 -  s * f)
        val t   = b * (1.0 - (s * (1.0 - f)))
        (hf.toInt: @switch) match {
          case 0 => new RGB(r = b, g = t, b = p)
          case 1 => new RGB(r = q, g = b, b = p)
          case 2 => new RGB(r = p, g = b, b = t)
          case 3 => new RGB(r = p, g = q, b = b)
          case 4 => new RGB(r = t, g = p, b = b)
          case 5 => new RGB(r = b, g = p, b = q)
        }
      }
    }
  }
}