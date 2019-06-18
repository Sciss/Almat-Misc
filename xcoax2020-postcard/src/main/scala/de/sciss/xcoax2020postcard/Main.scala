/*
 *  Main.scala
 *  (xCoAx2020-Postcard)
 *
 *  Copyright (c) 2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.xcoax2020postcard

import java.awt.geom.Line2D
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, RenderingHints}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream, FileInputStream, FileOutputStream, InputStream, OutputStream}

import de.sciss.file._
import de.sciss.fscape.{Graph, stream}
import de.sciss.neuralgas
import de.sciss.neuralgas.{ComputeGNG, GrayImagePD}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import javax.imageio.ImageIO

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main {
  case class Config(imgInF        : File    = file("in.png"),
                    imgOutF       : File    = file("out.png"),
                    invert        : Boolean = false,
                    invertOut     : Boolean = false,
                    strokeWidth   : Double  = 2.0,
                    rngSeed       : Int     = 0xBEE,
                    maxNodesDecim : Int     = 108,
                    maxNodes      : Int     = 0,
                    gngStepSize   : Int     = 27,
                    gngLambda     : Int     = 27,
                    gngEdgeAge    : Int     = 108,
                    gngEpsilon    : Double  = 0.05,
                    gngEpsilon2   : Double  = 1.0e-4,
                    gngAlpha      : Double  = 0.2,
                    gngBeta       : Double  = 5.0e-6,
                    gngUtility    : Double  = 18.0,
                    gngMaxSignals : Int     = 0,
                    interim       : Int     = 1000,
                    interimImages : Boolean = false,
                    widthOut      : Int     = 0,
                    heightOut     : Int     = 0,
                    decay         : Double  = 0.9999,
                    verbose       : Boolean = false
                   ) {

    lazy val fGNG: File = imgOutF.replaceExt("gng")

    def mkGNGIteration(i: Int): File = {
      val name = s"${fGNG.base}-$i.${fGNG.ext}"
      fGNG.replaceName(name)
    }
  }

  val bla = Config(
    imgInF      = file("/data/projects/Almat/events/xcoax2020/postcard/WacomTest.png"),
    imgOutF     = file("/data/projects/Almat/events/xcoax2020/postcard/render/out.png"),
    invert      = true,
    gngEpsilon  = 0.05,
    gngEpsilon2 = 0.05,
    gngAlpha    = 0.1,
    gngBeta     = 1.0e-5,
//    interim     = 0,
    interimImages = true,
    widthOut    = 1480 * 4,
    heightOut   = 1050 * 4,
    strokeWidth = 2.0   // N.B. this is already scaled by using widthOut and heightOut! 3.4
  )

  val selected = Config(
//    imgInF      = file("/data/projects/Almat/events/xcoax2020/postcard/WacomTest.png"),
    imgInF      = file("/data/projects/Almat/events/xcoax2020/postcard/WacomTest2.png"),
    imgOutF     = file("/data/projects/Almat/events/xcoax2020/postcard/render/out.png"),
    invert      = true,
    gngLambda   = 200,
    gngEdgeAge  = 88,
    gngEpsilon  = 0.05,
    gngEpsilon2 = 0.05,
    gngAlpha    = 0.1, // 0.0,
    gngBeta     = 1.0e-5,
    gngUtility  = 4.0,
    maxNodes    = 600,
    gngMaxSignals  = 10000000,
    gngStepSize = 200, // 500,
    interim     = 50,
    interimImages = true,
    widthOut    = 3488, // 3496, // 1480 * 4,
    rngSeed     = 3, // 0,
    heightOut   = 2464, // 2480, // 1050 * 4,
    strokeWidth = 1.0,
    decay       = 0.99,
    invertOut   = true,
//    verbose     = true
  )

  //  val examples: Vector[Config] = Vector(ex1, ex2, ex3)

  def main(args: Array[String]): Unit = {
//    val default = Config()
//    val p = new scopt.OptionParser[Config]("Schwaermen-Catalog-Cover") {
//      opt[File]('i', "input")
//        //        .required()
//        .text (s"Image input file ${default.imgInF})")
//        .action { (f, c) => c.copy(imgInF = f) }
//
//      opt[File]('o', "output")
//        //        .required()
//        .text (s"Image output file, should end in '.png' or '.jpg' ${default.imgOutF})")
//        .action { (f, c) => c.copy(imgOutF = f) }
//
//      opt[Unit] ("invert")
//        .text ("Invert gray scale probabilities.")
//        .action { (_, c) => c.copy(invert = true) }
//
//      opt[Double] ("stroke")
//        .text (s"Stroke width in pixels (default ${default.strokeWidth})")
//        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(strokeWidth = v) }
//
//      opt[Int] ("seed")
//        .text (s"Random number generator seed (default ${default.rngSeed})")
//        .action { (v, c) => c.copy(rngSeed = v) }
//
//      opt[Int] ('n', "max-nodes")
//        .text ("Maximum number of nodes (zero for threshold based)")
//        .validate(i => if (i >= 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(maxNodes = v) }
//
//      opt[Int] ("decim")
//        .text (s"Pixel decimation to determine maximum number of nodes (default ${default.maxNodesDecim})")
//        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(maxNodesDecim = v) }
//
//      opt[Int] ("step")
//        .text (s"GNG step size (default ${default.gngStepSize})")
//        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(gngStepSize = v) }
//
//      opt[Int] ("lambda")
//        .text (s"GNG lambda parameter (default ${default.gngLambda})")
//        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(gngLambda = v) }
//
//      opt[Int] ("edge-age")
//        .text (s"GNG maximum edge age (default ${default.gngEdgeAge})")
//        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(gngEdgeAge = v) }
//
//      opt[Double] ("eps")
//        .text (s"GNG epsilon parameter (default ${default.gngEpsilon})")
//        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(gngEpsilon = v) }
//
//      opt[Double] ("eps2")
//        .text (s"GNG epsilon 2 parameter (default ${default.gngEpsilon2})")
//        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(gngEpsilon2 = v) }
//
//      opt[Double] ("alpha")
//        .text (s"GNG alpha parameter (default ${default.gngAlpha})")
//        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(gngAlpha = v) }
//
//      opt[Double] ("beta")
//        .text (s"GNG beta parameter (default ${default.gngBeta})")
//        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(gngBeta = v) }
//
//      opt[Double] ("utility")
//        .text (s"GNG-U utility parameter (default ${default.gngUtility})")
//        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
//        .action { (v, c) => c.copy(gngUtility = v) }
//
//      opt[Int] ('t', "interim")
//        .text (s"Interim file output, every n steps, or zero to turn off (default ${default.interim})")
//        .validate(i => if (i >= 0) Right(()) else Left("Must be >= 0") )
//        .action { (v, c) => c.copy(interim = v) }
//
//      opt[Unit] ("interim-images")
//        .text ("Generate images for interim files.")
//        .action { (_, c) => c.copy(interimImages = true) }
//
//      opt[Int] ("ex")
//        .text (s"Use example <n> parameters (1 to ${examples.size}")
//        .validate(i => if (i >= 1 && i <= examples.size) Right(()) else Left(s"Must be >= 1 and <= ${examples.size}") )
//        .action { (i, _) => examples(i - 1) }
//
//      opt[Unit] ("selected")
//        .text ("Use the parameters selected for the catalog cover.")
//        .action { (_, _) => selected }
//
//      opt[Int] ('w', "width")
//        .text (s"Rendering output width in pixels, or zero to match input.")
//        .validate(i => if (i >= 0) Right(()) else Left("Must be >= 0") )
//        .action { (v, c) => c.copy(widthOut = v) }
//
//      opt[Int] ('h', "height")
//        .text (s"Rendering output height in pixels, or zero to match input.")
//        .validate(i => if (i >= 0) Right(()) else Left("Must be >= 0") )
//        .action { (v, c) => c.copy(heightOut = v) }
//    }
//    p.parse(args, default).fold(sys.exit(1)) { config =>

    for (idx <- 39 to 250) {
      println(s":::::::: idx = $idx ::::::::")
      val config = selected.copy(
        imgInF        = file(f"/data/projects/Almat/events/xcoax2020/postcard/templates/template$idx%04d.png"),
        imgOutF       = file(f"/data/projects/Almat/events/xcoax2020/postcard/render/out$idx%04d.png"),
        rngSeed       = idx,
        interimImages = false
      )
      run(config)
    }
  }

  def writeAccImage(acc: Array[Double], fOut: File, w: Int, h: Int, invertOut: Boolean): Unit = {
    val tmp   = File.createTemp(suffix = ".aif")
    try {
      val afOut = AudioFile.openWrite(tmp, AudioFileSpec(numChannels = 1, sampleRate = 44100.0))
      val gain = try {
        val bufF  = afOut.buffer(acc.length)
        val buf0  = bufF(0)
        var max   = 0.0
        var i = 0
        while (i < acc.length) {
          val v = acc(i)
          buf0(i) = v.toFloat
          if (v > max) max = v
          i += 1
        }
        afOut.write(bufF)
        if (max > 0.0) 1.0 / max else 1.0

      } finally {
        afOut.close()
      }

      val g = Graph {
        import de.sciss.fscape.graph._
        val in  = AudioFileIn(tmp, numChannels = 1)
        val n0  = in * gain
        val n   = if (invertOut) 1.0 - n0 else n0
        ImageFileOut(n, fOut, ImageFile.Spec(width = w, height = h, numChannels = 1,
          sampleFormat = ImageFile.SampleFormat.Int16))
      }
      val config = stream.Control.Config()
      config.useAsync   = false
      val ctrl = stream.Control(config)
      ctrl.run(g)
      Await.result(ctrl.status, Duration.Inf)

    } finally {
      tmp.delete()
    }
  }

  def renderImage(config: Config, quiet: Boolean = false)
                 (fGNG: File = config.fGNG, fImgOut: File = config.imgOutF): Unit = {
    val graph = readGNG(fGNG)
    val wIn   = graph.surfaceWidthPx
    val hIn   = graph.surfaceHeightPx
    val wOut  = if (config.widthOut  == 0) wIn else config.widthOut
    val hOut  = if (config.heightOut == 0) hIn else config.heightOut
    val sx    = wOut.toDouble / wIn
    val sy    = hOut.toDouble / hIn
    val img   = new BufferedImage(wOut, hOut, BufferedImage.TYPE_BYTE_GRAY)
    val g     = img.createGraphics()
    g.setColor(Color.white)
    g.fillRect(0, 0, wOut, hOut)
    if (sx != 1.0 || sy != 1.0) g.scale(sx, sy)
    g.setColor(Color.black)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON   )
    g.setRenderingHint(RenderingHints.KEY_RENDERING     , RenderingHints.VALUE_RENDER_QUALITY )
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE    )
    g.setStroke(new BasicStroke(config.strokeWidth.toFloat))
    val ln = new Line2D.Double()
    graph.edges.foreach { case Edge(from, to) =>
      val nFrom = graph.nodes(from)
      val nTo   = graph.nodes(to  )
      ln.setLine(nFrom.x, nFrom.y, nTo.x, nTo.y)
      g.draw(ln)
    }
    g.dispose()
    val fmt   = if (fImgOut.extL == "png") "png" else "jpg"
    ImageIO.write(img, fmt, fImgOut)
    if (!quiet) println(s"Wrote ${fImgOut.name}")
  }

  def renderImageTo(graph: ResGNG, img: BufferedImage, config: Config): Unit = {
    val wIn   = graph.surfaceWidthPx
    val hIn   = graph.surfaceHeightPx
    val wOut  = if (config.widthOut  == 0) wIn else config.widthOut
    val hOut  = if (config.heightOut == 0) hIn else config.heightOut
    val sx    = wOut.toDouble / wIn
    val sy    = hOut.toDouble / hIn
//    val img   = new BufferedImage(wOut, hOut, BufferedImage.TYPE_BYTE_GRAY)
    val g     = img.createGraphics()
    g.setColor(Color.black)
    g.fillRect(0, 0, wOut, hOut)
    if (sx != 1.0 || sy != 1.0) g.scale(sx, sy)
    g.setColor(Color.white)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON   )
    g.setRenderingHint(RenderingHints.KEY_RENDERING     , RenderingHints.VALUE_RENDER_QUALITY )
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE    )
    g.setStroke(new BasicStroke(config.strokeWidth.toFloat))
    val ln = new Line2D.Double()
    graph.edges.foreach { case Edge(from, to) =>
      val nFrom = graph.nodes(from)
      val nTo   = graph.nodes(to  )
      ln.setLine(nFrom.x, nFrom.y, nTo.x, nTo.y)
      g.draw(ln)
    }
    g.dispose()
  }

  case class Point2D(x: Float, y: Float)
  case class Edge(from: Int, to: Int)

  case class ResGNG(surfaceWidthPx: Int, surfaceHeightPx: Int, nodes: Vec[Point2D],
                    edges: Vec[Edge])

  def readGNG(fGNG: File): ResGNG = {
    val fis = new FileInputStream(fGNG)
    try {
      readGNGFrom(fis)
    } finally {
      fis.close()
    }
  }

  def readGNGFrom(is: InputStream): ResGNG = {
    val dis = new DataInputStream(is)
    require (dis.readInt() == GNG_COOKIE)
    val surfaceWidthPx  = dis.readShort()
    val surfaceHeightPx = dis.readShort()
    val nNodes = dis.readInt()
    val nodes = Vector.fill(nNodes) {
      val x = dis.readFloat()
      val y = dis.readFloat()
      Point2D(x, y)
    }
    val nEdges = dis.readInt()
    val edges = Vector.fill(nEdges) {
      val from = dis.readInt()
      val to   = dis.readInt()
      Edge(from, to)
    }
    ResGNG(surfaceWidthPx, surfaceHeightPx, nodes, edges)
  }

  def accumulateImage(in: BufferedImage, out: Array[Double], gain: Double): Unit = {
    val w = in.getWidth
    val h = in.getHeight
    var i = 0
    var y = 0
    while (y < h) {
      var x = 0
      while (x < w) {
        val rgb   = in.getRGB(x, y)
        val value = ((rgb & 0xFF0000) >> 16) / 255.0  // we assume gray scale input
        out(i) += value * gain
        i += 1
        x += 1
      }
      y += 1
    }
  }

  def run(config: Config): Unit = {
    import config._
    val img       = ImageIO.read(imgInF)
    val c         = new ComputeGNG
    val pd        = new GrayImagePD(img, invert)
    c.pd          = pd
    val wIn       = img.getWidth
    val hIn       = img.getHeight
    c.panelWidth  = wIn // / 8
    c.panelHeight = hIn // / 8
    c.maxNodes    = if (maxNodes > 0) maxNodes else pd.getNumPixels / maxNodesDecim
    println(s"wIn ${c.panelWidth}, hIn ${c.panelHeight}, maxNodes ${c.maxNodes}")
    c.stepSize    = gngStepSize
    c.algorithm   = neuralgas.Algorithm.GNGU
    c.lambdaGNG   = gngLambda
    c.maxEdgeAge  = gngEdgeAge
    c.epsilonGNG  = gngEpsilon  .toFloat
    c.epsilonGNG2 = gngEpsilon2 .toFloat
    c.alphaGNG    = gngAlpha    .toFloat
    c.setBetaGNG(gngBeta.toFloat)
    c.noNewNodesGNGB = false
    c.GNG_U_B     = true
    c.utilityGNG  = gngUtility  .toFloat
    c.autoStopB   = false
    c.reset()
    //    c.getRNG.setSeed(108L)
    var rngSeed1 = rngSeed
    c.getRNG.setSeed(rngSeed1)
    c.addNode(null)
    c.addNode(null)

    val wOut  = if (config.widthOut  == 0) wIn else config.widthOut
    val hOut  = if (config.heightOut == 0) hIn else config.heightOut

    val imgAcc  = new BufferedImage(wOut, hOut, BufferedImage.TYPE_BYTE_GRAY)
    val arrAcc  = new Array[Double](wOut * hOut)

//    val interimF      = if (interim != 0) interim else 1000

    var hasGrowth     = false

    val res           = new ComputeGNG.Result
    var lastProgress  = 0
    var iteration     = 0
    var lastN         = -1
    var lastE         = -1
    val t0            = System.currentTimeMillis()
    println("_" * 100)
    while (!res.stop && c.nNodes < c.maxNodes) {
      c.learn(res)
      val progress = (c.nNodes * 100) / c.maxNodes
      if (lastProgress < progress) while (lastProgress < progress) {
        print('#')
        lastProgress += 1
      }
      val decayIt = math.pow(decay, iteration)
      iteration += 1
      if (verbose) {
        println(s"it $iteration, signals ${c.numSignals}, nodes ${c.nNodes}, edges ${c.nEdges}")
      }
      val os = new ByteArrayOutputStream()
      writeGNGTo(c, os, w = wIn, h = hIn)
      val resGNG = readGNGFrom(new ByteArrayInputStream(os.toByteArray))
      renderImageTo(resGNG, imgAcc, config)
      accumulateImage(imgAcc, arrAcc, gain = decayIt)

      if (interimImages && interim > 0 && (iteration % interim == 0)) {
        val fTemp     = mkGNGIteration(iteration)
        val fTempImg  = fTemp.replaceExt(imgOutF.ext)
//        renderImage(config, quiet = true)(fGNG = fTemp, fImgOut = fTempImg)
        writeAccImage(arrAcc, fTempImg, w = wOut, h = hOut, invertOut = invertOut)
      }

      if (gngMaxSignals > 0 && c.numSignals >= gngMaxSignals /* || c.nNodes <= lastN && c.nEdges <= lastE */) {
        res.stop = true
      } else {
        lastN = c.nNodes
        lastE = c.nEdges
      }

      if (!hasGrowth) {
        if (iteration == 15) {
          rngSeed1 += 1000
          println(s"No growth. Adjusting seed to $rngSeed1")
          println("_" * 100)
          c.reset()
          c.getRNG.setSeed(rngSeed1)
          c.addNode(null)
          c.addNode(null)
          iteration = 0
        } else if (c.nNodes > 4 && c.nEdges > 2) {
          hasGrowth = true
        }
      }
    }

    println(s" Done GNG. ${c.numSignals} signals, took ${(System.currentTimeMillis() - t0) / 1000} sec.")
//    writeGNG(c, fGNG, w = wIn, h = hIn)
    writeAccImage(arrAcc, imgOutF, w = wOut, h = hOut, invertOut = invertOut)
  }

  final val GNG_COOKIE = 0x474E470   // "GNG\0" -- eh, not really :)

  def writeGNG(c: ComputeGNG, fOut: File, w: Int, h: Int): Unit = {
    val fos = new FileOutputStream(fOut)
    try {
      writeGNGTo(c, fos, w = w, h = h)
    } finally fos.close()
  }

  def writeGNGTo(c: ComputeGNG, os: OutputStream, w: Int, h: Int): Unit = {
    val dos = new DataOutputStream(os)
    dos.writeInt(GNG_COOKIE)
    dos.writeShort(w)
    dos.writeShort(h)
    dos.writeInt(c.nNodes)
    for (i <- 0 until c.nNodes) {
      val n = c.nodes(i)
      dos.writeFloat(n.x)
      dos.writeFloat(n.y)
    }
    dos.writeInt(c.nEdges)
    for (i <- 0 until c.nEdges) {
      val e = c.edges(i)
      dos.writeInt(e.from)
      dos.writeInt(e.to  )
    }
    dos.flush()
  }
}
