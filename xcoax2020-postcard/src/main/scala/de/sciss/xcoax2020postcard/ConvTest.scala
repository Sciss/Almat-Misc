package de.sciss.xcoax2020postcard

import de.sciss.file._
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{Graph, graph}
import de.sciss.synth.io.AudioFileSpec

import scala.swing.Swing

object ConvTest extends App {
  def any2stringadd: Any = ()

  var gui: SimpleGUI = _
  val cfg       = Control.Config()
  cfg.useAsync  = false
  cfg.progressReporter = p => Swing.onEDT(gui.progress = p.total)
  cfg.blockSize = 2048 // 512 // 2048

  val g = Graph {
    import graph._
    val fIn1    = file("/data/projects/Almat/events/xcoax2020/postcard/render-1/out.png")
    val fIn2    = file("/data/projects/Almat/events/xcoax2020/postcard/render/out.png")
    val baseDir = fIn1.parent.parent
    val fOut    = baseDir / "test.png"
    require (baseDir.canWrite)

    val width   = 3488 // 2048 // 3496 // 512
    val height  = 2464 // 2048 // 2480 // 512
    val frameSize = width * height
    val i1      = -ImageFileIn(fIn1, numChannels = 1).take(frameSize) + 1.0
    val i2      = -ImageFileIn(fIn2, numChannels = 1).take(frameSize) + 1.0
//    val i1      = ImageFileIn(fIn1, numChannels = 1).take(frameSize)
//    val i2      = ImageFileIn(fIn2, numChannels = 1).take(frameSize)
    val kernel  = 32
    val kernelS = kernel * kernel
    val m1      = MatrixInMatrix(i1, rowsOuter = height, columnsOuter = width, rowsInner = kernel, columnsInner = kernel)
    val m2      = MatrixInMatrix(i2, rowsOuter = height, columnsOuter = width, rowsInner = kernel, columnsInner = kernel)
     Length(i1).poll(0, "i1-len")
    //    val p       = Metro(kernelS)
    //    val avg     = RunningSum(m1, p) / kernelS
    //    val flt     = ResizeWindow(avg, size = kernelS, start = kernelS - 1)
    //    Length(flt).poll(0, "flt")

    val m1f       = Real2FFT(m1, rows = kernel, columns = kernel)
    val m2f       = Real2FFT(m2, rows = kernel, columns = kernel)
    val m3f       = m1f.complex * m2f
    val m3        = Real2IFFT(m3f, rows = kernel, columns = kernel)
//    val flt       = ResizeWindow(m3, size = kernelS, start = kernelS - 1)
    val flt       = ResizeWindow(m3, size = kernelS, start = kernelS/2, stop = -(kernelS/2 - 1))
//    i1.poll(Metro(1000), "bla")

//    Progress(Frames(flt) / (2 * frameSize), Metro(width))
    ProgressFrames(flt, frameSize)
//    Length(flt).poll(0, "flt-len")

    val i3        = flt
//    val frameTr1  = Metro(frameSize)
//    val frameTr2  = Metro(frameSize)
    val max      = RunningMax(i3 /*, trig = frameTr1*/).last // .drop(frameSize - 1)
    val min      = RunningMin(i3 /*, trig = frameTr1*/).last // .drop(frameSize - 1)
//    val max       = Gate(maxR, gate = frameTr2)
//    val min       = Gate(minR, gate = frameTr2)
    val mul       = (max - min).reciprocal
    val add       = -min

    min.poll(0, "min")
    max.poll(0, "max")
    mul.poll(0, "mul")
    add.poll(0, "add")

//    val i3e       = i3.elastic(frameSize / cfg.blockSize + 1)
    val i3e       = BufferMemory(i3, frameSize) // .elastic((frameSize / cfg.blockSize + 1) * 2)
//    m3.poll(0, "m3-0")
//    i3.poll(0, "i3-0")
//
    val noise     = WhiteNoise(0.1).take(frameSize)
    val i4        = (i3e + add) * mul + noise
    i4.poll(0, "i4-0")
    Length(i4).poll(0, "i4-length")

////    Progress(Frames(i4) / (2 * frameSize), Metro(width))
//    ProgressFrames(i4, frameSize)

    val sig     = i4.clip(0.0, 1.0)  // BufferMemory(i4, frameSize) // .clip(0.0, 1.0)
    sig.poll(0, "sig-0")

// TODO: this breaks the graph
    val specOut = ImageFile.Spec(width = width, height = height, numChannels = 1,
      sampleFormat = ImageFile.SampleFormat.Int16)
    ImageFileOut(file = fOut, spec = specOut, in = sig)

//    val specOut = AudioFileSpec(numChannels = 1, sampleRate = 44100)
//    AudioFileOut(file = fOut, spec = specOut, in = sig)
  }

  val ctl = Control(cfg)
  Swing.onEDT {
    gui = SimpleGUI(ctl)
  }
  ctl.run(g)
}