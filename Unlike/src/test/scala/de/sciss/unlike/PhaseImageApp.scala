package de.sciss.unlike

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.numbers
import de.sciss.unlike.PhaseCorrelation.{Settings, prepareData, prepareFFT, prepareImage, preparePeak}

object PhaseImageApp {
  def main(args: Array[String]): Unit = run(file(args(0)), file(args(1)))

  def run(f1: File, f2: File): Unit = {
    val settings  = Settings(downSample = 1.0, thresh1 = 0.5, thresh2 = 0.33)
    val imgA      = prepareImage(f1, settings)
    val imgB      = prepareImage(f2, settings)
    val fft       = prepareFFT(imgA)
    val pred      = prepareData(imgA, fft)
    val succ      = prepareData(imgB, fft)
    val matrix    = preparePeak(pred, succ, fft)
    val data      = matrix.data
    val min       = data.min
    val max       = data.max
//    val buf       = matrix.toAwt(mul = 1.0 / (max - min), add = -min)
    for (i <- data.indices) {
      import numbers.Implicits._
      data(i) = data(i) /* .clip(0, max) */ .linLin(min, max, 0, 1).pow(1.0/2)
    }
    val buf       = matrix.toAwt(mul = 1.0, add = 0)
    val w         = buf.getWidth
    val h         = buf.getHeight
    val buf2      = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_GRAY)
    val g         = buf2.createGraphics()
    // put zero point into centre
    g.drawImage(buf, 0  ,   0, w/2, h/2, w/2, h/2, w  , h  , null)
    g.drawImage(buf, w/2,   0, w  , h/2,   0, h/2, w/2, h  , null)
    g.drawImage(buf, 0  , h/2, w/2, h  , w/2,   0, w  , h/2, null)
    g.drawImage(buf, w/2, h/2, w  , h  ,   0,   0, w/2, h/2, null)
    g.dispose()
    ImageIO.write(buf2, "png", userHome / "Documents" / "temp" / "_killme.png")
  }
}