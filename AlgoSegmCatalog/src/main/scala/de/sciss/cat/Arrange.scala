package de.sciss.cat

import de.sciss.file.*

import java.awt.Color
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import Param.{rectW, rectH, gridSize, pages}

object Arrange:
  val baseDir     = file("/data/projects/Almat/events/graz2020/catalog/image_materials")
  val inDir       = baseDir / "cover"
  val bleed       = 76
  val bleedT      = bleed * 2
  val numColumns  = gridSize * pages
  val numRows     = gridSize
  val numImages   = numColumns * numRows
  val widthOut    = rectW * numColumns + bleedT
  val heightOut   = rectH * numRows    + bleedT

  def main(args: Array[String]): Unit =
    println(s"numImages $numImages")
    for i <- 0 until 2 do
      val fOut = baseDir / "letters-arrange-%d.png".format(i + 1)
      run(fOut, turn = i)

  def run(fOut: File, turn: Int): Unit =
    require (!fOut.exists(), fOut.path)
    val imgOut  = new BufferedImage(widthOut, heightOut, BufferedImage.TYPE_BYTE_BINARY)
    val g       = imgOut.createGraphics()
    g.setBackground(Color.white)
    g.clearRect(0, 0, widthOut, heightOut)
    val iOff = turn * numImages + 1
    for i <- 0 until numImages do
      val fIn     = inDir / s"letters-%d.png".format(i + iOff)
      require (fIn.isFile, fIn.path)
      val imgIn   = ImageIO.read(fIn)
      val col     = i % numColumns
      val row     = i / numColumns
      val isColF  = col == 0
      val isRowF  = row == 0
      val isColL  = col == numColumns - 1
      val isRowL  = row == numRows - 1
      val isColE  = isColF | isColL
      val isRowE  = isRowF | isRowL
      val dx1     = col       * rectW + (if isColF then 0 else bleed)
      val dx2     = dx1 + rectW + (if isColE then bleed else 0)
      val dy1     = row       * rectH + (if isRowF then 0 else bleed)
      val dy2     = dy1 + rectH + (if isRowE then bleed else 0)
      val sx1     = 0     + (if isColF then 0 else bleed)
      val sx2     = sx1 + rectW + (if isColE then bleed else 0)
      val sy1     = 0     + (if isRowF then 0 else bleed)
      val sy2     = sy1 + rectH + (if isRowE then bleed else 0)
      
      assert (dx2 - dx1 == sx2 - sx1)
      assert (dy2 - dy1 == sy2 - sy1)
      
      g.drawImage(imgIn, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2, null)
    end for
    ImageIO.write(imgOut, "png", fOut)
