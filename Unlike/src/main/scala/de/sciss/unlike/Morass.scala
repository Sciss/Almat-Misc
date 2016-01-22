/*
 *  Morass.scala
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

import de.sciss.file._
import de.sciss.numbers
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{ProcessorFactory, ProcessorLike}
import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFile, AudioFileSpec}
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D

import scala.concurrent.blocking

object Morass extends ProcessorFactory {
  case class Config(input: File = file("input"), template: File = file("template"), output: File = file("output"),
                    outputFileType    : AudioFileType = AudioFileType.AIFF,
                    outputSampleFormat: SampleFormat  = SampleFormat.Float,
                    inputWinSize      : Int = 16384,
                    templateWinSize   : Int = 16384,
                    analyzeWinType    : WindowFunction = WindowFunction.Hanning,
                    synthesizeWinType : WindowFunction = WindowFunction.Hanning,
                    synthesizeWinAmt  : Double  = 1.0,
                    ampModulation     : Double  = 0.0,
                    stepSize          : Int     = 16,
                    radius            : Double  = 1.0,
                    keepFileLength    : Boolean = true
   ) {
    require(inputWinSize    >= 2)
    require(templateWinSize >= 2)
    require(stepSize > 0 && stepSize <= inputWinSize && stepSize <= templateWinSize )
    require(radius   >= 0 && radius <= 1.0)
    require(synthesizeWinAmt >= 0 && synthesizeWinAmt <= 1.0)
  }

  type Product  = Unit
  type Repr     = Morass

  protected def prepare(config: Config): Prepared = new Impl(config)

  private final class Impl(val config: Config) extends ProcessorImpl[Product, Repr] with Repr {
    override def toString = s"Morass@${hashCode.toHexString}"

    protected def body(): Unit = {
      var resources = List.empty[() => Unit]
      import config._

      import math.{max, min}

      val winSize = max(inputWinSize, templateWinSize)

      try {
        val afInA         = blocking(AudioFile.openRead(input))
        resources       ::= { () => afInA.close() }
        val afInB         = blocking(AudioFile.openRead(template))
        resources       ::= { () => afInB.close() }
        val numCh         = max(afInA.numChannels, afInB.numChannels)
        val numFrames     = min(afInA.numFrames  , afInB.numFrames  )
        val numFramesOut  = if (keepFileLength) numFrames else numFrames + winSize
        val specOut       = AudioFileSpec(fileType = outputFileType, sampleFormat = outputSampleFormat,
                                          numChannels = numCh, sampleRate = afInA.sampleRate)
        val afOut         = blocking(AudioFile.openWrite(output, specOut))
        resources       ::= { () => afOut.close() }
        checkAborted()

        // val over          = winSize - (winSize % stepSize)
        val bufInSz       = winSize // + over
        val bufOutSz      = bufInSz + winSize
        val bufInSzM      = bufInSz  - stepSize
        val bufOutSzM     = bufOutSz - stepSize
        val bufInA        = Array.ofDim[Float](numCh, bufInSz )
        val bufInB        = Array.ofDim[Float](numCh, bufInSz )
        val bufOut        = Array.ofDim[Float](numCh, bufOutSz)
        val winAnaIn      = analyzeWinType    .create(inputWinSize   )
        val winAnaTemp    = analyzeWinType    .create(templateWinSize)
        val winSynth      = if (synthesizeWinAmt == 1.0) {
          synthesizeWinType.create(inputWinSize)
        } else {
          val arr  = new Array[Double](inputWinSize)
          val len  = (synthesizeWinAmt * inputWinSize + 0.5).toInt
          val lenH = len >> 1
          synthesizeWinType.fill(arr, 0, len)
          System.arraycopy(arr, lenH, arr, inputWinSize - (len - lenH), len - lenH)
          WindowFunction.Rectangle.fill(arr, lenH, inputWinSize - len)
          arr
        }
        val fft           = new DoubleFFT_1D(winSize)
        val fftBufA       = new Array[Double](winSize)
        val fftBufB       = new Array[Double](winSize)

        var framesRead    = 0L
        var framesWritten = 0L

        def prepareFFT(in: Array[Float], out: Array[Double], win: Array[Double]): Unit = {
          var i = 0
          while (i < win.length) {
            out(i) = in(i) * win(i)
            i += 1
          }
          while (i < winSize) {
            out(i) = 0f
            i += 1
          }
        }

        // def checkNaNs(buf: Array[Double], msg: String): Unit =
        //   if (buf.exists(_.isNaN)) println(s"NaN detected: $msg")

        val winSizeH      = winSize >> 1
        val radiusI       = max(1, min(winSizeH - 1, (radius * winSizeH + 0.5).toInt))
        val settings      = PhaseCorrelation.Settings(downSample = 1.0, transRadius = radiusI)
        var bufInOff      = 0
        while (framesWritten < numFramesOut) {
          blocking {
            val chunkIn = min(bufInSz - bufInOff, numFrames - framesRead).toInt
            afInA.read(bufInA, bufInOff, chunkIn)
            afInB.read(bufInB, bufInOff, chunkIn)
            var ch = 0
            while (ch < numCh) {
              val chInA = bufInA(ch)
              val chInB = bufInB(ch)
              val chOut = bufOut(ch)

              var i = bufInOff + chunkIn
              while (i < winSize) {
                chInA(i) = 0f
                chInB(i) = 0f
                i += 1
              }

              prepareFFT(chInA, fftBufA, winAnaIn  )
              prepareFFT(chInB, fftBufB, winAnaTemp)
              fft.realForward(fftBufA)
              fft.realForward(fftBufB)

              import PhaseCorrelation.{complexConj, elemMul, elemNorm, findPeakCentroid}
              complexConj(fftBufA)  // A is to be shift against B!
              elemMul(fftBufA, fftBufB)
              // checkNaNs(fftBufA, "elemMul")
              elemNorm(fftBufA)
              // checkNaNs(fftBufA, "elemNorm")
              fft.realInverse(fftBufA, true)
              // checkNaNs(fftBufA, "FFT inverse")

              val img     = new Image(fftBufA, width = winSize, height = 1)
              val prod    = findPeakCentroid(img, settings)
              val transX  = (prod.translateX + 0.5).toInt
              val shiftX  = if (keepFileLength) transX else transX + winSizeH
              // println(f"peak = $peak%1.2f")

              // handle overlap out
              System.arraycopy(chOut, stepSize, chOut, 0, bufOutSzM)

              import numbers.Implicits._
              val amp = ampModulation.linlin(0, 1, 1.0, prod.peak)
              i = if (shiftX < 0) -shiftX else 0
              while (i < winSynth.length) {
                chOut(i + shiftX) += (chInA(i) * amp * winSynth(i)).toFloat
                i += 1
              }

              // handle overlap in
              System.arraycopy(chInA, stepSize, chInA, 0, bufInSzM)
              System.arraycopy(chInB, stepSize, chInB, 0, bufInSzM)

              ch += 1
            }

            val chunkOut = min(stepSize, numFramesOut - framesWritten).toInt
            afOut.write(bufOut, 0, chunkOut)

            bufInOff       = bufInSzM
            framesRead    += chunkIn
            framesWritten += chunkOut
          }
          checkAborted()
          progress = framesWritten.toDouble / numFramesOut
        }

      } finally {
        resources.foreach(_.apply())
      }
    }
  }
}
trait Morass extends ProcessorLike[Unit, Morass] {
  def config: Morass.Config
}