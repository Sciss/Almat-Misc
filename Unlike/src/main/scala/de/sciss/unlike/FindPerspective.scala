package de.sciss.unlike

import java.awt.image.BufferedImage

import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{ProcessorFactory, ProcessorLike}

object FindPerspective extends ProcessorFactory {
  case class Config(a: BufferedImage, b: BufferedImage, maxDistance: Int = 100, initCoarse: Int = 32, rounds: Int = 4)

  case class Product(topLeft: IntPoint2D, topRight: IntPoint2D, bottomRight: IntPoint2D, bottomLeft: IntPoint2D)

  type Repr = FindPerspective

  protected def prepare(config: Config): Prepared = new Impl(config)

  private final class Impl(val config: Config) extends ProcessorImpl[Product, Repr] with Repr {
    protected def body(): Product = {
      import config._
      for (r <- 1 to rounds) {
        for (corner <- 0 until 4) {
          for (dx <- -maxDistance to maxDistance by initCoarse) {
            for (dy <- -maxDistance to maxDistance by initCoarse) {
              // something like this
              
              checkAborted()
            }
          }
        }
      }
      // [-2i -1i 0 +1i +2i]
      ???
    }
  }
}

/** A processor that estimates the perspective transform needed to equalize
  * an image `b` so that it maximally matches an image `a`. The transform
  * is returned as offsets of the four corners of the perspective frame.
  */
trait FindPerspective extends ProcessorLike[FindPerspective.Product, FindPerspective] {
  def config: FindPerspective.Config
}