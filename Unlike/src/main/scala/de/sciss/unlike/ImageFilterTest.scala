package de.sciss.unlike

import javax.imageio.ImageIO

import com.jhlabs.image.{NoiseFilter, GammaFilter}
import de.sciss.file._

object ImageFilterTest extends App {
  val base      = userHome / "Documents" / "projects" / "Unlike"
  val fIn       = base / "moor_8024" / "moor_8024-%05d.jpg".format(10)
  val fOut      = userHome / "Documents" / "temp" / "_killme.jpg"

  require(!fOut.exists())

  var img       = ImageIO.read(fIn)
  val fltGamma  = new GammaFilter(0.5f)
  val fltNoise  = new NoiseFilter
  fltNoise.setAmount(10)
  img           = fltGamma.filter(img, null)
  img           = fltNoise.filter(img, null)

  ImageIO.write(img, "jpg", fOut)
}
