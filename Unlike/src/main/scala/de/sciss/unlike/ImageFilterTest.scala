/*
 *  ImageFilterTest.scala
 *  (Unlike)
 *
 *  Copyright (c) 2015-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.unlike

import com.jhlabs.image.{GammaFilter, NoiseFilter}
import de.sciss.file._

import javax.imageio.ImageIO

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
