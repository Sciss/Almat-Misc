/*
 *  SDC_Thumbnail.scala
 *  (Unlike)
 *
 *  Copyright (c) 2015-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.unlike

import de.sciss.file._

object SDC_Thumbnail extends App {
  import RCThumbnail._

  fOut    = userHome/"Documents"/"applications"/"160411_SoundDevelopmentCity"/"latex"/"figures"/"sdc_imperfect_thumb.jpg"
  rows    = 3
  columns = 7

  run()
}
