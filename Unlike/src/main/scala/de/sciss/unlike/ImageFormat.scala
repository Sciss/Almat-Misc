package de.sciss.unlike

object ImageFormat {
  case object JPG extends ImageFormat {
    val extension = "jpg"
  }
  case object PNG extends ImageFormat {
    val extension = "png"
  }
}
sealed trait ImageFormat {
  /** File extension without leading period. */
  def extension: String
}