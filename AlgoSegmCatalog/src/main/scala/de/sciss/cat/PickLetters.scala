package de.sciss.cat

import de.sciss.file.*

import java.awt.{Color, Cursor, RenderingHints}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.swing.event.{Key, KeyPressed, MouseDragged, MouseEvent, MouseMoved, MousePressed, MouseReleased}
import scala.swing.{Component, Dimension, Graphics2D, MainFrame, Swing}

object PickLetters:
  val gridSize    = 3
  val layerSize   = 3
  val layerUnique = 2
  val pages       = 4
  val numLetters  = gridSize * gridSize * layerUnique * pages   // 72
  val widthIn     = 3744
  val heightIn    = 2720
  val numImagesIn = 1000
  val scaleDown   = 4
  val widthC      = widthIn/scaleDown
  val heightC     = heightIn/scaleDown
  val rectW       = 640
  val rectH       = 1280
  val rectWC      = rectW/scaleDown
  val rectHC      = rectH/scaleDown

  val baseDir     = file("/data/projects/Almat/events/xcoax2020/postcard/composed")
  
  def main(args: Array[String]): Unit =
    println(s"numLetters = $numLetters")
    Swing.onEDT(run())

  def run(): Unit =
    var indexIn = 1

    def loadImageIn(): BufferedImage = ImageIO.read(baseDir / "outcc%04d.png".format(indexIn))

    var imageIn = loadImageIn()
    var centerX = 0
    var centerY = 0
    var hasRect = false
    var picked  = Vector.empty[Pick]

    val f = new MainFrame

    def updateTitle(): Unit =
      f.title = s"idx $indexIn; picked ${picked.size}"

    def updateIndex(): Unit =
      hasRect = false
      imageIn = loadImageIn()
      updateTitle()
      component.repaint()

    def setRect(e: MouseEvent): Unit =
      centerX = e.point.x
      centerY = e.point.y
      hasRect = true
      component.repaint()

    object component extends Component:
      preferredSize = new Dimension(widthC, heightC)
      opaque        = true
      cursor        = Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR)
      focusable     = true

      override def paintComponent(g: Graphics2D): Unit =
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
        g.drawImage(imageIn, 0, 0, widthC, heightC, null)
        if hasRect then
          g.setColor(Color.blue)
          g.drawRect(centerX - rectWC/2, centerY - rectHC/2, rectWC, rectHC)

      listenTo(mouse.clicks)
      listenTo(mouse.moves)
      listenTo(keys)

      var dragging = false

      reactions += {
        case e: MousePressed =>
          dragging = true
          setRect(e)
        case _: MouseReleased =>
          dragging = false
        case e: MouseMoved if dragging =>
          setRect(e)
        case e: MouseDragged if dragging =>
          setRect(e)
        case e: KeyPressed =>
          e.key match {
            case Key.Left   if indexIn > 1                => indexIn -= 1; updateIndex()
            case Key.Right  if indexIn + 1 < numImagesIn  => indexIn += 1; updateIndex()
            case Key.Enter  if hasRect =>
              val p = Pick(idx = indexIn, x = centerX * scaleDown, y = centerY * scaleDown)
              println(p)
              picked :+= p
              if indexIn + 1 < numImagesIn then
                indexIn += 1
                updateIndex()

            case Key.P =>
              println("Vector(")
              println(picked.grouped(8).map(_.mkString("  ", ", ", "")).mkString("\n"))
              println(")")
            case _ =>
          }
      }

    f.contents = component
    f.pack().centerOnScreen()
    f.open()
    component.requestFocus()
