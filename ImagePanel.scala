package photoscala

import javax.swing.*
import java.awt.{Dimension, Graphics}

class ImagePanel(private var image: Image) extends JPanel:
  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    image.draw(g)

  override def getPreferredSize = Dimension(image.width, image.height)

  def replaceImage(newImage: Image): Unit =
    image = newImage
    revalidate()
    repaint()

  def getImage = image
