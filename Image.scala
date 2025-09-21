package photoscala

import java.awt.image.BufferedImage, BufferedImage.TYPE_INT_RGB
import javax.imageio.ImageIO
import java.io.File

class Image private (private val bufImage: BufferedImage):
  val width                              = bufImage.getWidth()
  val height                             = bufImage.getHeight()
  def getColor(x: Int, y: Int)           = Pixel.fromHex(bufImage.getRGB(x, y))
  def setColor(x: Int, y: Int, p: Pixel) = bufImage.setRGB(x, y, p.toInt)
  def save(path: String)                 = ImageIO.write(bufImage, "JPG", File(path))
  def crop(startX: Int, startY: Int, w: Int, h: Int) =
    assert(0 <= startX && startX < width)
    assert(0 <= startY && startY < height)
    assert(0 < w && 0 < h)
    assert(startX + w < width && startY + h < height)

    val newPixels = Array.fill(w * h)(0)
    bufImage.getRGB(startX, startY, w, h, newPixels, 0, w)
    val newBufImage = BufferedImage(w, h, TYPE_INT_RGB)
    newBufImage.setRGB(0, 0, w, h, newPixels, 0, w)
    new Image(newBufImage)

  def map(f: Pixel => Pixel): Image =
    val newPixels = Array.fill(width * height)(0)
    bufImage.getRGB(0, 0, width, height, newPixels, 0, width)
    newPixels.mapInPlace(color => f(Pixel.fromHex(color)).toInt)

    val newBufImage = BufferedImage(width, height, TYPE_INT_RGB)
    newBufImage.setRGB(0, 0, width, height, newPixels, 0, width)
    new Image(newBufImage)

  /*
     +---------------------------------------------------------+
     |                                                         |
     |           a1 a2 a3 a4 a5                                |
     |           b1 b2 b3 b4 b5                                |
  y >|           c1 c2 XX c4 c5                                |
     |           d1 d2 d3 d4 d5                                |
     |           e1 e2 e3 e4 e5                                |
     |                                                         |
     |                                                         |
     |                                                         |
     |                                                         |
     +---------------------------------------------------------+
                        ^
                        x
    window = List(a1, ..., a5, b1, ..., b5, ..., e1, ..., e5)
   */
  def window(x: Int, y: Int, w: Int, h: Int): Window =
    val offsetX = (w - 1) / 2
    val offsetY = (h - 1) / 2
    val horizCoord = (x - offsetX to x + offsetX)
      .map: xVal =>
        if xVal < 0 then 0 else if xVal >= width then width - 1 else xVal
    val vertCoord = (y - offsetY to y + offsetY)
      .map: yVal =>
        if yVal < 0 then 0 else if yVal >= height then height - 1 else yVal
    val pixels =
      for
        x <- horizCoord
        y <- vertCoord
      yield getColor(x, y)
    Window(w, h, pixels.toList)

object Image:
  def apply(width: Int, height: Int, pixels: Array[Pixel]): Image =
    val bufImage = BufferedImage(width, height, TYPE_INT_RGB)
    bufImage.setRGB(0, 0, width, height, pixels.map(_.toInt), 0, width)
    new Image(bufImage)

  def load(path: String) = new Image(ImageIO.read(File(path)))

  def black(width: Int, height: Int) =
    val colors = Array.fill(width * height)(0)
    val bufImg = BufferedImage(width, height, TYPE_INT_RGB)
    bufImg.setRGB(0, 0, width, height, colors, 0, width)
    new Image(bufImg)

@main
def runImage: Unit =
  Image
    .load("images/shot.jpg")
    .crop(100, 500, 200, 200)
    .save("images/shot-cropped.jpg")

  Image
    .load("images/hornet.jpg")
    .crop(100, 500, 200, 200)
    .save("images/hornet-cropped.jpg")
