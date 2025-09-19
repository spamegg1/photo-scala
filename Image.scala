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
    Image(newBufImage)

  def map(f: Pixel => Pixel): Image =
    val newPixels = Array.fill(width * height)(0)
    bufImage.getRGB(0, 0, width, height, newPixels, 0, width)
    newPixels.mapInPlace(color => f(Pixel.fromHex(color)).toInt)

    val newBufImage = BufferedImage(width, height, TYPE_INT_RGB)
    newBufImage.setRGB(0, 0, width, height, newPixels, 0, width)
    Image(newBufImage)

object Image:
  def load(path: String) = Image(ImageIO.read(File(path)))
  def black(width: Int, height: Int) =
    val colors = Array.fill(width * height)(0)
    val bufImg = BufferedImage(width, height, TYPE_INT_RGB)
    bufImg.setRGB(0, 0, width, height, colors, 0, width)
    Image(bufImg)

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
