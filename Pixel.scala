package photoscala

import java.awt.image.BufferedImage, BufferedImage.TYPE_INT_RGB
import javax.imageio.ImageIO
import java.io.File

case class Pixel(r: Int, g: Int, b: Int):
  assert(0 <= r && r < 256 && 0 <= g && g < 256 && 0 <= b && b < 256)

  // color: #12ffcb
  // r: 000000000000000000000000rrrrrrrr
  // g: 000000000000000000000000gggggggg
  // b: 000000000000000000000000bbbbbbbb
  // n: 00000000rrrrrrrrggggggggbbbbbbbb
  def toInt = (r << 16) | (g << 8) | b

  def draw(width: Int, height: Int, path: String) =
    val color  = toInt
    val image  = BufferedImage(width, height, TYPE_INT_RGB)
    val pixels = Array.fill(width * height)(color)
    image.setRGB(0, 0, width, height, pixels, 0, width)
    ImageIO.write(image, "JPG", File(path))

  infix def +(that: Pixel) =
    Pixel(Pixel.clamp(r + that.r), Pixel.clamp(g + that.g), Pixel.clamp(b + that.b))

object Pixel:
  val Black = Pixel(0, 0, 0)
  val White = Pixel(255, 255, 255)
  val Red   = Pixel(255, 0, 0)
  val Green = Pixel(0, 255, 0)
  val Blue  = Pixel(0, 0, 255)
  val Gray  = Pixel(128, 128, 128)

  def clamp(n: Int) = if n <= 0 then 0 else if n > 255 then 255 else n

  // color:     00000000rrrrrrrrggggggggbbbbbbbb
  // redmask:   00000000111111110000000000000000
  // greenmask: 00000000000000001111111100000000
  // bluemask:  00000000000000000000000011111111
  def fromHex(color: Int) = Pixel(
    (color & 0xff0000) >> 16,
    (color & 0x00ff00) >> 8,
    (color & 0x0000ff)
  )

@main
def runPixel: Unit =
  val yellow = Pixel.Red + Pixel.Green
  val file1  = "images/yellow.jpg"
  yellow.draw(40, 40, file1)

  val pink  = Transparency(0.5).combine(Pixel.Red, Pixel.White)
  val file2 = "images/pink.jpg"
  pink.draw(40, 40, file2)

  val darkRed = Multiply.combine(Pixel.Red, Pixel.Gray)
  val file3   = "images/darkRed.jpg"
  darkRed.draw(40, 40, file3)

  val lightRed = Screen.combine(Pixel.Red, Pixel.Gray)
  val file4    = "images/lightRed.jpg"
  lightRed.draw(40, 40, file4)
