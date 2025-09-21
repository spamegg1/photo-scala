package photoscala

import scala.annotation.targetName

case class Window(width: Int, height: Int, pixels: List[Pixel])

case class Kernel(width: Int, height: Int, values: List[Double]):
  val sum       = values.sum // all the values must sum up to 1.0
  val div       = if sum == 0 then 1 else sum
  def normalize = Kernel(width, height, values.map(_ / div))

  @targetName("multiply")
  infix def *(window: Window): Pixel =
    assert(width == window.width)
    assert(height == window.height)
    val (red, green, blue) = window.pixels
      .zip(values)
      .map:
        case (Pixel(r, g, b), k) =>
          (r * k, g * k, b * k)
      .reduce:
        case ((r1, g1, b1), (r2, g2, b2)) => (r1 + r2, g1 + g2, b1 + b2)
    Pixel(red.toInt, green.toInt, blue.toInt)

object Kernel:
  val blur   = Kernel(3, 3, List(1, 2, 1, 2, 4, 2, 1, 2, 1)).normalize
  val sharp  = Kernel(3, 3, List(0, -1, 0, -1, 5, -1, 0, -1, 0)).normalize
  val edge   = Kernel(3, 3, List(1, 0, -1, 2, 0, -2, 1, 0, -1)).normalize
  val emboss = Kernel(3, 3, List(-2, -1, 0, -1, 1, 1, 0, 1, 2)).normalize
