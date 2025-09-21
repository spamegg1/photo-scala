package photoscala

trait Transformation:
  def apply(image: Image): Image

case class Crop(x: Int, y: Int, w: Int, h: Int) extends Transformation:
  def apply(image: Image) =
    try image.crop(x, y, w, h)
    catch
      case ex =>
        println(s"Error, coords are OOB; max w: ${image.width} h: ${image.height}")
        image

case object Grayscale extends Transformation:
  def apply(image: Image): Image = image.map: pixel =>
    val avg = (pixel.r + pixel.g + pixel.b) / 3
    Pixel(avg, avg, avg)

case object Invert extends Transformation:
  def apply(image: Image): Image = image.map: pixel =>
    Pixel(255 - pixel.r, 255 - pixel.g, 255 - pixel.b)

case class Colorize(color: Pixel) extends Transformation:
  def apply(image: Image): Image = image.map: pixel =>
    val avg = (pixel.r + pixel.g + pixel.b) / 3
    Pixel(
      (color.r * (avg / 255.0)).toInt,
      (color.g * (avg / 255.0)).toInt,
      (color.b * (avg / 255.0)).toInt
    )

case class Blend(fg: Image, blendMode: BlendMode) extends Transformation:
  def apply(bg: Image): Image =
    val (width, height) = (fg.width, fg.height)
    if width != bg.width || height != bg.height then
      println("Error, images have different sizes:")
      println(s"fg: w: $width h: $height")
      println(s"bg: w: ${bg.width} h: ${bg.height}")
      bg
    else
      val result = Image.black(width, height)
      for
        x <- 0 until width
        y <- 0 until height
      do
        val newColor = blendMode.combine(fg.getColor(x, y), bg.getColor(x, y))
        result.setColor(x, y, newColor)
      result

case class KernelFilter(kernel: Kernel) extends Transformation:
  def apply(image: Image): Image =
    val pixels =
      for
        y <- 0 until image.height
        x <- 0 until image.width
      yield kernel * image.window(x, y, kernel.width, kernel.height)
    Image(image.width, image.height, pixels.toArray)

@main
def runTransf: Unit =
  val image = Image.load("images/suika.jpg")
  // Crop(200, 500, 200, 200)(image).save("images/suika-cropped.jpg")
  // Invert(image).save("images/suika-inverted.jpg")
  // Grayscale(image).save("images/suika-grayed.jpg")
  // Colorize(Pixel.Red)(image).save("images/suika-red.jpg")

  val fg = Image.load("images/suika-cropped.jpg")
  val bg = Image.load("images/shot-cropped.jpg")
  // Blend(fg, Multiply)(bg).save("images/suika-shot-multiply-blended.jpg")
  // Blend(fg, Screen)(bg).save("images/suika-shot-screen-blended.jpg")
  // Blend(fg, Transparency(0.3))(bg).save("images/suika-shot-transparent.jpg")
  // Blend(fg, Overlay)(bg).save("images/suika-shot-overlay.jpg")

  // KernelFilter(Kernel.blur)(image).save("images/suika-blurred.jpg")
  // KernelFilter(Kernel.sharp)(image).save("images/suika-sharpened.jpg")
  // KernelFilter(Kernel.edge)(image).save("images/suika-edge.jpg")
  // KernelFilter(Kernel.emboss)(image).save("images/suika-embossed.jpg")
