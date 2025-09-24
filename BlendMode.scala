package photoscala

trait BlendMode:
  def combine(fg: Pixel, bg: Pixel): Pixel

object BlendMode:
  def parse(word: String): BlendMode = word match
    case "transparency" => Transparency(0.5)
    case "multiply"     => Multiply
    case "screen"       => Screen
    case "overlay"      => Overlay
    case _ =>
      println("Invalid blend mode. Running no-blend.")
      NoBlend

class Transparency(factor: Double) extends BlendMode:
  val f = if factor <= 0 then 0.0 else if factor >= 1 then 1.0 else factor

  // fg * f + bg * (1-f)
  def combine(fg: Pixel, bg: Pixel) = Pixel(
    (fg.r * f + bg.r * (1 - f)).toInt,
    (fg.g * f + bg.g * (1 - f)).toInt,
    (fg.b * f + bg.b * (1 - f)).toInt
  )

object Multiply extends BlendMode:
  // fg/255 * bg/255 * 255
  def combine(fg: Pixel, bg: Pixel) = Pixel(
    (fg.r * bg.r / 255.0).toInt,
    (fg.g * bg.g / 255.0).toInt,
    (fg.b * bg.b / 255.0).toInt
  )

object Screen extends BlendMode:
  // 255 - (255-fg)/255 * (255-bg)/255 * 255
  def combine(fg: Pixel, bg: Pixel) = Pixel(
    (255 - (255 - fg.r) / 255.0 * (255 - bg.r) / 255.0 * 255).toInt,
    (255 - (255 - fg.g) / 255.0 * (255 - bg.g) / 255.0 * 255).toInt,
    (255 - (255 - fg.b) / 255.0 * (255 - bg.b) / 255.0 * 255).toInt
  )

object Overlay extends BlendMode:
  private def f(a: Double, b: Double) =
    if a < 0.5 then 2 * a * b else 1 - 2 * (1 - a) * (1 - b)

  def combine(fg: Pixel, bg: Pixel) = Pixel(
    (255 * f(bg.r / 255.0, fg.r / 255.0)).toInt,
    (255 * f(bg.g / 255.0, fg.g / 255.0)).toInt,
    (255 * f(bg.b / 255.0, fg.b / 255.0)).toInt
  )

case object NoBlend extends BlendMode:
  def combine(fg: Pixel, bg: Pixel): Pixel = fg
