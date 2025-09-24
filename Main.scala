package photoscala

import java.util.Scanner
import javax.swing.{JFrame, WindowConstants}
import scala.io.Source

object App:
  private var frame: Option[JFrame]          = None
  private var imagePanel: Option[ImagePanel] = None

  def load(path: String) =
    val image = Image.load(path)
    if frame.isEmpty then // initialization
      val newFrame      = JFrame("Photoscala")
      val newImagePanel = ImagePanel(image)
      newFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      newFrame.getContentPane.add(newImagePanel)
      newFrame.pack()
      newFrame.setVisible(true)
      frame = Some(newFrame)
      imagePanel = Some(newImagePanel)
    else
      imagePanel.foreach(_.replaceImage(image))
      frame.foreach(_.pack())

  def main(args: Array[String]): Unit =
    print("> ")
    Source.stdin
      .getLines()
      .foreach: command =>
        val words  = command.split(" ")
        val action = words.head
        action match
          case "load" =>
            val path = words(1)
            try load(path)
            catch case e => println(s"Cannot load file at path $path")
          case "save" =>
            if frame.isEmpty then println("Error: no image loaded, cannot save.")
            else imagePanel.foreach(_.getImage.save(words(1)))
          case "exit" => System.exit(0)
          case _ if frame.isEmpty =>
            println("Error: must have an image loaded before applying transformations")
          case _ =>
            imagePanel.foreach: panel =>
              val transformation = Transformation.parse(command)
              val newImage       = transformation(panel.getImage)
              panel.replaceImage(newImage)
        print("> ")
    ()
