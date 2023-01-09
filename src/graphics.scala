import java.awt.Color

class graphics {

  //images
  val grass = new GraphicsBitmap("/img/grass.jpg")
  val woodBox = new GraphicsBitmap("/img/WoodBox.png")
  val StoneWall = new GraphicsBitmap("/img/StoneWall.png")


  val display = new FunGraphics(950, 750, "BomberMan")

  // graphics

  def updateGraphics(): Unit = {
    val i1 = 50
    for (i <- 0 until gridX) {
      for (j <- 0 until gridY) {
        grille(i)(j) match {
          case "0" => display.drawPicture(j * i1 + 25, 125 + i * i1, StoneWall)

          case "2" => display.setColor(Color.yellow)
            display.drawFillRect(j * i1, 100 + i * i1, i1, i1)
          case " " => display.drawPicture(j * i1 + 25, 125 + i * i1, grass)
          case "E" => display.setColor(Color.red)
            display.drawFillRect(j * i1, 100 + i * i1, i1, i1)
          case "W" => display.drawPicture(j * i1 + 25, 125 + i * i1, woodBox)

        }

      }
    }
  }

}
