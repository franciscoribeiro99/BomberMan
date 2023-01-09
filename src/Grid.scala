import hevs.graphics.FunGraphics
import hevs.graphics.utils.GraphicsBitmap

import java.awt.Color
import scala.util.Random

class Grid {
  //implemente la zone de jeu 12x7
  val gridX = 13
  val gridY = 19
  val grille: Array[Array[String]] = Array.ofDim(gridX, gridY)
  val display = new FunGraphics(950, 750, "BomberMan")

  //images
  val grass = new GraphicsBitmap("/img/grass.jpg")
  val woodBox = new GraphicsBitmap("/img/WoodBox.png")
  val StoneWall = new GraphicsBitmap("/img/StoneWall.png")

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

  //initialise la grille
  def initGrid(): Unit = {
    for (i <- 0 until gridX)
      for (j <- 0 until gridY) {
        grille(i)(j) = " "
      }
  }

  //affiche la grille sur la console
  def displayGrid(): String = {
    var retS: String = ""
    for (i <- 0 until gridX) {
      for (j <- 0 until gridY) {
        retS += s"${grille(i)(j)} \t"
      }
      retS += "\n"
    }
    retS
  }

  //fonction qui implemente les murs non statiques non détruisable donc pas au début de tableau
  def setRemovalWall(): Unit = {
    var randomX = 0
    var randomY = 0
    var placesfound: Int = 0
    do {
      randomX = (Math.random() * gridX).toInt
      randomY = (Math.random() * gridY).toInt
      if (grille(randomX)(randomY) != "0" && grille(randomX)(randomY) != "2") {
        placesfound += 1
        grille(randomX)(randomY) = "W"
      }
    } while (placesfound < 5)
  }


  //fonction qui implemente les murs statiques
  def setWall(): Unit = {
    for (i <- 0 until gridX) {
      for (j <- 0 until gridY) {
        if (i == 0 || j == 0 || i == gridX - 1 || j == gridY - 1 || i % 2 == 0 && j % 2 == 0)
          grille(i)(j) = "0"
      }
    }

  }

  def addPlayer(): Unit = {
    grille(1)(1) = "2"
  }

  //fonction pour créer un énemy
  def createEnemy(times: Int): Unit = {
    grille(2)(15)="E"
    grille(5)(15)="E"
    grille(9)(15)="E"

    var actualtime = 0
    /* for (i <- grille.indices; j <- (grille(i).length / 2) until grille(i).length) {

       if (actualtime < times) {
         if (grille(i)(j) == " " && r.nextInt(2) == 0) {
           actualtime += 1
           grille(i)(j) = "E"
         }
       }*/
  }

  //to move the man
  var memx = 0
  var memy = 0
  def move(): Unit = {
    var read: Char = Input.readChar()
    var random = new Random()
    var r = 0
    var c = 0
    for (i <- 0 until gridX) {
      for (j <- 0 until gridY) {
        if (grille(i)(j) == "2") {
          r = i
          c = j
        }
      }
    }
    read match {
      case 'w' => if (grille(r - 1)(c) == " ") {
        grille(r - 1)(c) = "2"
        grille(r)(c) = " "
        moveenemy()
      }
      else {
        println("It's not possible")
      }
      case 's' => if (grille(r + 1)(c) == " ") {
        grille(r + 1)(c) = "2"
        grille(r)(c) = " "
        moveenemy()
      }
      else {
        println("It's not possible")
      }
      case 'a' => if (grille(r)(c - 1) == " ") {
        grille(r)(c - 1) = "2"
        grille(r)(c) = " "
        moveenemy()
      }
      else {
        println("It's not possible")
      }
      case 'd' => if (grille(r)(c + 1) == " ") {
        grille(r)(c + 1) = "2"
        grille(r)(c) = " "
        moveenemy()
      }
      else {
        println("It's not possible")
      }
      case 'x' =>   grille(memx)(memy) = "2"
                    grille(r)(c) = "X"
                    moveenemy()
      case _ => println("Allowed touches are a s d w")
                moveenemy()
    }
    memx = r
    memy = c
      println(memx)
  }
  def moveenemy():Unit= {
    var r = new Random()
    for (i <- 0 until gridX) {
      for (j <- 0 until gridY) {
        if (grille(i)(j) == "E") {
          r.nextInt(3) match {
            case 0 => if (grille(i - 1)(j) == " ") {
              grille(i - 1)(j) = "E"
              grille(i)(j) = " "
            }
            case 1 => if (grille(i + 1)(j) == " ") {
              grille(i + 1)(j) = "E"
              grille(i)(j) = " "
            }
            case 2 => if (grille(i)(j - 1) == " ") {
              grille(i)(j - 1) = "E"
              grille(i)(j) = " "
            }
            case 3 => if (grille(i)(j + 1) == " ") {
              grille(i)(j + 1) = "E"
              grille(i)(j) = " "
            }
            case _ =>
          }
        }
      }
    }
  }
  def placebomb(): Unit = {

    }
}
