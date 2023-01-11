import hevs.graphics.FunGraphics
import hevs.graphics.utils.GraphicsBitmap
import scala.util.Random
import java.awt.event.{KeyAdapter, KeyEvent}

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
  val BomberMan = new GraphicsBitmap("/img/Bomberman.png")
  val Enemy = new GraphicsBitmap("/img/ennemy.png")
  val Logo = new GraphicsBitmap("/img/logo.png")

  display.setKeyManager(new KeyAdapter() { // Will be called when a key has been pressed
    override def keyPressed(e: KeyEvent): Unit = {
      if (e.getKeyCode == KeyEvent.VK_UP) move('w')
      if (e.getKeyCode == KeyEvent.VK_DOWN) move('s')
      if (e.getKeyCode == KeyEvent.VK_LEFT) move('a')
      if (e.getKeyCode == KeyEvent.VK_RIGHT) move('d')
    }
  })


  def updateGraphics(): Unit = {
    val i1 = 50
    display.syncGameLogic(60)

    for (i <- 0 until gridX) {
      for (j <- 0 until gridY) {
        display.drawPicture(j * i1 + 25, 125 + i * i1, grass)
        grille(i)(j) match {
          case "0" => display.drawPicture(j * i1 + 25, 125 + i * i1, StoneWall)
          case "2" => display.drawPicture(j * i1 + 25, 125 + i * i1, BomberMan)
          case " " =>
          case "E" => display.drawPicture(j * i1 + 25, 125 + i * i1, Enemy)
          case "W" => display.drawPicture(j * i1 + 25, 125 + i * i1, woodBox)
          case "X" => display.drawPicture(j * i1 + 25, 125 + i * i1, woodBox)
        }

      }
    }
    display.drawPicture(display.width / 2, 75, Logo)
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
    grille(2)(15) = "E"
    grille(5)(15) = "E"
    grille(9)(15) = "E"


  }

  //to move the man
  var memx = 0
  var memy = 0
  var counter1: Long = 0
  var membombx = 0
  var membomby = 0

  def move(lettre: Char): Unit = {
    val read: Char = lettre
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
      case 'x' =>

        membombx = r
        membomby = c
        bomb(r, c)
        val start = System.currentTimeMillis()
        Thread.sleep(5000)
        explosion()
        moveenemy()
      case _ =>
        println("Allowed touches are a s d w")
        moveenemy()
    }
    memx = r
    memy = c
  }


  //creates a bom
  def bomb(x: Int, y: Int): Unit = {
    grille(x)(y) = "X"
    grille(memx)(memy) = "2"

  }

  def explosion(): Unit = {
    println(membombx)
    println(membomby)
    grille(membombx)(membomby) = " "
  }

  //l'énemi doit toujours bouger!
  def moveenemy(): Unit = {
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
    updateGraphics()
  }
}

