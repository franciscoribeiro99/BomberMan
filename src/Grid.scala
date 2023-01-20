
import hevs.graphics.FunGraphics
import hevs.graphics.utils.GraphicsBitmap

import java.awt.Color
import scala.util.Random
import java.awt.event.{KeyAdapter, KeyEvent}

class Grid {
  //implemente la zone de jeu 12x7

  /**
   * Things that have to be re-init between games
   */


  //l'énemi doit toujours bouger!
  var memE1x = 2
  var memE1y = 15
  var memE2x = 5
  var memE2y = 15
  var memE3x = 9
  var memE3y = 15
  var memx = 0
  var memy = 0
  var membombx = 0
  var membomby = 0
  var timetoexplosion: Int = 0
  var grille: Array[Array[String]] = Array.ofDim(gridX, gridY)

  // What are we currently doing and displaying (mui)
  var game_state: Int = STATE_PLAYING

  /**
   * Things that remain between different games
   */
  val gridX = 13
  val gridY = 19

  val STATE_PLAYING: Int = 0
  val STATE_SHOWING_GAME_OVER: Int = 1

  val display = new FunGraphics(950, 750, "BomberMan")

  //images
  val grass = new GraphicsBitmap("/img/grass.jpg")
  val woodBox = new GraphicsBitmap("/img/WoodBox.png")
  val stoneWall = new GraphicsBitmap("/img/StoneWall.png")
  val bomberMan = new GraphicsBitmap("/img/Bomberman.png")
  val enemyImg = new GraphicsBitmap("/img/ennemy.png")
  val gameLogo = new GraphicsBitmap("/img/logo.png")
  val bomb = new GraphicsBitmap("/img/bomb.png")
  val explosionimg = new GraphicsBitmap("/img/explosion.png")
  val gameoverimg = new GraphicsBitmap("/img/gameover.png")

  construct()

  def construct(): Unit = {
    //liaison avec les touches
    display.setKeyManager(new KeyAdapter() { // Will be called when a key has been pressed
      override def keyPressed(e: KeyEvent): Unit = {
        if (game_state == STATE_PLAYING) {
          if (e.getKeyCode == KeyEvent.VK_UP) move('w')
          if (e.getKeyCode == KeyEvent.VK_DOWN) move('s')
          if (e.getKeyCode == KeyEvent.VK_LEFT) move('a')
          if (e.getKeyCode == KeyEvent.VK_RIGHT) move('d')
          if (e.getKeyCode == KeyEvent.VK_SPACE) if (timetoexplosion == 0) move('x')
        }
        //permet de faire un restart quand notre jeu est gameover
        if (game_state == STATE_SHOWING_GAME_OVER) {
          if (e.getKeyCode == KeyEvent.VK_ENTER) {
            println("Enter pressed")
            game_state = STATE_PLAYING
          }
          // permet d'arreter le jeu
          if (e.getKeyCode == KeyEvent.VK_ESCAPE) {
            System.exit(-1)
          }
        }
      }
    })
  }

  // startplay est la fonction principale du jeu qui permet d'initialiser les variables qui en ont besoin
  def startplay(): Unit = {
    game_state = STATE_PLAYING
    timetoexplosion = 0
    grille = Array.ofDim(gridX, gridY)

    //on enregistre les positions initiales des ennemies
    memE1x = 2
    memE1y = 15
    memE2x = 5
    memE2y = 15
    memE3x = 9
    memE3y = 15
    memx = 0
    memy = 0


    initGrid()
    setWall()
    addPlayer()
    createEnemy()
    setRemovalWall(40)
    println(displayGrid())

    // on implemente une boucle for pour permettre au ennemies de se déplacer constantement
    while (game_state == STATE_PLAYING) {
      Thread.sleep(300)
      updateGraphics()
      moveenemy()
    }

    updateGraphics()

    // FIXME : comment this properly
    while (game_state == STATE_SHOWING_GAME_OVER) {
      Thread.sleep(300)
      println("In state STATE_SHOWING_GAME_OVER")
    }

  }

  //updateGraphics on FunGraphics
  def updateGraphics(): Unit = {
    val i1 = 50
    var NotfoundP = false
    // permet de commencer le compteur pour l'explosion de la bombe
    if (timetoexplosion >= 1) {
      timetoexplosion += 1
    }
    // explose la bombe
    if (timetoexplosion == 8) {
      explosion()
    }
    // on renitialise les cases explose par la bombe
    if (timetoexplosion == 10) {
      removeDamage()

    }
    display.syncGameLogic(60)
    //creations de tous les elemnts dans la fenetre
    display.frontBuffer.synchronized {
      display.clear()
      for (i <- 0 until gridX) {
        for (j <- 0 until gridY) {
          display.drawPicture(j * i1 + 25, 125 + i * i1, grass)
          if (grille(i)(j) == "2")
            NotfoundP = true
          grille(i)(j) match {
            case "0" => display.drawPicture(j * i1 + 25, 125 + i * i1, stoneWall)
            case "2" => display.drawPicture(j * i1 + 25, 125 + i * i1, bomberMan)
            case " " =>
            case "E" => display.drawPicture(j * i1 + 25, 125 + i * i1, enemyImg)
            case "W" => display.drawPicture(j * i1 + 25, 125 + i * i1, woodBox)
            case "X" => display.drawPicture(j * i1 + 25, 125 + i * i1, woodBox)
            case "B" => display.drawPicture(j * i1 + 25, 125 + i * i1, bomb)
            case "r" => display.drawPicture(j * i1 + 25, 125 + i * i1, explosionimg)

          }
        }

      }
    }
    // affichage du logo
    display.drawPicture(display.width / 2, 75, gameLogo)
    //permet d'afficher l'image du gameover quand on perd
    if (!NotfoundP && game_state == STATE_PLAYING) {
      game_state = STATE_SHOWING_GAME_OVER
    }

    if (game_state == STATE_SHOWING_GAME_OVER) {
      display.drawPicture(display.width / 2, display.height / 2, gameoverimg)
      display.drawString(740, 55, "Press Enter to restart", Color.BLUE, 20)

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

  //fonction qui implemente les murs non statiques et détruisable donc pas au début de tableau
  def setRemovalWall(much: Int): Unit = {
    var randomX = 0
    var randomY = 0
    var placesfound: Int = 0
    do {
      randomX = (Math.random() * gridX).toInt
      randomY = (Math.random() * gridY).toInt
      if (grille(randomX)(randomY) != "0" && grille(randomX)(randomY) != "2" && grille(randomX)(randomY) != "E" && (randomX!=1 && randomY!=2)) {
        placesfound += 1
        grille(randomX)(randomY) = "W"
      }
    } while (placesfound < much)
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

  //initialise le jouer
  def addPlayer(): Unit = {
    grille(1)(1) = "2"
  }

  //fonction qui initialise les ennemies
  def createEnemy(): Unit = {
    grille(memE1x)(memE1y) = "E"
    grille(memE2x)(memE2y) = "E"
    grille(memE3x)(memE3y) = "E"
  }

  //to move the man
  //fonction pour bouger le player
  def move(lettre: Char): Unit = {
    val read: Char = lettre
    var r = 0
    var c = 0
    // boucle pour trouver chaque fois la position du player
    for (i <- 0 until gridX) {
      for (j <- 0 until gridY) {
        if (grille(i)(j) == "2") {
          r = i
          c = j
        }
      }
    }
    // pour connecter les touches
    read match {
      case 'w' => if (grille(r - 1)(c) == " ") {
        grille(r - 1)(c) = "2"
        grille(r)(c) = " "

      }
      else {
        println("It's not possible")
      }
      case 's' => if (grille(r + 1)(c) == " ") {
        grille(r + 1)(c) = "2"
        grille(r)(c) = " "
      }
      else {
        println("It's not possible")
      }
      case 'a' => if (grille(r)(c - 1) == " ") {
        grille(r)(c - 1) = "2"
        grille(r)(c) = " "

      }
      else {
        println("It's not possible")
      }
      case 'd' => if (grille(r)(c + 1) == " ") {
        grille(r)(c + 1) = "2"
        grille(r)(c) = " "

      }
      else {
        println("It's not possible")
      }
      case 'x' =>
        //on place la bombe
        bomb(r, c)
        membombx = r
        membomby = c
      case _ =>
        println("Allowed touches are a s d w")

    }
    memx = r
    memy = c
  }


  //creation d'une bombe
  def bomb(x: Int, y: Int): Unit = {
    grille(x)(y) = "B"
    if (memx == 0 && memy == 0)
      grille(1)(2) = "2"
    else
      grille(memx)(memy) = "2"
    timetoexplosion = 1

  }


  //fonction qui explose la bombe
  def explosion(): Unit = {
    grille(membombx)(membomby) = "r"

    if (grille(membombx + 1)(membomby) != "0")
      grille(membombx + 1)(membomby) = "r"

    if (grille(membombx - 1)(membomby) != "0")
      grille(membombx - 1)(membomby) = "r"

    if (grille(membombx)(membomby + 1) != "0")
      grille(membombx)(membomby + 1) = "r"

    if (grille(membombx)(membomby - 1) != "0")
      grille(membombx)(membomby - 1) = "r"
  }

  //efface tout ce qu'il y a autour d'une bombe sauf les murs statiques
  def removeDamage(): Unit = {
    if (grille(membombx)(membomby) == "r" || grille(membombx)(membomby) == "W")
      grille(membombx)(membomby) = " "

    if (grille(membombx + 1)(membomby) == "r" || grille(membombx + 1)(membomby) == "W")
      grille(membombx + 1)(membomby) = " "

    if (grille(membombx - 1)(membomby) == "r" || grille(membombx - 1)(membomby) == "W")
      grille(membombx - 1)(membomby) = " "

    if (grille(membombx)(membomby + 1) == "r" || grille(membombx)(membomby + 1) == "W")
      grille(membombx)(membomby + 1) = " "

    if (grille(membombx)(membomby - 1) == "r" || grille(membombx)(membomby - 1) == "W")
      grille(membombx)(membomby - 1) = " "
    timetoexplosion = 0
  }


  //bouje les ennemies avec un random en terminant le jeu s'il trouve le player
  def moveenemy(): Unit = {
    var r = new Random()
    if (grille(memE1x)(memE1y) == "E") {
      r.nextInt(4) match {
        case 0 => if (grille(memE1x - 1)(memE1y) == " ") {
          grille(memE1x - 1)(memE1y) = "E"
          grille(memE1x)(memE1y) = " "
          memE1x = memE1x - 1
        }
        else if (grille(memE1x - 1)(memE1y) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case 1 => if (grille(memE1x + 1)(memE1y) == " ") {
          grille(memE1x + 1)(memE1y) = "E"
          grille(memE1x)(memE1y) = " "
          memE1x = memE1x + 1
        }
        else if (grille(memE1x + 1)(memE1y) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case 2 => if (grille(memE1x)(memE1y - 1) == " ") {
          grille(memE1x)(memE1y - 1) = "E"
          grille(memE1x)(memE1y) = " "
          memE1y = memE1y - 1
        }
        else if (grille(memE1x)(memE1y - 1) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case 3 => if (grille(memE1x)(memE1y + 1) == " ") {
          grille(memE1x)(memE1y + 1) = "E"
          grille(memE1x)(memE1y) = " "
          memE1y = memE1y + 1
        } else if (grille(memE1x)(memE1y + 1) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case _ =>

      }
    }
    if (grille(memE2x)(memE2y) == "E") {
      r.nextInt(4) match {
        case 0 => if (grille(memE2x - 1)(memE2y) == " ") {
          grille(memE2x - 1)(memE2y) = "E"
          grille(memE2x)(memE2y) = " "
          memE2x = memE2x - 1
        }
        else if (grille(memE2x - 1)(memE2y) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case 1 => if (grille(memE2x + 1)(memE2y) == " ") {
          grille(memE2x + 1)(memE2y) = "E"
          grille(memE2x)(memE2y) = " "
          memE2x = memE2x + 1
        }
        else if (grille(memE2x + 1)(memE2y) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case 2 => if (grille(memE2x)(memE2y - 1) == " ") {
          grille(memE2x)(memE2y - 1) = "E"
          grille(memE2x)(memE2y) = " "
          memE2y = memE2y - 1
        }
        else if (grille(memE2x)(memE2y - 1) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case 3 => if (grille(memE2x)(memE2y + 1) == " ") {
          grille(memE2x)(memE2y + 1) = "E"
          grille(memE2x)(memE2y) = " "
          memE2y = memE2y + 1
        } else if (grille(memE2x)(memE2y + 1) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case _ =>

      }
    }
    if (grille(memE3x)(memE3y) == "E") {
      r.nextInt(4) match {
        case 0 => if (grille(memE3x - 1)(memE3y) == " ") {
          grille(memE3x - 1)(memE3y) = "E"
          grille(memE3x)(memE3y) = " "
          memE3x = memE3x - 1
        }
        else if (grille(memE3x - 1)(memE3y) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case 1 => if (grille(memE3x + 1)(memE3y) == " ") {
          grille(memE3x + 1)(memE3y) = "E"
          grille(memE3x)(memE3y) = " "
          memE3x = memE3x + 1
        }
        else if (grille(memE3x + 1)(memE3y) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case 2 => if (grille(memE3x)(memE3y - 1) == " ") {
          grille(memE3x)(memE3y - 1) = "E"
          grille(memE3x)(memE3y) = " "
          memE3y = memE3y - 1
        }
        else if (grille(memE3x)(memE3y - 1) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case 3 => if (grille(memE3x)(memE3y + 1) == " ") {
          grille(memE3x)(memE3y + 1) = "E"
          grille(memE3x)(memE3y) = " "
          memE3y = memE3y + 1
        } else if (grille(memE3x)(memE3y + 1) == "2")
          game_state = STATE_SHOWING_GAME_OVER
        case _ =>

      }
    }
  }
}

