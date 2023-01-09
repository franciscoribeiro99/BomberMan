import scala.util.Random

class Grid {
  //implemente la zone de jeu 12x7
  val gridX = 13
  val gridY = 18
  val grille: Array[Array[String]] = Array.ofDim(gridX, gridY)

  //initialise la grille
  def initGrid(): Unit = {
    for (i <- 0 until gridX)
      for (j <- 0 until gridY) {
        grille(i)(j) = "x"
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
        grille(randomX)(randomY) = "1"
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
    val r = new Random()
    var actualtime = 0
    for (i <- grille.indices; j <- (grille(i).length / 2) until grille(i).length) {

      if (actualtime < times) {
        if (grille(i)(j) == "x" && r.nextInt(2) == 0) {
          actualtime += 1
          grille(i)(j) = "E"
        }
      }
    }
  }

  //to move the man
  def move(): Unit = {
    var read: Char = Input.readChar()
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
      case 'w' => if (grille(r - 1)(c) == "x") {
        grille(r - 1)(c) = "2"
        grille(r)(c) = "x"
      }
      else {
        println("It's not possible")
      }
      case 's' => if (grille(r + 1)(c) == "x") {
        grille(r + 1)(c) = "2"
        grille(r)(c) = "x"
      }
      else {
        println("It's not possible")
      }
      case 'a' => if (grille(r)(c - 1) == "x") {
        grille(r)(c - 1) = "2"
        grille(r)(c) = "x"
      }
      else {
        println("It's not possible")
      }
      case 'd' => if (grille(r)(c + 1) == "x") {
        grille(r)(c + 1) = "2"
        grille(r)(c) = "x"
      }
      else {
        println("It's not possible")
      }
      case _ => println("Allowed touches are a s d w")
    }

  }


}
