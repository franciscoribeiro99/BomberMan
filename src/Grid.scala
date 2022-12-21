class Grid {
  //implemente la zone de jeu 12x7
  val gridX = 9
  val gridY = 11
  val grille: Array[Array[String]] = Array.ofDim(gridX, gridY)

  def initGrid(): Unit = {
    for (i <- 0 until gridX)
      for (j <- 0 until gridY) {
        grille(i)(j) = "x"
      }
  }

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

  //fonction qui implemente les murs non statiques non détruisable donc pas au début de tableau--> marta
  def setRemovalWall(): Unit = {
    var randomX=0
    var randomY=0
    var placesfound: Int = 0
    do {
      randomX=(Math.random()*gridX).toInt
      randomY=(Math.random()*gridY).toInt
     if(grille(randomX)(randomY)!= "0"){
       placesfound+=1
       grille(randomX)(randomY)="1"
     }
    } while (placesfound < 5)
  }


  //fonction qui implemente les murs statiques --> Francisco
  def setWall(): Unit = {
    for (i <- 0 until gridX) {
      for (j <- 0 until gridY) {
        if (i == 0 || j == 0 || i == gridX - 1 || j == gridY - 1 || i % 2 == 0 && j % 2 == 0)
          grille(i)(j) = "0"
      }
    }

  }


}
