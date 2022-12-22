object GamePlay extends App {
  val grid1 = new Grid()

  grid1.initGrid()
  grid1.setWall()
  grid1.addPlayer()
  grid1.setRemovalWall()
  grid1.createEnemy(4)
  println(grid1.displayGrid())
}
