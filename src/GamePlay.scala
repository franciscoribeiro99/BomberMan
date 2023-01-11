object GamePlay extends App {
  val grid1 = new Grid()
  grid1.initGrid()
  grid1.setWall()
  grid1.addPlayer()
  grid1.createEnemy(4)
  grid1.setRemovalWall()
  println(grid1.displayGrid())
  grid1.updateGraphics()

}
