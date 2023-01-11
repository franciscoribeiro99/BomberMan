object GamePlay extends App {

  val grid1 = new Grid()
  grid1.initGrid()
  grid1.setWall()
  grid1.addPlayer()
  grid1.createEnemy()
  grid1.setRemovalWall(10)
  println(grid1.displayGrid())
  while (!grid1.gameover) {
    Thread.sleep(300)
    grid1.updateGraphics()
    grid1.moveenemy()
  }
  grid1.updateGraphics()

}
