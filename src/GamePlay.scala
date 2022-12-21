object GamePlay extends App {
  val grid1 = new Grid()
  grid1.initGrid()
  grid1.setWall()
  grid1.setRemovalWall()
  println(grid1.displayGrid())
}
