object GamePlay extends App {
  val grid1 = new Grid()

  while (true) {
    // Runs the game once
    grid1.startplay()
    println(grid1.displayGrid())
  }

}
