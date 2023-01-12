object GamePlay extends App {
  val grid1 = new Grid()
  do{
    grid1.startplay()
    println(grid1.displayGrid())
  }while(grid1.replay==true && grid1.gameover==true)


}
