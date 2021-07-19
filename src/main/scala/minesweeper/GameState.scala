package minesweeper

case class GameState(
    grid: Grid,
    mines: Mines,
    hits: Hits,
    endState: Option[EndState]
) {

  import GridLike._

  def isWon =
    hits.size == grid.size - mines.size

  def isLost(input: Coord) =
    mines.contains(input)

  def update(input: Coord): GameState = {
    val newHits = hits ++ Grid.explore(grid, mines, input)

    val newEndState =
      if (isLost(input))
        Some(Lose)
      else if (isWon)
        Some(Win)
      else
        None

    this.copy(hits = newHits, endState = newEndState)
  }
}

object GameStateShow {

  def show(gameState: GameState) = {
    showGameGrid(gameState) + "\n" +
      gameState.endState.fold("")(EndStateShow.show)
  }

  def showGameGrid(gameState: GameState) = {
    import gameState._

    showXAxis(grid) + "\ny:\n" +
      rows(gameState).zipWithIndex
        .map { case (row, index) => s" $index\t$row" }
        .mkString("\n")
  }

  def showXAxis(grid: Grid) =
    "x:\t" + (0 until grid.xLength)
      .map(x => x.toString.padTo(2, " ").mkString)
      .mkString

  def rows(gameState: GameState) = {
    import gameState._
    import grid._

    (0 until yLength)
      .map(y => (0 until xLength).map((_, y)))
      .map(showRow(mines, hits))
  }

  def showRow(mines: Mines, hits: Hits)(coords: Seq[Coord]) =
    coords
      .map {
        case coord if hits.contains(coord) => showHit(mines, coord)
        case _                             => showUnexplored
      }
      .mkString("|")

  def showHit(mines: Mines, coord: Coord) = {
    import Mines._
    if (mines.contains(coord)) showMine
    else mines.surrounding(coord).toString
  }

  def showMine = "x"

  def showUnexplored = " "

}
