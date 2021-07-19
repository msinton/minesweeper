package object minesweeper {
  type Coord = (Int, Int)
  type Dimensions = (Int, Int)
  type Hits = Set[Coord]
  type Mine = Coord
  type Mines = Set[Mine]
  type NumMines = Int

}
