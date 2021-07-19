package minesweeper

trait MinesLike[A] {

  def contains(a: A, coord: Coord): Boolean

  def surrounding(mines: A, coord: Coord): Int = {
    val (x, y) = coord
    (for {
      i <- x - 1 to x + 1
      j <- y - 1 to y + 1
      hit = contains(mines, (i, j))
      n = if (hit) 1 else 0
    } yield n).sum
  }

  def zeroSurrounding(mines: A, coord: Coord): Boolean =
    surrounding(mines, coord) == 0

}

object Mines {

  def apply[A](implicit ev: MinesLike[A]): MinesLike[A] = ev

  def instance[A](b: (A, Coord) => Boolean): MinesLike[A] = new MinesLike[A] {
    def contains(a: A, coord: Coord): Boolean = b(a, coord)
  }

  implicit val setToMines: MinesLike[Mines] = Mines.instance {
    _ contains _
  }

  implicit class MinesSyntax[A](a: A) {

    def surrounding(coord: Coord)(implicit ev: MinesLike[A]): Int =
      ev.surrounding(a, coord)

    def zeroSurrounding(coord: Coord)(implicit ev: MinesLike[A]): Boolean =
      ev.zeroSurrounding(a, coord)

  }

  import state.Rand
  import Rand._

  def setup(grid: Grid, numMines: NumMines): RNG[Mines] = {

    import GridLike._

    val nextInt = Rand.nonNegativeLessThan _

    val nextMine: RNG[Mine] = for {
      x <- nextInt(grid.xLength)
      y <- nextInt(grid.yLength)
    } yield (x, y)

    def nextDistinctMine(existing: Mines): RNG[Mine] =
      for {
        attempt <- nextMine
        mine <- if (existing.contains(attempt)) nextDistinctMine(existing)
        else Rand.pure(attempt)
      } yield mine

    def mines(numMines: Int, s: RNG[Mines]): RNG[Mines] =
      for {
        _mines <- s
        result <- if (numMines == 0 || _mines.size >= grid.size)
          Rand.pure(_mines)
        else mines(numMines - 1, nextDistinctMine(_mines).map(_mines + _))
      } yield result

    mines(numMines, Rand.pure(Set.empty[Mine]))
  }
}
