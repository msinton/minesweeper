package minesweeper

case class Grid(xLength: Int, yLength: Int)

object Grid {
  def explore(grid: Grid, mines: Mines, coord: Coord): Hits = {

    import ContainsCoord._
    import Mines._

    type Explored = Set[Coord]
    def loop(coords: Set[Coord], explored: Explored): Explored = {

      val adjacent = for {
        (x, y) <- coords
        if mines.zeroSurrounding((x, y))
        next <- Seq((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
        if grid.contains(next)
        if !explored.contains(next)
      } yield next

      if (adjacent.isEmpty) explored ++ coords
      else loop(adjacent, explored ++ coords)
    }

    loop(Set(coord), Set())
  }
}

trait GridLike[A] {
  def dimensions(a: A): Dimensions

  def size(a: A): Int = {
    val (xLength, yLength) = dimensions(a)
    xLength * yLength
  }
}

object GridLike {

  def instance[A](b: A => Dimensions): GridLike[A] = new GridLike[A] {
    def dimensions(a: A): Dimensions = b(a)
  }

  implicit val gridToGridLike: GridLike[Grid] =
    GridLike.instance(grid => (grid.xLength, grid.yLength))

  implicit class GridSyntax[A](a: A) {
    def dimensions(implicit ev: GridLike[A]): Dimensions =
      ev.dimensions(a)

    def size(implicit ev: GridLike[A]): Int =
      ev.size(a)
  }
}
