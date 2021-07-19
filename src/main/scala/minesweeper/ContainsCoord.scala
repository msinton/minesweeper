package minesweeper

trait ContainsCoord[A] {

  def contains(a: A, c: Coord): Boolean
}

object ContainsCoord {

  def apply[A](implicit ev: ContainsCoord[A]): ContainsCoord[A] = ev

  def instance[A](b: (A, Coord) => Boolean): ContainsCoord[A] =
    new ContainsCoord[A] {
      def contains(a: A, c: Coord): Boolean = b(a, c)
    }

  implicit val gridToContainsCoord: ContainsCoord[Grid] = instance(
    (grid, coord) => {
      val (x, y) = coord
      val Grid(xLength, yLength) = grid
      x >= 0 && x < xLength && y >= 0 && y < yLength
    }
  )

  implicit class ContainsCoordSyntax[A](a: A) {
    def contains(c: Coord)(implicit ev: ContainsCoord[A]): Boolean =
      ev.contains(a, c)
  }
}
