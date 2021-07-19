package minesweeper

trait Show[T] {
  def show(t: T): String
}

object Show {

  def apply[T](implicit ev: Show[T]): Show[T] = ev

  def instance[T](b: T => String): Show[T] = new Show[T] {
    def show(t: T) = b(t)
  }

  implicit class ShowSyntax[A](a: A) {
    def show(implicit ev: Show[A]): String = ev.show(a)
  }

  implicit val showGameState: Show[GameState] =
    instance(GameStateShow.show)
}
