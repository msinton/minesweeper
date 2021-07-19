package minesweeper

sealed trait EndState
case object Win extends EndState
case object Lose extends EndState

object EndStateShow {

  def showWin =
    """
      |--------- You win! ---------
      |
      |          （ ^_^）
    """.stripMargin

  def showLose =
    """
      |--------- You lose ---------
      |
      |         ¯\_(ツ)_/¯
    """.stripMargin

  def show(endState: EndState) = endState match {
    case Lose => showLose
    case Win  => showWin
  }
}
