package minesweeper

import state._
import state.Rand._
import cats.implicits._
import cats._
import cats.data._

import scala.io.StdIn

import scala.util.Try

case class Config(xLength: Int, yLength: Int, numMines: NumMines)

object Minesweeper {
  import GridLike._
  import Show._

  val beginner = Config(xLength = 4, yLength = 4, numMines = 2)
  val intermediate = Config(xLength = 6, yLength = 5, numMines = 12)
  val hard = Config(xLength = 10, yLength = 10, numMines = 27)

  def selectDifficulty(): Config =
    readLine(s"""Select difficulty:

    |b Beginner      ${beginner.numMines} mines
    |i Intermediate  ${intermediate.numMines} mines
    |h Hard          ${hard.numMines} mines
    |
    """.stripMargin) match {
      case "b" => beginner
      case "i" => intermediate
      case "h" => hard
      case _   => beginner
    }

  def getInput(): Coord =
    Try {
      val x = readLine("x: ").toInt
      (x, readLine("y: ").toInt)
    }.getOrElse((-1, -1))

  def newGame(config: Config): RNG[GameState] = {
    val Config(xLength, yLength, numMines) = config

    val grid = Grid(xLength = xLength, yLength = yLength)
    for {
      mines <- Mines.setup(grid, numMines)
      gameState = GameState(grid, mines, Set(), None)
    } yield gameState
  }

  def restartWhenEnded(gameState: GameState): RNG[GameState] =
    State(
      rng =>
        if (gameState.endState.isDefined)
          newGame(selectDifficulty()).run(rng).value
        else (rng, gameState)
    )

  def gameLoop(): RNG[GameState] = {

    val updateLoop: GameState => GameState = gameState => {
      val nextState = gameState.update(getInput())
      println()
      println(nextState.show)
      nextState
    }

    val iteration: RNG[GameState] => RNG[GameState] = initialState =>
      for {
        gameState <- initialState
        nextState = updateLoop(gameState)
        a <- restartWhenEnded(nextState)
      } yield a

    def go(in: RNG[GameState]): RNG[GameState] =
      iteration(in).flatMap(nextState => go(in.map(_ => nextState)))

    go(newGame(selectDifficulty()))
  }

  type RNGGameState = (Seed, GameState)

  type MyState[T] = State[RNGGameState, T]

  def loop(in: MyState[Unit]): MyState[Unit] = {
    in flatMap (_ => loop(in))
  }

}

object Game extends App {
  import Minesweeper._

  // gameLoop().run(state.Seed(1))

  val start = newGame(selectDifficulty())
  loop(for {
    newState <- start
    seed <- start.get
    result <- State.set((seed, newState))
  } yield result).run(state.Seed(1))

}
