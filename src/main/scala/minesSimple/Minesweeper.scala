package minesSimple

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Random, Try}

object Minesweeper extends App {

  type Hits = Set[(Int, Int)]
  type Mines = Set[(Int, Int)]
  type Coord = (Int, Int)

  val xLength = 16
  val yLength = 15
  val numMines = 26

  val random = new Random()
  val mines = setMines(numMines)
  val numBoxes = xLength * yLength

  def setMines(numMines: Int): Set[(Int, Int)] = {
    if (numMines < 1 || numMines > xLength * yLength) Set()
    else {
      val r = (0 until numMines)
        .map(_ => random.nextInt(xLength))
        .map(x => (x, random.nextInt(yLength)))
        .toSet
      r ++ setMines(numMines - r.size)
    }
  }

  def calcNum(x: Int, y: Int) =
    (for {
      i <- x - 1 to x + 1
      j <- y - 1 to y + 1
      hit = mines.contains((i, j))
      n = if (hit) 1 else 0
    } yield n).sum

  def hitMine(c: Coord) = mines.contains(c)

  val isZero = (calcNum _).tupled andThen (_ == 0)

  def explode(coord: Coord): Hits = {

    type Checked = Set[Coord]
    def loop(coord: Coord, checked: Checked, zeros: Hits): (Checked, Hits) = {
      val (x, y) = coord

      val adjacent = Seq((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
        .filter((inGrid _).tupled)
        .filterNot(checked.contains)

      if (adjacent.isEmpty) (checked, zeros)
      else
        adjacent
          .filter(isZero)
          .foldLeft((checked ++ adjacent, zeros)) {
            case ((checked, zeros), coord) =>
              loop(coord, checked, zeros + coord)
          }
    }

    loop(coord, Set(coord), Set(coord))._1
  }

  def inGrid(x: Int, y: Int) =
    x >= 0 && x < xLength && y >= 0 && y < yLength

  def repr(hits: Hits) =
    (0 until yLength).map { y =>
      for {
        x <- 0 until xLength
        h = if (hits.contains((x, y))) showNumOrMine(x, y) else " "
      } yield h
    }

  def showNumOrMine(x: Int, y: Int) = {
    if (mines.contains((x, y))) "x" else calcNum(x, y).toString
  }

  def show(hits: Hits) =
    repr(hits).map(_.mkString("|"))

  def nextHits(x: Int, y: Int): Hits = {
    val coord = (x, y)
    val exploded = if (isZero(coord)) explode(coord) else Set.empty[Coord]
    exploded + coord
  }

  sealed trait Status
  case object Win extends Status
  case object Lose extends Status
  
  def nextState(x: Int, y: Int, prevHits: Hits): (Hits, Option[Status]) = {
    val hits = if (inGrid(x, y)) prevHits ++ nextHits(x, y) else prevHits
    if (hitMine(x, y)) {
      (Set(), Some(Lose))
    } else if (hits.size == numBoxes - numMines) {
      (Set(), Some(Win))
    } else
      (hits, None)
  }

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

  def showGrid(hits: Hits) = {
    "\t" + (0 until xLength)
      .map(x => s"$x".padTo(2, " ").mkString) + "\n".mkString + "\n" +
      show(hits).zipWithIndex
        .map {
          case (line, n) => s"$n\t$line"
        }
        .mkString("\n")
  }

  def renderGrid(hits: Hits) = {
    println()
    println(showGrid(hits))
  }

  val renderStatus: Status => Unit = {
    case Win  => println(showWin)
    case Lose => println(showLose)
  }

  def render(hits: Hits, status: Option[Status]): Unit = {
    renderGrid(hits)
    status.map(renderStatus)
    println()
  }

  @tailrec
  def gameLoop(prevState: (Hits, Option[Status])): Hits = {
    (render _).tupled(prevState)
    print("x: ")
    val x = Try(readLine().toInt).getOrElse(-1)
    print("y: ")
    val y = Try(readLine().toInt).getOrElse(-1)
    val state = nextState(x, y, prevState._1)
    (render _).tupled(state)
    gameLoop(state)
  }

  println("------------ Minesweeper -------------")
  gameLoop((Set(), None))

}
