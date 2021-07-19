package state

import cats.data.State

final case class Seed(long: Long) {
  def next = Seed((long * 0X5DEECE66DL + 0XBL) & 0XFFFFFFFFFFFFL)
}

object Rand {

  type RNG[A] = State[Seed, A]

  def pure[A](a: A): RNG[A] = State.pure[Seed, A](a)

  val nextInt: RNG[Int] = State(
    seed => (seed.next, (seed.long >>> 16).toInt)
  )

  val nonNegativeInt: RNG[Int] =
    nextInt.map(i => if (i < 0) -(i + 1) else i)

  def nonNegativeLessThan(n: Int): RNG[Int] = nonNegativeInt.flatMap { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) State(seed => (seed, mod))
    else nonNegativeLessThan(n)
  }
}
