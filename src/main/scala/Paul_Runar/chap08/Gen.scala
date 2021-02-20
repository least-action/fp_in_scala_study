package Paul_Runar.chap08
import Paul_Runar.chap06.State
import Paul_Runar.chap06.RNG

import Paul_Runar.chap08.Prop.{FailedCase, SuccessCount}

import scala.util.Success


object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = {
    ???
  }
  def &&(p: Prop): Prop = ???
}

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => Gen.listOfN(i, this))
}

object Gen {
  def listOf[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val diff = stopExclusive - start
    Gen(State(RNG.nonNegativeInt).map(i => start + i % diff))
  }

  def unit[A](a: A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ > 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    def weightedSelect(d1: Double, d2: Double)(d: Double): Boolean = {
      val r = d % (d1 + d2)
      if (r < d1) true
      else false
    }
    Gen(State(RNG.double).map(weightedSelect(g1._2, g2._2))).flatMap((b: Boolean) => if (b) g1._1 else g2._1)
  }

}
