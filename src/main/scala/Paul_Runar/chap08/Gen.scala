package Paul_Runar.chap08
import Paul_Runar.chap06.State
import Paul_Runar.chap06.RNG
import Prop._
import Gen._


object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n , rng)
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed
          else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception : ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
         maxSize: Int = 100,
         testCases: Int = 100,
         rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x => x
    }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(f, s) => Falsified(msg + '\n' + f, s)
      case x => x
    }
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => Gen.listOfN(i, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val diff = stopExclusive - start
    Gen(State(RNG.nonNegativeInt).map(i => start + i % diff))
  }

  def unit[A](a: A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ > 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    i => g.listOfN(i)
  }

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

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)
}
