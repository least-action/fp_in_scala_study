package Paul_Runar.chap06

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)
//  type Rand[A] = State[RNG, A]

  def int(rng: RNG): (Int, RNG) =
    rng.nextInt

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i+1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (j, r2) = double(r1)
    ((i, j), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (i, r1) = double(rng)
    val (j, r2) = double(r1)
    val (k, r3) = double(r2)
    ((i, j, k), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def loop(c: Int, r: RNG, l: List[Int]): (List[Int], RNG) = {
      if (c <= 0) (l, r)
      else {
        val (i, rr) = r.nextInt
        loop(c-1, rr, i :: l)
      }
    }
    loop(count, rng, Nil)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def randIndDouble: Rand[(Int, Double)] =
    both(int, double)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def sequence_[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => fs.foldRight((List[A](), rng))((r, z) => {
      val (a, rr) = r(z._2)
      (a :: z._1, rr)
    })
  }

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (i, r) = f(rng)
      g(i)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def nonNegativeLessThan_(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => rng => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) (mod, rng)
      else nonNegativeLessThan(n)(rng)
    })

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => (f(a), _))

  def doubleViaMapViaFlatMap: Rand[Double] =
    mapViaFlatMap(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def bothViaMap2ViaFlatMap[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2ViaFlatMap(ra, rb)((_, _))

  def randIndDoubleViaFlatMapBoth: Rand[(Int, Double)] =
    bothViaMap2ViaFlatMap(int, double)
}