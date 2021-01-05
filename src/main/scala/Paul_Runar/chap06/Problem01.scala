package Paul_Runar.chap06

object Problem01 extends App {
  def nonNegativeInt_(rng: RNG): (Int, RNG) = {
    val r = rng.nextInt
    if (r._1 == Int.MinValue) (Int.MaxValue, r._2)
    else (math.abs(r._1), r._2)
  }

  val a = SimpleRNG(-123)
  println(RNG.nonNegativeInt(a))
}
