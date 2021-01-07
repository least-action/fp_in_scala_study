package Paul_Runar.chap06

object Problem09 extends App {
  val a = SimpleRNG(42)
  println(RNG.doubleViaMapViaFlatMap(a))
  println(RNG.randIndDoubleViaFlatMapBoth(a))
}
