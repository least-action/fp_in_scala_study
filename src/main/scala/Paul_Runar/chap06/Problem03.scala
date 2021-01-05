package Paul_Runar.chap06

object Problem03 extends App {
  val a = SimpleRNG(42)

  println(RNG.intDouble(a))
  println(RNG.doubleInt(a))
  println(RNG.double3(a))
}
