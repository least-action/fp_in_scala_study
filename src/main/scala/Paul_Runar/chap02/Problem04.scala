package Paul_Runar.chap02

object Problem04 {
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def func(x: Int): (Int => Int) =
    y => x + y

  def main(args: Array[String]): Unit = {
    assert(func(1)(2) == 3)

    val a = uncurry(func)
    assert(a(1, 2) == 3)
  }
}
