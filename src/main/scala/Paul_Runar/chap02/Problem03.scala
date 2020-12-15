package Paul_Runar.chap02

object Problem03 {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => (b => f(a, b))

  def func(a: Int, b: Int): Int = {
    a + b
  }

  def main(args: Array[String]): Unit = {
    val newFunc = curry(func)(2)
    assert(newFunc(3) == 5)
  }
}
