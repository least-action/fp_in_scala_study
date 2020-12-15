package Paul_Runar.chap02

object Problem05 {
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def func1(x: Int): Int = x * 2
  def func2(x: Int): Int = x + 3

  def main(args: Array[String]): Unit = {
    val newFunc = compose(func1, func2)
    assert(newFunc(3) == 12)
  }
}
