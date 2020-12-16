package Paul_Runar.chap03

object Problem07 {
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum(l: List[Int]): Int = {
    foldRight(l, 0)((x, y) => x + y)
  }

  def product(l: List[Int]): Int = {
    foldRight(l, 1)(_ * _)
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 4, 5)
    assert(sum(a) == 15)
    assert(product(a) == 120)
  }
}
