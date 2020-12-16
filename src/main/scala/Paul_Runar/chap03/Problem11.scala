package Paul_Runar.chap03

object Problem11 {
  @scala.annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product(l: List[Int]): Int =
    foldLeft(l, 1)(_ * _)

  def listLength[A](l: List[A]): Int =
    foldLeft(l, 0)((x: Int, _) => x + 1)

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 4, 5, 6)
    assert(sum(a) == 21)
    assert(product(a) == 720)
    assert(listLength(a) == 6)
  }
}
