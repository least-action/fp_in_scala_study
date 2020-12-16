package Paul_Runar.chap03

object Problem09 {
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, b) => b + 1)
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 5, 2, 3, 4)
    assert(length(a) == 5)
  }
}
