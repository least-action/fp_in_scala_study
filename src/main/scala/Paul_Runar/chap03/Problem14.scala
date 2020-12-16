package Paul_Runar.chap03

object Problem14 {
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def append[A](l: List[A], x: A): List[A] =
    foldRight(l, Cons(x, Nil))(Cons(_, _))

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 4, 5)
    println(append(a, 6))
  }
}
