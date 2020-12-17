package Paul_Runar.chap03

object Problem19 {
  @scala.annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((l: List[A], a: A) => Cons(a, l))

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((x, y) => f(y, x))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]())((i, acc) => if (f(i)) Cons(i, acc) else acc)
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 4, 3, 6, 2, 1, 7, 3, 4)
    println(filter(a)(_ < 5))
  }
}
