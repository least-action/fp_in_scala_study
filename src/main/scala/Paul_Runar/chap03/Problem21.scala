package Paul_Runar.chap03

object Problem21 {
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

  def append[A](a: List[A], b: List[A]): List[A] =
    foldRight(a, b)(Cons(_, _))


  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B])((a, acc) => append(f(a), acc))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 4, 3, 6, 2, 78, 2)
    println(filter(a)(_ < 5))
  }
}
