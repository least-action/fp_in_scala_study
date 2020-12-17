package Paul_Runar.chap03

object Problem15 {
  @scala.annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((l: List[A], a: A) => Cons(a, l))

  def merge[A](l: List[List[A]]): List[A] = {
    foldLeft(reverse(l), List[A]())((acc, li) => foldLeft(reverse(li), acc)((acc, li) => Cons(li, acc)))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((x, y) => f(y, x))
  }

  def merge2[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]())((li, acc) => foldRight(li, acc)(Cons(_, _)))
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3)
    val b = List(4, 5, 6)
    val c = List(7, 8, 9)
    println(merge(List(a, b, c)))
    println(merge2(List(a, b, c)))
  }
}