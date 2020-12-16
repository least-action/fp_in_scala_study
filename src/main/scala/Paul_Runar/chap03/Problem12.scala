package Paul_Runar.chap03

object Problem12 {
  @scala.annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((x: List[A], y: A) => Cons(y, x))
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 4, 5)
    println(reverse(a))
  }
}
