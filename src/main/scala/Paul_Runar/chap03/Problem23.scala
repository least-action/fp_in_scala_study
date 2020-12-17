package Paul_Runar.chap03

object Problem23 {
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

  def zipWith(a: List[Int], b: List[Int])(f: (Int, Int) => Int): List[Int] = {
    @scala.annotation.tailrec
    def loop(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = a match {
      case Nil => acc
      case Cons(x, xs) => b match {
        case Nil => acc
        case Cons(y, ys) => loop(xs, ys, Cons(f(x, y), acc))
      }
    }
    reverse(loop(a, b, Nil: List[Int]))
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3)
    val b = List(5, 7, 9)
    println(zipWith(a, b)(_ + _))
    println(zipWith(a, b)(_ * _))
  }
}
