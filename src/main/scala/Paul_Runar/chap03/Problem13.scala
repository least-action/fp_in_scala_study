package Paul_Runar.chap03

object Problem13 {
  @scala.annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  }

  def foldRight1[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((x, y) => f(y, x))
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((l: List[A], a: A) => Cons(a, l))

  def rsum(l: List[Int]): Int =
    foldRight(l, 0)(_ + _)

  def rsum1(l: List[Int]): Int = {
    foldRight1(l, 0)((x, y) => x + y)
  }

  def lsum(l: List[Int]): Int = {
    foldLeft(l, 0)((x, y) => x + y)
  }

  def zeroList(n: Int): List[Int] = {
    if (n == 0) Nil
    else Cons(0, zeroList(n-1))
  }

  def main(args: Array[String]): Unit = {
    val a = zeroList(7000)
//    println(rsum(a))
    println(rsum1(a))
    println(lsum(a))
  }
}
