package Paul_Runar.chap03

object Problem10 {
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @scala.annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def rsum(l: List[Int]): Int = {
    foldRight(l, 0)((x, y) => x + y)
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
//    println(rsum(a))  // stack over flow
    println(lsum(a))
  }
}
