package Paul_Runar.chap03

object Problem17 {
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

  def doubleToString(li: List[Double]): List[String] = {
    foldRight(li, List[String]())((d, acc) => Cons('"' + d.toString + '"' , acc))
  }

  def main(args: Array[String]): Unit = {
    val a = List(1.2, 5.6, 1.23)
    println(doubleToString(a))
  }
}
