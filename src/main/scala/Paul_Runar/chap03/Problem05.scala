package Paul_Runar.chap03

object Problem05 {
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else l
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 5, 1, 2, 3)
    println(dropWhile(a, (_: Int) < 5))
  }
}
