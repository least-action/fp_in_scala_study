package Paul_Runar.chap03

object Problem04 {
  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) =>
      if (n <= 0) l
      else drop(tail, n-1)
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 4, 5)
    println(drop(a, 0))
    println(drop(a, 1))
    println(drop(a, 2))
    println(drop(a, 3))
    println(drop(a, 4))
    println(drop(a, 5))
  }
}
