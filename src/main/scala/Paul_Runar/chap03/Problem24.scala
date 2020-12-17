package Paul_Runar.chap03

object Problem24 {
  @scala.annotation.tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(x, xs), Cons(y, ys)) =>
      if (x == y) startsWith(xs, ys)
      else false
  }

  @scala.annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, tail) => hasSubsequence(tail, sub)
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2)
    val b = List(2, 3)
    val c = List(4)
    val d = List(1, 3)
    val x = List(1, 2, 3, 4)
    assert(hasSubsequence(x, a))
    assert(hasSubsequence(x, b))
    assert(hasSubsequence(x, c))
    assert(!hasSubsequence(x, d))
  }
}
