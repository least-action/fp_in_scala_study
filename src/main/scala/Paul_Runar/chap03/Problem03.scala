package Paul_Runar.chap03

object Problem03 {
  def setHead[A](li: List[A], x: A): List[A] = li match {
    case Nil => Nil
    case Cons(_, tail) => Cons(x, tail)
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 4, 5)
    println(setHead(a, 3))
  }
}
