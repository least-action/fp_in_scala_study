package Paul_Runar.chap03

object Problem02 {
  def tail[A](li: List[A]): List[A] = li match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }
  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 4, 5)
    println(tail(a))
  }
}