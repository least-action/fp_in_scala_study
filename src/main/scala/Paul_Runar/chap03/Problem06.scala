package Paul_Runar.chap03

object Problem06 {
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 4)
    println(init(a))
  }
}
