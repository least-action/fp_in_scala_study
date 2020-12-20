package Paul_Runar.chap03

object Problem28 {
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  def main(args: Array[String]): Unit = {
    val t1 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val f = (x: Int) => x + 1
    println(map(t1)(f))
  }
}
