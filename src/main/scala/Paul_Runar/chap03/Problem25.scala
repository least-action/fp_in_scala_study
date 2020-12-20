package Paul_Runar.chap03

object Problem25 {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    assert(size(t) == 7)
  }
}
