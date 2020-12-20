package Paul_Runar.chap03

object Problem27 {
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def main(args: Array[String]): Unit = {
    val t1 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6))))
    assert(depth(t1) == 2)
    assert(depth(t2) == 3)
  }
}
