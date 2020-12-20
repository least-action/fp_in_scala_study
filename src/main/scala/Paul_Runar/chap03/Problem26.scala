package Paul_Runar.chap03

object Problem26 {
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    assert(maximum(t) == 4)
  }
}
