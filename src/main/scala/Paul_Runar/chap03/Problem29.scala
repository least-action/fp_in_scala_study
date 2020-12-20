package Paul_Runar.chap03

object Problem29 {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size[A](t: Tree[A]): Int =
    fold(t)((x: A) => 1)(_ + _ + 1)

  def maximum(t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def depth[A](t: Tree[A]): Int =
    fold(t)((x: A) => 0)((x, y: Int) => 1 + (x max y))

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)((x: A) => Leaf(f(x)): Tree[B])((x, y: Tree[B]) => Branch(x, y))

  def main(args: Array[String]): Unit = {
    val t1 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6))))
    val f = (x: Int) => x + 1

    assert(size(t1) == 7)
    assert(maximum(t1) == 4)
    assert(depth(t1) == 2)
    assert(depth(t2) == 3)
    println(map(t1)(f))
  }
}
