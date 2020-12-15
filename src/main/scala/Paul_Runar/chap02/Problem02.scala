package Paul_Runar.chap02

object Problem02 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(idx: Int): Boolean = {
      if (idx >= as.length - 1) true
      else if (!ordered(as(idx), as(idx+1))) false
      else loop(idx + 1)
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    val a = Array[Int](1, 2, 3, 4, 5)
    val b = Array[Int](1, 2, 3, 4, 3)
    val c = Array[Int]()
    val d = Array[Int](1)

    assert(isSorted(a, (_: Int)<(_: Int)))
    assert(!isSorted(b, (_: Int)<(_: Int)))
    assert(isSorted(c, (_: Int)<(_: Int)))
    assert(isSorted(d, (_: Int)<(_: Int)))
    assert(!isSorted(Array(1, 4, 3), (x: Int, y: Int) => x < y))
  }
}
