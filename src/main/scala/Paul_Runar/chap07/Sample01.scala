package Paul_Runar.chap07

object Sample01 extends App {
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      sum(l) + sum(r)
    }

  println(sum(IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
}
