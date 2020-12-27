package Paul_Runar.chap05

object Problem09 extends App {
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n+1))

  assert(from(3).take(3).toList == List(3, 4, 5))
}
