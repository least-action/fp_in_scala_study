package Paul_Runar.chap05

object Problem10 extends App {
  def fibs: Stream[Int] = {
    def loop(cur: Int, next: Int): Stream[Int] =
      Stream.cons(cur, loop(next, next+cur))
    loop(0, 1)
  }

  assert(fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
}
