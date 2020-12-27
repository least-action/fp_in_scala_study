package Paul_Runar.chap05

object Problem08 extends App {
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def constant_[A](a: A): Stream[A] =
    Stream.cons(a, constant_(a))

  assert(constant("hi").take(3).toList == List("hi", "hi", "hi"))
}
