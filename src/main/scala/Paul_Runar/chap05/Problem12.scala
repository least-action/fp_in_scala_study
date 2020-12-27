package Paul_Runar.chap05

object Problem12 extends App {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => Stream.cons(h, unfold(s)(f))
    case None => Stream.empty
  }

  val fibsViaUnfold: Stream[Int] = unfold((0, 1)){case (f0, f1) => Some((f0, (f1, f0 + f1)))}
  val fibsViaUnfold2: Stream[Int] = unfold((0, 1))(x => Some((x._1, (x._2, x._1 + x._2))))

  def fromViaUnfold(n: Int) = unfold(n)(x => Some((x, x+1)))

  def constantViaUnfold[A](a: A) = unfold(a)(_ => Some((a, a)))

  val onesViaUnfold = unfold(1)(_ => Some((1, 1)))

  assert(onesViaUnfold.take(3).toList == List(1, 1, 1))
  assert(constantViaUnfold("hi").take(3).toList == List("hi", "hi", "hi"))
  assert(fromViaUnfold(3).take(3).toList == List(3, 4, 5))
  assert(fibsViaUnfold.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
}
