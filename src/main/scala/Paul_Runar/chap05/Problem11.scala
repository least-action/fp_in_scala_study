package Paul_Runar.chap05

object Problem11 extends App {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((h,s)) => Stream.cons(h, unfold(s)(f))
      case None => Stream.empty
    }

  assert(unfold(1)(x => if (x < 5) Some((x, x+1)) else None).toList == List(1, 2, 3, 4))
}
