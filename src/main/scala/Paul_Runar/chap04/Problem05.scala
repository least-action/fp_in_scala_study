package Paul_Runar.chap04

object Problem05 {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None}

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((aa, z) => map2(f(aa), z)(_ :: _))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(aa => aa)

  def main(args: Array[String]): Unit = {
    val a = List("1", "1", "1")
    val b = List("1", "1", "1l")
    assert(traverse(a)(i => Try(i.toInt)) == Some(List(1, 1, 1)))
    assert(traverse(b)(i => Try(i.toInt)) == None)
  }
}
