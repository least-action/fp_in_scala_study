package Paul_Runar.chap04

object Problem07 {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](Right(Nil))((a, z) => a.map2(z)(_ :: _))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[A]] = ???

  def main(args: Array[String]): Unit = {
    val a = List(Right(1), Right(2), Right(3))
    println(sequence(a))
  }
}
