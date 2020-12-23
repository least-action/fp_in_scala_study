package Paul_Runar.chap04

object Problem04 {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil): Option[List[A]])((aa, z) => (map2(aa, z)(_ :: _)))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence2(t) map (hh :: _))
  }

  def sequence3[A](a: List[Option[A]]): Option[List[A]] = {
    @scala.annotation.tailrec
    def loop(a: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = a match {
      case Nil => acc
      case x :: xs => x match {
        case None => None
        case Some(v) => loop(xs, Some(v :: acc.getOrElse(Nil)))
      }
    }
    loop(a, Some(Nil)) map (_.reverse)
  }

  def opList(start: Int, end: Int): List[Option[Int]] = {
    def loop(start: Int, end: Int, acc: List[Option[Int]]): List[Option[Int]] = {
      if (start > end) acc
      else loop(start + 1, end, Some(start) :: acc)
    }
    loop(start, end, Nil)
  }


  def main(args: Array[String]): Unit = {
    val a = List(Some(1), Some(2), Some(4), Some(3))
    val b = List(Some(1), Some(2), None, Some(3))
    val c = None :: opList(0, 10000000)

    val t = System.nanoTime
    println((System.nanoTime - t) / 1e9d)
    sequence (c)
    println((System.nanoTime - t) / 1e9d)

    sequence2(c)
    println((System.nanoTime - t) / 1e9d)

    sequence3(c)
    println((System.nanoTime - t) / 1e9d)
  }
}
