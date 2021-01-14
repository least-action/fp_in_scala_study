package Paul_Runar.chap06

case class State[S,+A](run: S => (A,S)) {
  def flatMap[B](f: A => State[S,B]): State[S,B] = State((s: S) => {
      val (a, s1) = this.run(s)
      f(a).run(s1)
    })

  def map[B](f: A => B): State[S,B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap(a => sb.map(f(a,_)))
}

object State {
  def unit[S,A](a: A): State[S,A] =
    State(s => (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.reverse.foldLeft(unit[S,List[A]](List()))((z, sa) => sa.map2(z)(_ :: _))

  def get[S]: State[S,S] = State(s => (s, s))
  def set[S](s: S): State[S,Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
