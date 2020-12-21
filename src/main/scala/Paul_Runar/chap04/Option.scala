package Paul_Runar.chap04

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(get) => Some(f(get))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B =  this match {
    case None => default
    case Some(get) => get
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap((x: A) => if (f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
