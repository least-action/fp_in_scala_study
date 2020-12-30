package Paul_Runar.chap05

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = {
    @scala.annotation.tailrec
    def loop(a: Stream[A], acc: List[A]): List[A] = a match {
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)
    }
    loop(this, Nil).reverse
  }

  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @scala.annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  def reverse: Stream[A] = {
    @scala.annotation.tailrec
    def loop(base: Stream[A], acc: Stream[A]): Stream[A] = base match {
      case Cons(h, t) => loop(t(), Cons(h, () => acc))
      case _ => acc
    }
    loop(this, Empty)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 1) => Cons(h, () => t().take(n-1))
    case Cons(h, _) if (n == 1) => Cons(h, () => Empty)
    case _ => Empty
  }

  def take_(n: Int): Stream[A] = {
    @scala.annotation.tailrec
    def loop(n: Int, base: Stream[A], acc: Stream[A]): Stream[A] = {
      if (n < 1) acc
      else base match {
        case Cons(h, t) => loop(n-1, t(), Cons(h, () => acc))
        case _ => acc
      }
    }
    loop(n, this, Empty).reverse
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def drop_(n: Int): Stream[A] = {
    @scala.annotation.tailrec
    def loop(n: Int, cur: Stream[A]): Stream[A] =
      if (n < 1) cur
      else cur match {
        case Cons(_, t) => loop(n-1, t())
        case _ => cur
      }
    loop(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile_(p: A => Boolean): Stream[A] = {
    @scala.annotation.tailrec
    def loop(base: Stream[A], acc: Stream[A]): Stream[A] = base match {
      case Cons(h, t) => if (p(h())) loop(t(), Cons(h, () => acc)) else acc
      case _ => acc
    }
    loop(this, Empty).reverse
  }

  def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, z) => p(a) && z)

  def forAll_(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])((a, z) => if (p(a)) Cons(() => a, () => z) else z)

  def headOptionViaFoldRight: Option[A] =
    this.foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, z) => Cons(() => f(a), () => z))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, z) => if (f(a)) Cons(() => a, () => z) else z)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, z) => Cons(() => a, () => z))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, z) => f(a).append(z))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this)({case Cons(h, t) => Some(f(h()), t()) case Empty => None})

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n))({
      case (Cons(h, t), nn) if (nn > 0) => Some(h(), (t(), nn-1))
      case _ => None
    })

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    Stream.unfold(this)({
      case Cons(h, t) if (f(h())) => Some(h(), t())
      case _ => None
    })

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this, s2))({
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    })

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_, _))

  def zip_[B](s2: Stream[B]): Stream[(A,B)] = {
    Stream.unfold((this, s2))({
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
      case _ => None
    })
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    Stream.unfold((this, s2))({
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Empty, Empty) => None
    })
  }

  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll({ case (v1, v2) => v1 == v2})

  def startsWith_[B >: A](s: Stream[B]): Boolean =
    Stream.unfold((this, s))({
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((h1() == h2(), (t1(), t2())))
    }).forAll(x => x)

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(Stream.empty)

  def tailss: Stream[Stream[A]] =
    Stream.unfold(this)({
      case Cons(h, t) => Some((Cons(h, t), t()))
      case _ => None
    }).append(Stream(Empty))

  def hasSequence[B>:A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => Stream.cons(h, unfold(s)(f))
    case None => Stream.empty
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(x => Some((x, x+1)))

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}