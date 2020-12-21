package Paul_Runar.chap04

object Problem03 {
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    (age * numberOfSpeedingTickets) / 2.0

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None}

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
  def lift2[A, B](f: A => Option[B]): Option[A] => Option[B] = {
    _ flatMap f
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    val f1 = (a: A) => lift((b: B) => f(a, b))
    val f2 = (b: Option[B]) => lift2((a: A) => f1(a)(b))
    val f3 = (a: Option[A], b: Option[B]) => f2(b)(a)
    f3(a, b)

//    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try { age.toInt }
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def main(args: Array[String]): Unit = {

  }
}
