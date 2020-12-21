package Paul_Runar.chap04

object Problem02 {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def main(args: Array[String]): Unit = {
    val a = Seq[Double](1.0, 2.0, 3.0)
    println(variance(a))
  }
}
