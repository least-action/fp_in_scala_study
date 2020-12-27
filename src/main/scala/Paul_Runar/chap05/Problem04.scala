package Paul_Runar.chap05

object Problem04 {
  def main(args: Array[String]): Unit = {
    val a = Stream(1, 2, 3, 4, 5)
    assert(a.forAll(_ < 6))
  }
}
