package Paul_Runar.chap05

object Problem03 {
  def main(args: Array[String]): Unit = {
    val a = Stream(1, 2, 3, 4)
    println(a.takeWhile(_ < 3).toList)
  }
}
