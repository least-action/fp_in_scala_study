package Paul_Runar.chap05

object Problem05 {
  def main(args: Array[String]): Unit = {
    val a = Stream(1, 2, 3, 4)
    println(a.takeWhileViaFoldRight(_ < 2).toList)
  }
}
