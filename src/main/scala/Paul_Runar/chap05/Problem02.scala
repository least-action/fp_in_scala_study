package Paul_Runar.chap05

object Problem02 {
  def main(args: Array[String]): Unit = {
    val a = Stream(1, 2, 3, 4, 5, 6)
    println(a.take(2).toList)
    println(a.drop(2).toList)
  }
}
