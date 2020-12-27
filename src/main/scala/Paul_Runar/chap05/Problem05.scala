package Paul_Runar.chap05

object Problem05 {
  def main(args: Array[String]): Unit = {
    val a = Stream(1, 2, 3, 4)
    assert(a.takeWhileViaFoldRight(_ < 3).toList == List(1, 2))
  }
}
