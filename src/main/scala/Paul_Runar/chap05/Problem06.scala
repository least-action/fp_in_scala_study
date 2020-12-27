package Paul_Runar.chap05

object Problem06 {
  def main(args: Array[String]): Unit = {
    val a = Stream(1, 2, 3, 4, 5)
    val b = Empty
    val c = Stream(None, 1, 2, 3)
    assert(a.headOptionViaFoldRight.contains(1))
    assert(b.headOptionViaFoldRight.isEmpty)
    println(c.headOptionViaFoldRight)
  }
}
