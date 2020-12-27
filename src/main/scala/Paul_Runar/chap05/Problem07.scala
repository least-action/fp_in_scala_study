package Paul_Runar.chap05

object Problem07 {
  def main(args: Array[String]): Unit = {
    val a = Stream(1, 2, 3, 4, 5)
    assert(a.map(_ + 1).toList == List(2, 3, 4, 5, 6))
    assert(a.filter(x => x%2 == 1).toList == List(1, 3, 5))
    assert(a.append(Stream(6, 7, 8)).toList == List(1, 2, 3, 4, 5, 6, 7, 8))
    assert(a.flatMap(x => Stream(x*10, x*10 + 1)).toList == List(10, 11, 20, 21, 30, 31, 40, 41, 50, 51))
  }
}
