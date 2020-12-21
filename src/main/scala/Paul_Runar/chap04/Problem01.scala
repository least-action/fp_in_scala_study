package Paul_Runar.chap04

object Problem01 {
  def main(args: Array[String]): Unit = {
    val a = Some(1)
    val a2 = None
    val b = Some("hello")
    assert((a map (_ + 1)) == Some(2))
    assert((a flatMap ((x: Int) => Some(x + 1))) == Some(2))
    assert((a getOrElse 2) == 1)
    assert((a2 getOrElse 2) == 2)
    assert((a orElse b) == Some(1))
    assert((a2 orElse b) == Some("hello"))
    assert((a filter (_ < 2)) == Some(1))
    assert((a filter (_ > 2)) == None)
  }
}
