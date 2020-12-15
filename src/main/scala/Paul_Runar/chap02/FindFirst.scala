package Paul_Runar.chap02

object FindFirst {
  def findFirstString(ss: Array[String], key: String): Int = {
    @scala.annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @scala.annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    val ss = Array[String]("a", "b", "c", "hello")
    println(findFirstString(ss, "hello"))
    val as = Array[Int](1, 2, 4, 5, 6)
    println(findFirst(as, (_: Int) == 3))
  }
}
