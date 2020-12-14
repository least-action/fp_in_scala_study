package Paul_Runar.chap02

object Problem01 {
  def fibonacci(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, base: Int, next: Int): Int = {
      if    (n <= 1) base
      else  go(n-1, next, next + base)
    }

    go(n, 0, 1)
  }
}
