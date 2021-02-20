package Paul_Runar.chap08

object Problem01 extends App {
  def sum(il: List[Int]): Int = {
    il.sum
  }

  val myList = List(1, 2, 3)
  sum(myList) == sum(myList.reverse)
}
