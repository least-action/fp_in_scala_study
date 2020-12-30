package Paul_Runar.chap05

object Problem15 extends App {
  assert(Stream(1, 2, 3).tails.toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), List()))
}
