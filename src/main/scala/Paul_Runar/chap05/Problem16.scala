package Paul_Runar.chap05

object Problem16 extends App {
  assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
}
