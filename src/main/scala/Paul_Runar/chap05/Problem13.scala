package Paul_Runar.chap05

object Problem13 extends App {
  val a = Stream(1, 2, 3, 4, 5, 6, 1, 2, 3)
  println(a.mapViaUnfold(_ + 1).toList)// == List(2, 3, 4))
  println(a.takeViaUnfold(2).toList)// == List(1, 2))
  println(a.takeWhileViaUnfold(_ < 4).toList)
}
