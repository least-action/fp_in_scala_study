package Paul_Runar.chap05

object Problem13 extends App {
  val a = Stream.fromViaUnfold(1)
  assert(a.mapViaUnfold(_ + 1).take(3).toList == List(2, 3, 4))
  assert(a.takeViaUnfold(2).toList == List(1, 2))
  assert(a.takeWhileViaUnfold(_ < 4).toList == List(1, 2, 3))

  val b = Stream.fromViaUnfold(3)
  val c = Stream.fromViaUnfold(5)
  assert(b.zipWith(c)(_ + _).take(3).toList == List(8, 10, 12))
  assert(b.zip(c).take(3).toList == List((3, 5), (4, 6), (5, 7)))

  val d = Stream(1, 2, 3)
  val e = Stream(5, 6)
  assert(d.zipAll(e).toList == List((Some(1), Some(5)), (Some(2), Some(6)), (Some(3), None)))

}
