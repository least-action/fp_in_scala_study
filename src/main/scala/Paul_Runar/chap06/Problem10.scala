package Paul_Runar.chap06

object Problem10 extends App {
  val st = State((i: Int) => (i*10, i+1))
  val (i1, s1) = st.run(1)
  val (i2, s2) = st.run(s1)
  println((i1, s1))
  println((i2, s2))
}
