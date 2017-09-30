package lectures.functions

object Y2_combinator extends App {
  def fixByVal(step: ((BigInt, Long) => Long) => ((BigInt, Long) => Long)): (BigInt, Long) => Long =
    step(fixByVal(step))

  def fixByName(step: (=> (BigInt, Long) => Long) => ((BigInt, Long) => Long)): (BigInt, Long) => Long =
    step(fixByName(step))

  val res1 = fixByName(recur => (x, steps) =>
    if (x == 1) steps
    else recur(if (x % 2 == 0) x / 2 else 3 * x + 1, steps + 1))(27, 0)
  println(res1)

  //  val res2 = fixByVal(recur => (x, steps) =>
  //    if (x == 1) steps
  //    else recur(if (x % 2 == 0) x / 2 else 3 * x + 1, steps + 1))(27, 0)
  //  print(res2)

//  def loop: Int = loop
//
//  def f_byName(x: Int, y: => Int): Int =
//    if (x == 1) 1
//    else f_byName(x - 1, y)
//
//  println(f_byName(5, loop))

//  def f_byValue(x: Int, y: Int): Int =
//    if (x == 1) 1
//    else f_byValue(x - 1, y)
//
//  println(f_byValue(5, loop))
}