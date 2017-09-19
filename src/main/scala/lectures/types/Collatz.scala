package lectures.types

object Collatz extends App{
  def collatzSteps(x: BigInt, steps: Long = 0): Long =
    if (x == 1) steps
    else collatzSteps(if (x % 2 == 0) x / 2 else 3 * x + 1, steps + 1)

  def collatzSeq(x: BigInt, seq: Seq[BigInt] = Vector.empty): Seq[BigInt] =
    if (x == 1) seq
    else collatzSeq(if (x % 2 == 0) x / 2 else 3 * x + 1, seq :+ x)

  println(collatzSteps(27))

  println(collatzSeq(27))
}
