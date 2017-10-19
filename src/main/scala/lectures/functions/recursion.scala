package lectures.functions

object recursion extends App {
  def collatzSteps(x: BigInt, steps: Long = 0): Long =
    if (x == 1) steps
    else collatzSteps(if (x % 2 == 0) x / 2 else 3 * x + 1, steps + 1)

  def collatzSeq(x: BigInt, seq: Seq[BigInt] = Vector.empty): Seq[BigInt] =
    if (x == 1) seq
    else collatzSeq(if (x % 2 == 0) x / 2 else 3 * x + 1, seq :+ x)


  def fixByVal(step: ((BigInt, Long) => Long) => ((BigInt, Long) => Long)): (BigInt, Long) => Long =
    step(fixByVal(step))

  def fixByName(step: (=> (BigInt, Long) => Long) => ((BigInt, Long) => Long)): (BigInt, Long) => Long =
    step(fixByName(step))


  def countOnesFixed = fixByName(recursive => (num, count) =>
    if (num == 0) count
    else recursive(num / 2, if (num % 2 == 0) count else count + 1)
  )

println(countOnesFixed(41424, 12))
}
