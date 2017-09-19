package lectures.functions

import Y2_combinator.fixByName

object Decompose extends App{
  def countOnes(num: BigInt, count: Long): Long = fixByName(
    recur => (_num: BigInt, _count: Long) =>
      if (_num == 0) _count
      else recur(_num / 2, if (_num % 2 == 0) _count else _count + 1))(num, count)

  println(countOnes(27, 0))
}
