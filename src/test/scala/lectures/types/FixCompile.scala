object DecomposeCountOnes extends App{
  def fixByName(step: (=> (BigInt, Long) => Long) => ((BigInt, Long) => Long)): (BigInt, Long) => Long =
    step(fixByName(step))
  /*
    def countOnes(num: BigInt, count: Long): Long =
      if (num == 0) count
      else countOnes(num / 2, if (num % 2 == 0) count else count + 1)
  */
  println(fixByName( recur => (num, count) =>
    if (num == 1) count
    else  recur(num/2, if (num % 2 == 0) count else count + 1))(27, 0))
}
