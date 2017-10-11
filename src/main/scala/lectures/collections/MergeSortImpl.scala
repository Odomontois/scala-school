package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def merge(data1: Seq[Int], data2: Seq[Int]): Seq[Int] = {
    def loop(acc: Seq[Int])(seq1: Seq[Int], seq2: Seq[Int]): Seq[Int] =
      if (seq1.isEmpty && seq2.isEmpty) acc
      else if (seq1.isEmpty && seq2.nonEmpty) acc ++ seq2
      else if (seq1.nonEmpty && seq2.isEmpty) acc ++ seq1
      else {
        if(seq1.head < seq2.head) loop(acc :+ seq1.head)(seq1.tail, seq2)
        else loop(acc :+ seq2.head)(seq1, seq2.tail)
      }

    loop(Seq[Int]())(data1, data2)
  }

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    val n = data.size
    if (n < 2) data
    else {
      val (first, second) = data.splitAt(n / 2)
      val firstSorted = mergeSort(first)
      val secondSorted = mergeSort(second)
      merge(firstSorted, secondSorted)
    }
  }

}
