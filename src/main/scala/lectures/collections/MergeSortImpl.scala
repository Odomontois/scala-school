package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = if (data.size == 1) data
    else merge(mergeSort( data.slice(0,data.size/2)), mergeSort(data.slice(data.size/2,data.size)))

  def merge(a:Seq[Int],b:Seq[Int]):Seq[Int] = (a,b) match
  {
    case(a, Nil) => a
    case(Nil, b) => b
    case(aHead :: aTail, bHead :: bTail) =>
      if (aHead < bHead) aHead +: merge(aTail, b)
      else bHead +: merge(a, bTail)
  }

  println(mergeSort(List(6,7,1,3,2,4,3)))
}
