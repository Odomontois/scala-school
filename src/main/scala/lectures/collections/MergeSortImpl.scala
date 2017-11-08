package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def merge(data1: Seq[Int],data2: Seq[Int]): Seq[Int] ={
    (data1,data2) match {
      case (_, Nil) =>  data1
      case (Nil, _) =>  data2
      case (h1::tail1,h2::tail2)=>{
        if (h1 < h2) {
          h1 +: merge(tail1,data2)
        } else {
          h2 +: merge(data1,tail2)
        }
      }
    }
  }

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    val n = data.length/2
    if(n==0)
      data
    else {
     val (left,right) = data splitAt n
      merge(mergeSort(left),mergeSort(right))
    }
  }
}