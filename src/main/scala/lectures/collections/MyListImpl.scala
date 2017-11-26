package lectures.collections

/**
  * Представим, что по какой-то причине Вам понадобилась своя обертка над списком целых чисел List[Int]
  *
  * Вы приняли решение, что будет достаточно реализовать 4 метода:
  * * * * * def flatMap(f: (Int => MyList)) -  реализуете на основе соответствующего метода из List
  * * * * * метод map(f: (Int) => Int) - с помощью только что полученного метода flatMap класса MyList
  * * * * * filter(???) - через метод flatMap класса MyList
  * * * * * foldLeft(acc: Int)(???) - через декомпозицию на head и tail
  *
  * Для того, чтобы выполнить задание:
  * * * * * раскомментируйте код
  * * * * * замените знаки вопроса на сигнатуры и тела методов
  * * * * * не используйте var и мутабильные коллекции
  *
  */
object MyListImpl extends App {



  case class MyList[T](data: List[T]) {

    def flatMap(f: (T => MyList[T])) =
      MyList(data.flatMap(inp => f(inp).data))

    def map(f: (T => T)) = this.flatMap(i => MyList(List(f(i))))

    def foldLeft(acc: T)(f: ((T,T) => T)):T =
    {
      def fl(acc2:T, tmp:List[T] ):T = tmp match{
        case Nil => acc2
        case head::tail => fl(f(acc2,head),tail)
      }
      fl(acc, this.data)
    }

    def filter(f: (T => Boolean)) = this.flatMap(i => if (f(i))  MyList(List(i))  else MyList(Nil) )
  }




  require(MyList[Double](List(1.1, 2.1, 3.1, 4.1, 5.1, 6.1)).map(_ * 2).data == List(2.2, 4.2, 6.2, 8.2, 10.2, 12.2))
  require(MyList[Int](List(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList[Int](List(1, 2, 3, 4, 5, 6)).foldLeft(0)((m,n) => m + n) == 21)
  require(MyList[Int](Nil).foldLeft(0)((m,n) => m + n) == 0)

}