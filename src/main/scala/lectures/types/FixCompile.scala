package lectures.types

/**
  * Программа должна удвоить все нечетные члены List и потом просуммировать,
  * сохранив результат в 'result'
  *
  * Ваша задача:
  * * Раскоментируйте код
  * * Исправьте тело метода mapper так, чтобы программа скомпилировалась
  */
object FixCompile extends App {

  val mapper = (i: Int) => if (i % 2 != 0) i * 2 else i

  val result = List(1, 2, 3, 4, 5, 6, 7, 8, 9).map {
      mapper
    }.foldLeft(0) { (acc, v) => acc + v }

   print(result)
}

