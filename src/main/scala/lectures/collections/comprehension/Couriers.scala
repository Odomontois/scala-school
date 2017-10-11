package lectures.collections.comprehension

/**
  * Помогите курьерам разобраться с обслуживанием адресов
  *
  * Каждый день на работу выходит 'courierCount' курьеров
  * Им нужно обслужить 'addressesCount' адресов
  * Каждый курьер может обслужить courier.canServe адресов, но только при условии, что позволит дорожная ситуация.
  * Т.е. если trafficDegree < 5, то курьер обслужит все адреса, которые может, иначе - ни одного
  *
  * Входные данные для приложения содержат 2 строки
  * В первой строке - количество адресов, которые требуется обслужить
  * Во второй - количество курьеров, вышедших на работу.
  *
  * Ваша задача:
  * Изучить код и переписать его так,
  * что бы в нем не было ни одного цикла for, ни одной переменной или мутабильной коллекции
  *
  * Для этого используйте функции комбинаторы: filter, withFilter, fold, map, flatMap и т.д.
  *
  */

case class Traffic(degree: Double)

object Courier {
  def couriers(courierCount: Int): List[Courier] =
    ((1 to courierCount) map { i => Courier(i) }).toList
}

case class Courier(index: Int) {
  val canServe: Int = (Math.random() * 10).toInt
}

object Address {
  def addresses(addressesCount: Int): List[Address] =
    ((1 to addressesCount) map { i => Address(s"$i$i$i") }).toList
}

case class Address(postIndex: String)

object CouriersWithComprehension extends App {

  import Address._
  import Courier._

  val sc = new java.util.Scanner(System.in)
  val addressesCount = sc.nextInt()
  val courierCount = sc.nextInt()
  val addrs = addresses(addressesCount)
  val cours = couriers(courierCount)

  // какие адреса были обслужены
  def serveAddresses(addresses: List[Address], couriers: List[Courier]): List[Address] = {
    var accum = 0

    // My implementation
    couriers
      .map(courier => (courier, traffic().degree))
      .flatMap { case (courier, trafficDegree) =>
        (0 until courier.canServe)
          .withFilter { _ => trafficDegree < 5 && accum < addresses.length }
          .map { _ => // t =>
            val addr = addresses(accum)
            accum = accum + 1
            addr
          }
      }

    //Given implementation
    //        for (courier <- couriers;
    //             trafficDegree = traffic().degree;
    //             t <- 0 until courier.canServe if trafficDegree < 5 && accum < addresses.length
    //        ) yield {
    //          val addr = addresses(accum)
    //          accum = accum + 1
    //          addr
    //        }
  }

  def traffic(): Traffic = Traffic(Math.random() * 10)

  def printServedAddresses(addresses: List[Address], couriers: List[Courier]) {
    serveAddresses(addresses, couriers)
      .foreach(a => println(a.postIndex))
  }

  printServedAddresses(addrs, cours)

}
