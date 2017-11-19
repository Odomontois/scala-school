package lectures.oop

import lectures.oop.TreeTest.root

import scala.collection.mutable.Stack


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  var left: Option[BST]
  var right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]

  def fold(aggregator: Int)(f: (Int, Int) =>(Int)): Int

  def unapply = (value,left,right)

}

case class BSTImpl(value: Int,
                   var left: Option[BST] = None,
                   var right: Option[BST] = None) extends BST {

  def add(newValue: Int): BST = {
    if (BSTImpl.root.isEmpty) { BSTImpl.root = Option(this)}
    if (newValue < this.value)
      this.unapply match {
        case(_,None,_) => BSTImpl.size +=1; left = Option(BSTImpl(newValue));
        case _ => left.get.add(newValue)
      }
    else
      this.unapply match {
        case(_,_,None) => BSTImpl.size +=1; right = Option(BSTImpl(newValue));
        case _ => right.get.add(newValue)
      }
    BSTImpl.root.get
  }


  def find(value: Int): Option[BST] = {
    val stack = Stack(BSTImpl.root)
    //stack.push(BSTImpl.root)
    while (stack.nonEmpty){
      val temp = stack.top
      //println(temp.get.value)
      stack.pop
      if (value == temp.get.value) return temp
      if (value < temp.get.value) {
        if (temp.get.left.isDefined) stack.push(temp.get.left)
      }
      else
        if (temp.get.right.isDefined) stack.push(temp.get.right)
    }
    None
  }

  def fold(aggregator: Int)(f: (Int, Int) =>(Int)): Int ={
    def inner(value: Int, cur:BST):Int =
    cur.unapply match {
      case(_,None,None) => cur.value
      case(_,None,_) => f(cur.value,inner(value,cur.right.get))
      case(_,_,None) => f(cur.value,inner(value,cur.left.get))
      case _ => f(f(cur.value,inner(value,cur.left.get)), inner(value,cur.right.get))
    }
    inner(aggregator,BSTImpl.root.get)
  }



  override def toString: String = {
    var result = new StringBuilder("")
    val stack = Stack(BSTImpl.root)
    /*
    var pow = 2
    var border = 2 * pow
    var counter = 0
    */
    var noIntsonRow = false
    while ((stack.nonEmpty) /*&& (!noIntsonRow) && (pow < 1000)*/){
      val temp = stack.top
      result.append(temp.get.value + " ")
      //if (temp.get.value != -1) counter += 1
      stack.pop
      if (temp.get.left.isDefined) { stack.push(temp.get.left)}  //else {stack.push(Some(BSTImpl(-1, None,None))) }
      if (temp.get.right.isDefined) { stack.push(temp.get.right)} // else {stack.push(Some(BSTImpl(-1, None,None))) }
      /*println(counter,pow,border)
      pow += 2

      if (pow == border) {
        println(counter,pow,border)
        border *= 2
        if (counter >= border-1) { noIntsonRow = true }
        counter = border/2
      }*/
    }
    result.toString()
  }


}

object BSTImpl {
  var root: Option[BST] = None
  var size = 1
}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue / 50).toInt
  val markerItem2 = (Math.random() * maxValue / 50).toInt
  val markerItem3 = (Math.random() * maxValue / 50).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 100)

  val tree: BST =
  (for (i <- 1 to nodesCount)
    yield (Math.random() * maxValue).toInt).map(el => root.add(el)).last




  //add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct

  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem2).isDefined)
  require(testTree.find(markerItem3).isDefined)

  println(testTree)
  println(BSTImpl.size)
  println(testTree.fold(0)(_+_))

}
