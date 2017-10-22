package lectures.oop

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

}

case class BSTImpl(value: Int,
                   var left: Option[BST] = None,
                   var right: Option[BST] = None) extends BST {

  def add(newValue: Int): BST = {
    if (BSTImpl.root.isEmpty) { BSTImpl.root = Option(this)}
    if (newValue < this.value)
      this.unapply match {
        case(_,None,_) => left = Option(BSTImpl(newValue));
        case _ => left.get.add(newValue)
      }
    if (newValue > this.value)
      this.unapply match {
        case(_,_,None) => right = Option(BSTImpl(newValue));
        case _ => right.get.add(newValue)
      }
    BSTImpl.root.get
  }


  def unapply = (value,left,right)

  def find(value: Int): Option[BST] = {
    val stack = Stack(BSTImpl.root)
    //stack.push(BSTImpl.root)
    while (stack.nonEmpty){
      val temp = stack.top
      //println(temp)
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
/*
  override def toString: String = {
    var result = new StringBuilder("")
    val stack = Stack(BSTImpl.root)
    //stack.push(BSTImpl.root)
    while (stack.nonEmpty){
      val temp = stack.top
      result.append(temp.get.value + " ")
      stack.pop
      if (temp.get.left.isDefined) stack.push(temp.get.left)
      if (temp.get.right.isDefined) stack.push(temp.get.right)
    }
    result.toString()
  }
*/

  // override def toString() = ???

}

object BSTImpl {
  var root: Option[BST] = None
  var current: Option[BST] = None
  def setCurToRoot(): Unit = {current = root}
}

object TreeTest /*extends App*/ {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
//  val tree: BST = ??? // generator goes here
  val tree: BST = BSTImpl(maxValue / 2)
  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  //require(testTree.find(markerItem).isDefined)
  //require(testTree.find(markerItem).isDefined)
  //require(testTree.find(markerItem).isDefined)

  println(testTree)
}

object Tmp extends App{
  val tree: BST = BSTImpl(5)
  val testTree = tree.add(7).add(3).add(4)
  println(tree)
  //println(testTree.find(4))
}