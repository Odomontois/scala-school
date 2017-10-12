package lectures.oop

import scala.collection.immutable.Queue
import scala.util.Random


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
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]
}


case class BSTImpl(value: Int,
                   left: Option[BST] = None,
                   right: Option[BST] = None) extends BST {

  def add(newValue: Int): BST = {
    def addToChild(child: Option[BST]): Option[BST] = {
      val newChild = child match {
        case Some(bst) => bst.add(newValue) //.asInstanceOf[BSTImpl]
        case None => BSTImpl(newValue)
      }
      Option(newChild)
    }

    if (newValue == value) this
    else if (newValue < value) {
      val newLeft = addToChild(left)
      BSTImpl(value, newLeft, right)
    } else {
      val newRight = addToChild(right)
      BSTImpl(value, left, newRight)
    }
  }

  def find(value: Int): Option[BST] = {
    if (value == this.value) Option(this)
    else if (value < this.value) {
      if (left.nonEmpty) left.get.find(value)
      else None
    } else {
      if (right.nonEmpty) right.get.find(value)
      else None
    }
  }

  lazy val depth: Int = {
    def childDepth(child: Option[BST]): Int = child match {
      case Some(bst) => math.max(childDepth(bst.left), childDepth(bst.right)) + 1
      case None => 0
    }

    math.max(childDepth(left), childDepth(right)) + 1
  }


  // TODO: Please refactor it, Someone! T___T
  override def toString() = {
    def toString(acc: String)(queue: Queue[(Option[BST], Int, Boolean)]): String = {
      if (queue.isEmpty) acc
      else {
        val ((headMaybe, headLevel, isLeft), queueTail) = queue.dequeue
        val newQueue = if (headLevel >= 1) {
          val childrenLevel = headLevel - 1
          val (maybeLeft, maybeRight) = headMaybe match {
            case Some(bts) => (bts.left, bts.right)
            case None => (None, None)
          }
          queueTail
            .enqueue((maybeLeft, childrenLevel, true))
            .enqueue((maybeRight, childrenLevel, false))
        } else Queue.empty[(Option[BST], Int, Boolean)]
        val newString_? =
          if (newQueue.nonEmpty && headLevel != newQueue.head._2) "\n"
          else ""
        val newAcc = acc + atLevel(headLevel, headMaybe, isLeft) + newString_?
        toString(newAcc)(newQueue)
      }
    }

    /**
      * @param level root has level equal to depth() - 1, and deepest leaf - equal to 1
      */
    def atLevel(level: Int, child: Option[BST], isLeft: Boolean): String = {
      val margin = Array.fill[Char](1 << level)(' ').mkString("")
      val halfMargin = Array.fill[Char](1 << level - 1)(' ').mkString("")
      child match {
        case Some(bts) if isLeft =>  s"$margin ${bts.value}"
        case Some(bts) if !isLeft =>  s"$halfMargin ${bts.value}"
        case None => s"$margin $margin"
      }
    }

    toString("")(Queue((Option(this), depth - 1, true)))
  }
}

object BSTImplGenerator {
  val rnd = new Random()

  def apply(nodesCount: Int, bound: (Int, Int) = (0, Int.MaxValue)): Option[BSTImpl] = {
    if (nodesCount == 0) None
    else {
      val leftSize = rnd.nextInt(nodesCount / 2 + 1) + nodesCount / 4
      val rightSize = nodesCount - leftSize - 1
      val value = math.max(rnd.nextInt(bound._2 - bound._1), leftSize + 1) + bound._1
      val left = BSTImplGenerator(leftSize, (bound._1, value))
      val right = BSTImplGenerator(rightSize, (value + 1, bound._2))
      Option(BSTImpl(value, left, right))
    }
  }
}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = BSTImplGenerator(nodesCount, (0, maxValue)).getOrElse(BSTImpl(-1)) // generator goes here

  println(s"original depth = ${tree.asInstanceOf[BSTImpl].depth}")
  println(s"original tree =\n$tree")
  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)

  println(s"testTree =\n$testTree")
}