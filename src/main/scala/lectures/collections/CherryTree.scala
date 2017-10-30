package lectures.collections

import java.util.NoSuchElementException

import lectures.collections
import lectures.collections.CherryTree.{Node, Node1, Node2}

import scala.reflect.runtime.universe._
import scala.collection.generic._
import scala.collection.{GenTraversableOnce, LinearSeq, LinearSeqOptimized, mutable}

sealed trait CherryTree[+T] extends LinearSeq[T]
  with LinearSeqOptimized[T, CherryTree[T]]
  with GenericTraversableTemplate[T, CherryTree]
  with Product with Serializable{
  override def init: CherryTree[T] = {
    if (this.isEmpty)throw new UnsupportedOperationException("empty CherryTree")
    else this
  }
  override def last: T = this.apply(size-1)
  def append[S >: T](x: S): CherryTree[S]
  def prepend[S >: T](x: S): CherryTree[S]

  def concat[S >: T](xs: CherryTree[S]): CherryTree[S] = {
    if (this.isEmpty) xs
    else
      if (xs.isEmpty) this
      else
        if (this.size > xs.size){
          var result = this.append(xs.head)
          xs.tail.foreach(el => result = result.append(el))
          result
        }
        else {
          var result = xs.prepend(this.last)
          this.slice(0,this.size-1).reverse.foreach(el => result = result.prepend(el))
          result
        }
  }
  override def toString(): String = super.toString()
  override def companion = CherryTree
  override def stringPrefix: String = "CherryTree"

  override def apply(n: Int): T = {
    def find(tree: CherryTree[_], index: Int, start: Int, stop: Int, mult: Int): T = {
      tree match {
        case CherryBranch(left, inner, right) => index match {
          case id: Int if id < (start + left.size * mult) => findInNode(id - start, 0, left.size * mult - 1, left)
          case id: Int if id > (stop - right.size * mult) => findInNode(id - (stop + 1 - right.size * mult), 0, right.size * mult - 1, right)
          case id => find(inner, id, start + left.size * mult, stop - right.size * mult, mult * 2)
        }
        case cs1@CherrySingle(x:Node[_]) => findInNode(index - start, 0, stop - start , x)
        case cs2@CherrySingle(x) => x.asInstanceOf[T]
        case CherryNil => throw new NoSuchElementException("empty CherryList")
      }
    }
    find(this, n, 0, size-1, 1)
  }

  private def findInNode(index:Int, left: Int, right: Int, str:Node[_]):T ={
    if ((index < left)|| (index > right)) throw new IndexOutOfBoundsException("index is out of bounds of the structure")
    str match {
      case n1: Node1[_] => n1.x match {
        case x:Node[_] => findInNode(index,left,right,x)
        case x => x.asInstanceOf[T]
      }
      case n2: Node2[_] => n2.x match {
        case x:Node[_] => if (index <= (left+right)/2) findInNode(index,left,(left+right)/2,x)
        else findInNode(index, (left+right)/2 +1,right, n2.y.asInstanceOf[Node[_]])
        case x => if (left == index) x.asInstanceOf[T] else n2.y.asInstanceOf[T]
      }
    }
  }


  // If we have a default builder, there are faster ways to perform some operations
  @inline private[this] def isDefaultCBF[A, B, That](bf: CanBuildFrom[CherryTree[A], B, That]): Boolean = bf eq CherryTree.ReusableCBF

  override def :+[B >: T, That](elem: B)(implicit bf: CanBuildFrom[CherryTree[T], B, That]) =
    if (isDefaultCBF(bf)) append(elem).asInstanceOf[That] else super.:+(elem)

  override def +:[B >: T, That](elem: B)(implicit bf: CanBuildFrom[CherryTree[T], B, That]) =
    if (isDefaultCBF(bf)) append(elem).asInstanceOf[That] else super.:+(elem)

  override def ++[B >: T, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[CherryTree[T], B, That]) =
    if (isDefaultCBF(bf)) concat(that.asInstanceOf[CherryTree[B]]).asInstanceOf[That] else super.++(that)
}

case object CherryNil extends CherryTree[Nothing] {
  override def head = throw new NoSuchElementException("head of empty CherryList")
  override def tail = throw new UnsupportedOperationException("tail of empty CherryList")
  override def foreach[U](f: (Nothing) => U) = ()
  override def append[S >: Nothing](x: S): CherryTree[S] = CherrySingle(x)
  override def prepend[S >: Nothing](x: S): CherryTree[S] = CherrySingle(x)
  override def size = 0
  override def isEmpty = true
}
final case class CherrySingle[+T](x: T) extends CherryTree[T] {
  override def head = x
  override def tail = CherryNil
  override def foreach[U](f: T => U) = f(x)
  def append[S >: T](y: S) = CherryBranch(Node1(x), CherryNil, Node1(y))
  def prepend[S >: T](y: S) = CherryBranch(Node1(y), CherryNil, Node1(x))
  override def size = 1
  override def isEmpty = false
}
final case class CherryBranch[+T](left: Node[T], inner: CherryTree[Node2[T]], right: Node[T]) extends CherryTree[T] {
  override def head = left match {
    case Node1(x)    => x
    case Node2(x, _) => x
  }
  override def tail = left match {
    case Node1(_)    => inner match {
      case CherryNil => right match {
        case Node1(x)    => CherrySingle(x)
        case Node2(x, y) => CherryBranch(Node1(x), CherryNil, Node1(y))
      }
      case tree      => CherryBranch(tree.head, tree.tail, right)
    }
    case Node2(_, x) => CherryBranch(Node1(x), inner, right)
  }
  override def foreach[U](f: T => U) = {
    left.foreach(f)
    inner.foreach(_.foreach(f))
    right.foreach(f)
  }
  def append[S >: T](x: S) = right match {
    case Node1(y)    => CherryBranch(left, inner, Node2(y, x))
    case n: Node2[S] => CherryBranch(left, inner.append(n), Node1(x))
  }

  def prepend[S >: T](x: S) = left match {
    case Node1(y)    => CherryBranch(Node2(x, y), inner, right)
    case n: Node2[S] => CherryBranch(Node1(x), inner.prepend(n), right)
  }

  override def size = left.size + inner.size * 2 + right.size
  override def isEmpty = false
  def unapply = (left,inner,right)
}


object CherryTree extends SeqFactory[CherryTree] {
  private class CherryTreeBuilder[T]() extends mutable.Builder[T, CherryTree[T]] {
    private[this] var coll: CherryTree[T] = CherryNil
    def +=(elem: T) = {coll = coll.append(elem); this }
    def clear(): Unit = coll = CherryNil
    def result(): CherryTree[T] = coll
  }

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, CherryTree[A]] =
    ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  def newBuilder[T]: mutable.Builder[T, CherryTree[T]] = new CherryTreeBuilder[T]

  def getIndexOfElement[T](tree: CherryTree[T], el:T):Int ={
    for (i <- 0 until tree.size){
      if (tree.apply(i) == el) return i
    }
    return -1
  }

  sealed trait Node[+T] {
    def foreach[U](f: T => U): Unit
    def size: Int
  }
  final case class Node1[+T](x: T) extends Node[T] {
    override def foreach[U](f: (T) => U): Unit = f(x)
    def size = 1
  }
  final case class Node2[+T](x: T, y: T) extends Node[T] {
    def foreach[U](f: (T) => U): Unit = {
      f(x)
      f(y)
    }
    def size = 2
  }
}

object CherryApp extends App{
  var tmp = CherryTree(1)
  for (i <- 2 to 112){
    tmp = tmp.append(i)
  }

//  tmp.lnApply(1)

  for (i <- 0 to 115) {
    println("result ",CherryTree.getIndexOfElement(tmp,i))
  }


  println(CherryTree(1).apply(0))
  def find1(str:Node[_]):Unit ={
    str match {
      case n1: Node1[_] => n1.x match {
        case x: Int => println(x)
        case x:Node[_] => find1(x)
      }
      case n2: Node2[_] => n2.x match {
        case x:Int => n2.foreach(println)
        case x:Node[_] => find1(x); find1(n2.y.asInstanceOf[Node[_]])
      }
    }
  }

  def find(index:Int, left: Int, right: Int, str:Node[_]):Int ={
    str match {
      case n1: Node1[_] => n1.x match {
        case x: Int => x
        case x:Node[_] => find(index,left,right,x)
      }
      case n2: Node2[_] => n2.x match {
        case x:Int => if (left == index) x else n2.y.asInstanceOf[Int]
        case x:Node[_] => if (index <= (left+right)/2) find(index,left,(left+right)/2,x)
                          else find(index, (left+right)/2 +1,right, n2.y.asInstanceOf[Node[_]])
      }
    }
  }


/*
  find1(Node1(Node1(4)))
  println("------------")
  //find( Node2(Node2(3,4),Node1(5)))
  find1( Node2( Node2(Node2(4,5),Node2(6,7)),Node1(Node2(9,7))) )
*/

/*
  val n1 = Node1(Node2(Node2(Node2(Node2(16,17),Node2(18,19)),Node2(Node2(20,21),Node2(22,23))),Node2(Node2(Node2(24,25),Node2(26,27)),Node2(Node2(28,29),Node2(30,31)))))


  println(tmp)
  var k:Int=1

  while (tmp.isInstanceOf[CherryBranch[Int]]) {
    println(tmp.asInstanceOf[CherryBranch[Int]].left/*.size*k*/)
    println(tmp.asInstanceOf[CherryBranch[Int]].right/*.size*k*/)
   // tmp = tmp.asInstanceOf[CherryBranch[Int]].inner.asInstanceOf[CherryBranch[Int]]

    tmp.asInstanceOf[CherryBranch[Int]].inner.asInstanceOf[CherryTree[Int]] match{
      case br: CherryBranch[Int] => tmp = br// tmp.asInstanceOf[CherryBranch[Int]].inner.asInstanceOf[CherryBranch[Int]];
      case CherryNil => tmp = CherryNil
      case cs: CherrySingle[Int] => tmp = cs
    }
    println(tmp)
    println(tmp.size*2*k)
    println("----")
    k = k *2
  }
  println(tmp.size)
*/

}