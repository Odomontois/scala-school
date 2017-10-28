package lectures.collections

import lectures.collections.CherryTree.{Node, Node1, Node2}

import scala.annotation.tailrec
import scala.collection.generic._
import scala.collection.{GenTraversableOnce, LinearSeq, LinearSeqOptimized, mutable}

sealed trait CherryTree[+T] extends LinearSeq[T]
  with LinearSeqOptimized[T, CherryTree[T]]
  with GenericTraversableTemplate[T, CherryTree]
  with Product with Serializable {

  override def apply(n: Int): T =
    if (n < 0 || n >= size) throw new IndexOutOfBoundsException(s"requested elem with index $n of tree with size $size")
    else if (n <= this.size / 2) drop(n).head
    else take(n + 1).last

  override def drop(n: Int): CherryTree[T] = {

    @tailrec
    def dropRecur(n: Int, tree: CherryTree[T]): CherryTree[T] = {
      if (n >= size) CherryNil
      else if (n <= 0) tree
      else (tree: @unchecked) match {
        // we check size before, so we don't need to consider CherryNil and CherrySingle
        case CherryBranch(_, inner, right) =>
          if (n == 1) tree.tail
          // optimization for quicker calculation of near to tail n values
          else if (n == tree.size - 1) right match {
            case Node1(x) => CherrySingle(x)
            case Node2(_, y) => CherrySingle(y)
          } else if (n == tree.size - 2) right match {
            case Node1(a) => inner.last match {
              case Node2(_, b) => CherryBranch(Node1(b), CherryNil, Node1(a))
            }
            case Node2(x, y) => CherryBranch(Node1(x), CherryNil, Node1(y))
          } else dropRecur(n - 1, tree.tail)
      }
    }

    dropRecur(n, this)
  }

  override def take(n: Int): CherryTree[T] = {

    @tailrec
    def takeRecur(n: Int, tree: CherryTree[T]): CherryTree[T] = {
      if (n >= size) tree
      else if (n <= 0) CherryNil
      else (tree: @unchecked) match {
        // we check size before, so we don't need to consider CherryNil and CherrySingle
        case CherryBranch(left, inner, _) =>
          // optimization for quicker calculation of near to head n values
          if (n == 1) left match {
            case Node1(x) => CherrySingle(x)
            case Node2(x, _) => CherrySingle(x)
          } else if (n == 2) left match {
            case Node1(x) => inner.head match {
              case Node2(y, _) => CherryBranch(Node1(x), CherryNil, Node1(y))
            }
            case Node2(x, y) => CherryBranch(Node1(x), CherryNil, Node1(y))
          }
          else if (n == tree.size - 1) tree.init
          else takeRecur(n, tree.init)
      }
    }

    takeRecur(n, this)
  }

  override def init: CherryTree[T] = this match {
    case CherryNil => throw new UnsupportedOperationException("init of empty CherryTree")
    case CherrySingle(_) => CherryNil
    case CherryBranch(left, inner, right) => right match {
      case Node1(_) => inner match {
        case CherryNil => left match {
          case Node1(x) => CherrySingle(x)
          case Node2(x, y) => CherryBranch(Node1(x), CherryNil, Node1(y))
        }
        case CherrySingle(node) => CherryBranch(left, CherryNil, node)
        case tree@CherryBranch(_, _, _) => CherryBranch(left, tree.init, tree.last)
      }
      case Node2(x, _) => CherryBranch(left, inner, Node1(x))
    }
  }

  override def last: T = this match {
    case CherryNil => throw new NoSuchElementException("last of empty CherryTree")
    case CherrySingle(x) => x
    case CherryBranch(_, _, right) => right match {
      case Node1(x) => x
      case Node2(_, y) => y
    }
  }

  def append[S >: T](x: S): CherryTree[S]
  def prepend[S >: T](x: S): CherryTree[S]

  def concat[S >: T](xs: CherryTree[S]): CherryTree[S] = {

    def concatAppend(biggerTree: CherryTree[S], smallerTree: CherryTree[S]): CherryTree[S] = {
      smallerTree match {
        case CherryNil => biggerTree
        case CherrySingle(x) => biggerTree.append(x)
        case CherryBranch(leftSmaller, innerSmaller, rightSmaller) => (biggerTree: @unchecked) match {
          // we know that biggerTree have at least the same size as small, so it can't be neither CherryNil nor CherrySingle
          case CherryBranch(leftBigger, innerBigger, rightBigger) => rightBigger match {
            case Node1(x) => leftSmaller match {
              case Node1(y) => CherryBranch(
                leftBigger,
                innerBigger.append(Node2(x, y)) ++ innerSmaller,
                rightSmaller
              )
              case Node2(y, z) =>
                val (constructedTree, last) = constructTreeAppend(z, innerSmaller, innerBigger :+ Node2(x, y))
                buildCherryBranchAppend(rightSmaller, leftBigger, constructedTree, last)
            }
            case nodeBiggerRight: Node2[S] => leftSmaller match {
              case Node1(x) =>
                val (constructedTree, last) = constructTreeAppend(x, innerSmaller, innerBigger :+ nodeBiggerRight)
                buildCherryBranchAppend(rightSmaller, leftBigger, constructedTree, last)
              case nodeSmallerLeft: Node2[S] => CherryBranch(
                leftBigger,
                innerBigger.append(nodeBiggerRight).append(nodeSmallerLeft) ++ innerSmaller,
                rightSmaller
              )
            }
          }
        }
      }
    }

    def concatPrepend(biggerTree: CherryTree[S], smallerTree: CherryTree[S]): CherryTree[S] = {
      smallerTree match {
        case CherryNil => biggerTree
        case CherrySingle(x) => biggerTree.prepend(x)
        case CherryBranch(leftSmaller, innerSmaller, rightSmaller) => (biggerTree: @unchecked) match {
          // we know that biggerTree have at least the same size as small, so it can't be neither CherryNil nor CherrySingle
          case CherryBranch(leftBigger, innerBigger, rightBigger) => leftBigger match {
            case Node1(x) => rightSmaller match {
              case Node1(y) => CherryBranch(
                leftSmaller,
                innerSmaller ++ innerBigger.prepend(Node2(y, x)),
                rightBigger
              )
              case Node2(y, z) =>
                val (constructedTree, last) = constructTreePrepend(y, innerSmaller, Node2(z, x) +: innerBigger)
                buildCherryBranchPrepend(rightBigger, leftSmaller, constructedTree, last)
            }
            case nodeBiggerLeft: Node2[S] => rightSmaller match {
              case Node1(x) =>
                val (constructedTree, last) = constructTreePrepend(x, innerSmaller, nodeBiggerLeft +: innerBigger)
                buildCherryBranchPrepend(rightBigger, leftSmaller, constructedTree, last)
              case nodeSmallerRight: Node2[S] => CherryBranch(
                leftSmaller,
                innerSmaller ++ innerBigger.prepend(nodeBiggerLeft).prepend(nodeSmallerRight),
                rightBigger
              )
            }
          }
        }
      }
    }

    @tailrec
    def constructTreeAppend(single: S, from: CherryTree[Node2[S]], to: CherryTree[Node2[S]]): (CherryTree[Node2[S]], S) = {
      from match {
        case CherryNil => (to, single)
        case _ => from.head match {
          case Node2(x, y) => constructTreeAppend(y, from.tail, to :+ Node2(single, x))
        }
      }
    }

    @tailrec
    def constructTreePrepend(single: S, from: CherryTree[Node2[S]], to: CherryTree[Node2[S]]): (CherryTree[Node2[S]], S) = {
      from match {
        case CherryNil => (to, single)
        case _ => from.last match {
          case Node2(x, y) => constructTreePrepend(x, from.init, Node2(y, single) +: to)
        }
      }
    }

    def buildCherryBranchAppend(rightS: Node[S], leftB: Node[S], tree: CherryTree[Node2[S]], last: S): CherryBranch[S] = {
      rightS match {
        case Node1(y) => CherryBranch(
          leftB,
          tree,
          Node2(last, y)
        )
        case Node2(y, z) => CherryBranch(
          leftB,
          tree :+ Node2(last, y),
          Node1(z)
        )
      }
    }

    def buildCherryBranchPrepend(rightB: Node[S], leftS: Node[S], tree: CherryTree[Node2[S]], last: S): CherryBranch[S] = {
      leftS match {
        case Node1(y) => CherryBranch(
          Node2(y, last),
          tree,
          rightB
        )
        case Node2(y, z) => CherryBranch(
          Node1(y),
          Node2(z, last) +: tree,
          rightB
        )
      }
    }

    if (this.size >= xs.size) concatAppend(this, xs)
    else concatPrepend(xs, this)
  }

  override def toString(): String = super.toString()
  override def companion: CherryTree.type = CherryTree
  override def stringPrefix: String = "CherryTree"


  // If we have a default builder, there are faster ways to perform some operations
  @inline private[this] def isDefaultCBF[A, B, That](bf: CanBuildFrom[CherryTree[A], B, That]): Boolean =
    bf eq CherryTree.ReusableCBF

  override def :+[B >: T, That](elem: B)(implicit bf: CanBuildFrom[CherryTree[T], B, That]): That =
    if (isDefaultCBF(bf)) append(elem).asInstanceOf[That] else super.:+(elem)

  override def +:[B >: T, That](elem: B)(implicit bf: CanBuildFrom[CherryTree[T], B, That]): That =
    if (isDefaultCBF(bf)) prepend(elem).asInstanceOf[That] else super.+:(elem)

  override def ++[B >: T, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[CherryTree[T], B, That]): That =
    if (isDefaultCBF(bf)) concat(that.asInstanceOf[CherryTree[B]]).asInstanceOf[That] else super.++(that)
}

case object CherryNil extends CherryTree[Nothing] {
  override def head = throw new NoSuchElementException("head of empty CherryTree")
  override def tail = throw new UnsupportedOperationException("tail of empty CherryTree")
  override def foreach[U](f: (Nothing) => U): Unit = ()
  override def append[S >: Nothing](x: S): CherryTree[S] = CherrySingle(x)
  override def prepend[S >: Nothing](x: S) = CherrySingle(x)
  override def size = 0
  override def isEmpty = true
}

final case class CherrySingle[+T](x: T) extends CherryTree[T] {
  override def head: T = x
  override def tail: CherryNil.type = CherryNil
  override def foreach[U](f: T => U): Unit = f(x)
  override def append[S >: T](y: S) = CherryBranch(Node1(x), CherryNil, Node1(y))
  override def prepend[S >: T](y: S) = CherryBranch(Node1(y), CherryNil, Node1(x))
  override def size: Int = x match {
    case node: Node2[_] => node.size
    case _ => 1
  }
  override def isEmpty = false
}

final case class CherryBranch[+T](left: Node[T], inner: CherryTree[Node2[T]], right: Node[T]) extends CherryTree[T] {
  override def head: T = left match {
    case Node1(x)    => x
    case Node2(x, _) => x
  }
  override def tail: CherryTree[T] = left match {
    case Node1(_)    => inner match {
      case CherryNil => right match {
        case Node1(x)    => CherrySingle(x)
        case Node2(x, y) => CherryBranch(Node1(x), CherryNil, Node1(y))
      }
      case tree      => CherryBranch(tree.head, tree.tail, right)
    }
    case Node2(_, x) => CherryBranch(Node1(x), inner, right)
  }
  override def foreach[U](f: T => U): Unit = {
    left.foreach(f)
    inner.foreach(_.foreach(f))
    right.foreach(f)
  }
  override def append[S >: T](x: S): CherryBranch[S] = right match {
    case Node1(y)    => CherryBranch(left, inner, Node2(y, x))
    case n: Node2[S] => CherryBranch(left, inner.append(n), Node1(x))
  }
  override def prepend[S >: T](x: S): CherryBranch[S] = left match {
    case Node1(y) => CherryBranch(Node2(x, y), inner, right)
    case node: Node2[S] => CherryBranch(Node1(x), inner.prepend(node), right)
  }
  override def size: Int = left.size + inner.size + right.size
  override def isEmpty = false
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

  sealed trait Node[+T] {
    def foreach[U](f: T => U): Unit
    def size: Int
  }

  final case class Node1[+T](x: T) extends Node[T] {
    override def foreach[U](f: (T) => U): Unit = f(x)
    def size: Int = x match {
      case node: Node2[_] => node.size
      case _ => 1
    }
  }

  final case class Node2[+T](x: T, y: T) extends Node[T] {
    def foreach[U](f: (T) => U): Unit = {
      f(x)
      f(y)
    }
    def size: Int = x match {
      case node: Node2[_] =>  2 * node.size
      case _ => 2
    }
  }

}

