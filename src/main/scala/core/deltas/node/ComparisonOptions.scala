package core.deltas.node

import scala.collection.mutable

case class ComparisonOptions(compareIntegers: Boolean = true, takeAllLeftKeys: Boolean = true, takeAllRightKeys: Boolean = true)
{
  def deepEquality(first: Any, second: Any): Boolean = {

    def deepEquality(first: Any, second: Any, closed: mutable.Set[(Node, Node)]): Boolean = {
      if (first == second)
        return true

      (first, second) match {
        case (seq1: Set[_], seq2: Set[_]) =>
          if (seq1.size != seq2.size)
            return false
          true //TODO missing checks.
        case (seq1: Seq[_], seq2: Seq[_]) =>
          if (seq1.length != seq2.length)
            return false
          seq1.zip(seq2).forall(p => deepEquality(p._1, p._2, closed))
        case (meta1: Node, meta2: Node) => deepEqualityMeta(meta1, meta2, closed)
        case (int1: Integer, int2: Integer) => if (compareIntegers) first == second else true
        case _ => first == second
      }
    }

    def deepEqualityMeta(first: Node, second: Node, closed: mutable.Set[(Node, Node)]): Boolean = {
      val key = (first, second)
      if (!closed.add(key))
        return true

      if (!first.shape.equals(second.shape))
        return false

      val sharedKeys = (takeAllLeftKeys, takeAllRightKeys) match {
        case (true, true) => first.data.keySet ++ second.data.keySet
        case (false, false) => first.data.keySet.intersect(second.data.keySet)
        case (true, false) => first.data.keySet
        case (false, true) => second.data.keySet
      }
      sharedKeys.forall(key => (first.data.get(key), second.data.get(key)) match {
        case (Some(firstVal), Some(secondVal)) => deepEquality(firstVal, secondVal, closed)
        case _ => false
      })
    }

    deepEquality(first, second, mutable.Set[(Node, Node)]())
  }
}