package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class BiSequence[Left, Right, Result](var first: BiGrammar[Left], var second: BiGrammar[Right],
                 val bijective: SequenceBijective[Left, Right, Result],
                 val horizontal: Boolean) extends BiGrammar[Result] with Layout {

  override def withChildren(newChildren: Seq[BiGrammar[_]]) = new BiSequence(
    newChildren(0).asInstanceOf[BiGrammar[Left]],
    newChildren(1).asInstanceOf[BiGrammar[Right]], bijective, horizontal)

  override def children = Seq(first, second)

  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean =
    recursive(first) || recursive(second)

  override protected def getLeftChildren(recursive: BiGrammar[_] => Seq[BiGrammar[_]]): Seq[BiGrammar[_]] = {
    if (first.containsParser())
      recursive(first)
    else {
      recursive(second)
    }
  }
}

object BiSequence {

  def tuple[A,B]: SequenceBijective[A,B,(A,B)] = SequenceBijective[A,B,(A,B)](packTuple, unpackTuple[A,B])

  private def packTuple[A,B]: (A, B) => (A, B) = (a: A, b: B) => (a,b)
  private def unpackTuple[A,B]: ((A,B)) => Option[(A, B)] = t => Some(t)

  def ignoreLeft[Right] =
    SequenceBijective[Unit, Right, Right]((_, b: Right) => b, x => Some(Unit, x))

  def ignoreRight[Left] =
    SequenceBijective[Left, Unit, Left]((a: Left, _) => a, x => Some(x, Unit))
}

case class SequenceBijective[Left, Right, Result](construct: (Left, Right) => Result, destruct: Result => Option[(Left, Right)])