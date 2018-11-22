package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class BiSequence(var first: BiGrammar, var second: BiGrammar,
                 val bijective: SequenceBijective,
                 val horizontal: Boolean) extends BiGrammar with Layout {

  override def withChildren(newChildren: Seq[BiGrammar]) = new BiSequence(newChildren(0), newChildren(1), bijective, horizontal)

  override def children = Seq(first, second)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean =
    recursive(first) || recursive(second)

  override protected def getLeftChildren(recursive: BiGrammar => Seq[BiGrammar]): Seq[BiGrammar] = {
    if (first.containsParser())
      recursive(first)
    else {
      recursive(second)
    }
  }
}
