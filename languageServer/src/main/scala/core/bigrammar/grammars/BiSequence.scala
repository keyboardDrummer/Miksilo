package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class BiSequence(var first: BiGrammar, var second: BiGrammar,
                 val bijective: SequenceBijective,
                 val horizontal: Boolean) extends BiGrammar with Sequence {

  override def withChildren(newChildren: Seq[BiGrammar]) = new BiSequence(newChildren(0), newChildren(1), bijective, horizontal)

}
