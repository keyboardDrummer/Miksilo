package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeGrammar, NodeShape}
import deltas.javac.methods.call.CallDelta
import deltas.statement.StatementDelta

object EmitStatementDelta extends DeltaWithGrammar {

  object Shape extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val call = find(CallDelta.Shape)
    val callGrammar = call.inner.asInstanceOf[NodeGrammar].inner
    val grammar = "emit" ~~ callGrammar.asNode(Shape) ~ ";"
    find(StatementDelta.Grammar).addAlternative(grammar)
  }

  override def description = "Adds the emit statement"

  override def dependencies = Set(StatementDelta, CallDelta)
}
