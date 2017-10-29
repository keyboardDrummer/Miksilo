package core.bigrammar

import core.particles.node.GrammarKey

trait BiGrammarObserver[Result] {

  def getReference(name: GrammarKey): Result

  def setReference(result: Result, reference: Result)

  def handleGrammar(self: BiGrammar, children: Seq[Result], recursive: (BiGrammar) => Result): Result

  def observe(grammar: BiGrammar) = {
    var cache = Map.empty[Labelled, Result]
    def withoutCache(grammar: BiGrammar) = handleGrammar(grammar, grammar.children.map(child => withCache(child)), withCache)
    def withCache(grammar: BiGrammar): Result = grammar match {
      case labelled: Labelled => cache.getOrElse(labelled, {
          val reference = getReference(labelled.name)
          cache += labelled -> reference
          val result = withoutCache(labelled)
          setReference(result, reference)
          reference
        })
      case _ => withoutCache(grammar)
    }
    withCache(grammar)
  }
}
