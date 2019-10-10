package core.bigrammar

import core.bigrammar.grammars.Labelled
import core.language.node.GrammarKey

trait BiGrammarObserver[Result] {

  def getReference(name: GrammarKey): Result

  def setReference(result: Result, reference: Result)

  def handleGrammar(self: BiGrammar[_], children: Seq[Result], recursive: BiGrammar[_] => Result): Result

  def observe(grammar: BiGrammar[_]) = {
    var cache = Map.empty[Labelled[_], Result]
    def withoutCache(grammar: BiGrammar[_]) = handleGrammar(grammar, grammar.children.map(child => withCache(child)), withCache)
    def withCache(grammar: BiGrammar[_]): Result = grammar match {
      case labelled: Labelled[_] => cache.getOrElse(labelled, {
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
