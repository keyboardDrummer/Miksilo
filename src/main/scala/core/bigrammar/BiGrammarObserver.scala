package core.bigrammar

trait BiGrammarObserver[Result] {

  def labelledEnter(name: AnyRef): Result

  def labelledLeave(inner: Result, partial: Result)

  def handleGrammar(self: BiGrammar, recursive: BiGrammar => Result): Result

  def observe(grammar: BiGrammar) = {
    var cache = Map.empty[Labelled, Result]
    def helper(grammar: BiGrammar): Result = grammar match {
      case labelled: Labelled =>
        cache.getOrElse(labelled, {
          val result = labelledEnter(labelled.name)
          cache += labelled -> result
          labelledLeave(helper(labelled.inner), result)
          result
        })
      case _ => handleGrammar(grammar, helper)
    }
    helper(grammar)
  }
}
