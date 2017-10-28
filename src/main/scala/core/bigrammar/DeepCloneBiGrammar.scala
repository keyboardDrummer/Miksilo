package core.bigrammar

import core.particles.node.GrammarKey

class DeepCloneBiGrammar extends BiGrammarObserver[BiGrammar] { //TODO deze class gaat wel eens stuk, misschien omdat de default case soms teveel afvangt. Fixen?

  override def labelledEnter(name: GrammarKey): BiGrammar = new Labelled(name)

  override def labelledLeave(inner: BiGrammar, partial: BiGrammar): Unit = partial.asInstanceOf[Labelled].inner = inner

  override def handleGrammar(self: BiGrammar, recursive: (BiGrammar) => BiGrammar): BiGrammar = self match {
    case choice:Choice => new Choice(recursive(choice.left), recursive(choice.right))
    case many: ManyVertical => new ManyVertical(recursive(many.inner))
    case many: ManyHorizontal => new ManyHorizontal(recursive(many.inner))
    case mapGrammar: MapGrammar => new MapGrammar(recursive(mapGrammar.inner), mapGrammar.construct, mapGrammar.deconstruct, mapGrammar.showMap)
    case sequence:Sequence => new Sequence(recursive(sequence.first), recursive(sequence.second))
    case topBottom: TopBottom => new TopBottom(recursive(topBottom.first), recursive(topBottom.second))
    case as:As => As(recursive(as.inner), as.key)
    case _ => self
  }
}
