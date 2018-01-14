package core.bigrammar

class RootGrammar(val value: BiGrammar) extends GrammarPath
{
  override def ancestorGrammars: Set[BiGrammar] = Set.empty

  override def ancestors: Seq[GrammarPath] = Seq.empty

  override def hashCode(): Int = value.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case other: RootGrammar => value.equals(other.value)
    case _ => false
  }
}
