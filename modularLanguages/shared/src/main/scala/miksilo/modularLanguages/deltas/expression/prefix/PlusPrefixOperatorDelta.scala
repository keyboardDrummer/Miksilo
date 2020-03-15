package miksilo.modularLanguages.deltas.expression.prefix

object PlusPrefixOperatorDelta extends PrefixOperatorDelta {
  override def keyword = "+"
}

object PrefixIncrementDelta extends PrefixOperatorDelta {
  override def keyword = "++"
}

object PrefixDecrementDelta extends PrefixOperatorDelta {
  override def keyword = "--"
}
