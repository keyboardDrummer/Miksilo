package grammar


object Number extends Grammar {
  override def mustConsume: Boolean = true

}

object Identifier extends Grammar {
  override def mustConsume: Boolean = true
}