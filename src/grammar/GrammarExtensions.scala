package grammar

object FailureG extends Grammar {
  override def mustConsume: Boolean = true
}

object Success extends Grammar {
  override def mustConsume: Boolean = false
}