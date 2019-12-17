package core.language.node

trait Key extends AnyRef
{
  override lazy val toString: String = debugRepresentation

  /**
    * This hashcode does not change over runs, while the default hashcode does.
    * This makes the compilation process more deterministic.
    */
  override def hashCode(): Int = this.getClass.toString.hashCode

  def debugRepresentation: String = this match {
    case anyRef: AnyRef =>
      try
      {
        val shape = anyRef.getClass
        getClassName(shape)
      }
      catch
        {
          case e: java.lang.InternalError => e.toString
        }
    case _ => this.toString
  }

  private def getClassName(shape: Class[_]): String = {
    "blurp"
//    val enclosing: Class[_] = shape.getEnclosingClass
//    val addition = if (enclosing == null) "" else getClassName(enclosing) + "."
//    addition + getDirectClassName(shape)
  }

  protected def getDirectClassName(shape: Class[_]): String = {
    val simpleName: String = shape.getSimpleName
    if (simpleName.last == '$')
      simpleName.dropRight(1)
    else
      simpleName
  }
}

/**
  * Defines a field for a Node
  */
trait NodeField extends GrammarKey {
  override def debugRepresentation: String = this match {
    case anyRef: AnyRef =>
      try
      {
        val shape = anyRef.getClass
        getDirectClassName(shape)
      }
      catch
        {
          case e: java.lang.InternalError => e.toString
        }
    case _ => this.toString
  }
}

trait GrammarKey extends Key