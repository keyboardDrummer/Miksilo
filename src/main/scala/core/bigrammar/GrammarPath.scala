package core.bigrammar

import util.{ExtendedType, Property}


trait GrammarPath {
  def get: BiGrammar
  def children: Seq[GrammarPath] = {
    val clazz: Class[_ <: BiGrammar] = get.getClass
    new ExtendedType(clazz).properties.map(property => {
        new GrammarSelection(this, property.asInstanceOf[Property[BiGrammar, AnyRef]])
      })
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case otherPath: GrammarPath => otherPath.get.equals(get)
      case _ => false
    }
  }

  override def hashCode(): Int = get.hashCode()
}

class Root(value: BiGrammar) extends GrammarPath
{
  override def get: BiGrammar = value
}

class GrammarSelection(previous: GrammarPath, property: Property[BiGrammar, AnyRef]) extends GrammarPath
{
  val parent = previous.get

  def set(value: BiGrammar): Unit = {
    property.set(parent, value)
  }

  override def get: BiGrammar = {
    property.get(parent).asInstanceOf[BiGrammar]
  }

  override def toString = s"GrammarSelection($get)"
}
