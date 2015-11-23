package core.bigrammar

import util.{GraphBasics, ExtendedType, Property}


trait GrammarPath {
  def get: BiGrammar
  def children: Seq[GrammarPath] = {
    val clazz: Class[_ <: BiGrammar] = get.getClass
    new ExtendedType(clazz).properties.
      filter(property => classOf[BiGrammar].isAssignableFrom(property._type)).
      map(property => {
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
  def ancestors: Seq[GrammarPath]
  def descentsIncludingSelf: Seq[GrammarPath] = GraphBasics.traverseBreadth[GrammarPath](Seq(this), path => path.children)
}

class RootGrammar(value: BiGrammar) extends GrammarPath
{
  override def get: BiGrammar = value

  override def ancestors: Seq[GrammarPath] = Seq.empty
}

class GrammarSelection(val previous: GrammarPath, property: Property[BiGrammar, AnyRef]) extends GrammarPath
{
  val parent = previous.get

  def set(value: BiGrammar): Unit = {
    property.set(parent, value)
  }

  override def get: BiGrammar = {
    property.get(parent).asInstanceOf[BiGrammar]
  }
  
  def removeMeFromOption(): Unit = {
    val choiceParent = parent.asInstanceOf[Choice]
    val me = get
    val sibling = Set(choiceParent.left,choiceParent.right).filter(grammar => grammar != me).head 
    previous.asInstanceOf[GrammarSelection].set(sibling)
  }

  def removeMeFromSequence(): Unit = {
    val choiceParent = parent.asInstanceOf[SequenceLike]
    val me = get
    val sibling = Set(choiceParent.first,choiceParent.second).filter(grammar => grammar != me).head
    previous.asInstanceOf[GrammarSelection].set(sibling)
  }

  override def toString = s"GrammarSelection($get)"

  override def ancestors: Seq[GrammarPath] = Seq(previous) ++ previous.ancestors
}
