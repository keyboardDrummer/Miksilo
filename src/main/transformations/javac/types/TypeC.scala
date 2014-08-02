package transformations.javac.types

import core.grammar.seqr
import core.transformation._
import transformations.javac.base.model.{JavaTypes, QualifiedClassName}

import scala.collection.mutable

object TypeC extends GrammarTransformation {
  override def transform(program: MetaObject, state: TransformationState): Unit = {}

  override def dependencies: Set[Contract] = Set.empty

  def isAssignableTo(state: TransformationState)(to: MetaObject, from: MetaObject): Boolean = {
    val fromSuperTypes = getSuperTypes(state)(from)
    if (to.equals(from))
      true

    fromSuperTypes.exists(_type => _type.equals(to))
  }

  def getSuperTypes(state: TransformationState)(_type: MetaObject) = getSuperTypesRegistry(state)(_type.clazz)(_type)

  def getSuperTypesRegistry(state: TransformationState) = {
    getState(state).superTypes
  }

  private def getState(state: TransformationState) = state.data.getOrElseUpdate(this, new State()).asInstanceOf[State]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val parseType = grammars.create(TypeGrammar)

    val parseObjectType = identifier.someSeparated(".") ^^ { case ids: Seq[Any] =>
      val stringIds = ids.collect({ case v: String => v})
      if (ids.size > 1)
        JavaTypes.objectType(new QualifiedClassName(stringIds))
      else
        JavaTypes.objectType(stringIds(0))
    }
    val parseIntType = "int" ^^ (_ => JavaTypes.intType)
    val parseArrayType = parseType ~ "[]" ^^ { case _type seqr _ => JavaTypes.arrayType(_type.asInstanceOf[MetaObject])}
    parseType.inner = parseArrayType | parseObjectType | parseIntType
  }

  class State {
    val superTypes = new mutable.HashMap[Any, MetaObject => Seq[MetaObject]]()
  }

  object TypeGrammar

}
