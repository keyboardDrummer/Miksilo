package transformations.types

import core.transformation.sillyCodePieces.GrammarTransformation
import core.transformation.{Contract, MetaObject, TransformationState}

trait TypeInstance extends GrammarTransformation {

  val key: AnyRef

  override def inject(state: TransformationState): Unit = {
    TypeC.getSuperTypesRegistry(state).put(key, _type => getSuperTypes(_type, state))
    TypeC.getState(state).toByteCodeString.put(key, _type => getByteCodeString(_type, state))
    TypeC.getState(state).stackSize.put(key, getStackSize)
    super.inject(state)
  }

  def getSuperTypes(_type: MetaObject, state: TransformationState): Seq[MetaObject]

  def getStackType(_type: MetaObject) = _type

  def getByteCodeString(_type: MetaObject, state: TransformationState): String

  def getStackSize: Int

  override def dependencies: Set[Contract] = Set(TypeC)
}
