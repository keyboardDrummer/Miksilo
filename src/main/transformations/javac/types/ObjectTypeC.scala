package transformations.javac.types

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}

object ObjectTypeC extends ProgramTransformation {
  override def transform(program: MetaObject, state: TransformationState): Unit = {
    TypeC.getSuperTypesRegistry(state).put(this, _objectType => {
      Seq.empty //TODO find parent types.
    })
  }

  override def dependencies: Set[Contract] = Set(TypeC)
}
