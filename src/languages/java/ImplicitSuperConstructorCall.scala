package languages.java

import transformation.{TransformationState, MetaObject, ProgramTransformation}

object ImplicitSuperConstructorCall extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(ConstructorC)

  override def transform(program: MetaObject, state: TransformationState): Unit = ???
}
