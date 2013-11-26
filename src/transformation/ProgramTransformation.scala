package transformation


trait ProgramTransformation {
  def transform(program: MetaObject, state: TransformationState)
  def dependencies: Set[ProgramTransformation]
}
