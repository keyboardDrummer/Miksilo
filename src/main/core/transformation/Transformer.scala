package core.transformation

class Transformer(val injectors: Seq[Injector]) {
  def transform(program: MetaObject) = {
    val state = new TransformationState
    for (transformation <- injectors.reverse)
      transformation.inject(state)

    injectors.collect({ case transformation: ProgramTransformation => transformation.transform(program, state)})
    program
  }
}
