package transformation

import languages._

object TransformationManager {
  val transformations = Seq[ProgramTransformation](AddWhile,AddStatementToSSM, AddIfElse, AddBlock, AddDoWhile, AddIfElse, AddForLoop)
  def buildCompiler(transformations: Seq[ProgramTransformation]) : Compiler = {
    new Compiler {
      def compile(program: MetaObject) {
        val state = new TransformationState
        for(transformation <- transformations)
          transformation.transform(program,state)
      }
    }
  }
}
