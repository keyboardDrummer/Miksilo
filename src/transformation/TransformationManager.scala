package transformation

import languages._
import languages.ssm._
import languages.bytecode._
import languages.java.base.JavaBase
import languages.java._

object TransformationManager {
  val transformations = Seq[ProgramTransformation](AddWhile,AddStatementToSSM, AddIfElse, AddBlock,
    AddDoWhile, AddIfElse, AddForLoop, JavaMinus,
    ByteCode, ByteCodeGoTo, JavaBase, TernaryC, SubtractionC, LiteralC, StringLiteralC, AdditionC)

  def buildCompiler(transformations: Seq[ProgramTransformation]) : Compiler = {
    new Compiler {
      def compile(program: MetaObject) = {
        val state = new TransformationState
        for(transformation <- transformations)
          transformation.transform(program,state)
        program
      }
    }
  }
}
