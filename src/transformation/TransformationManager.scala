package transformation

import languages._
import languages.ssm._
import languages.bytecode._
import languages.javac.base.JavaBase
import languages.javac._

object TransformationManager {
  val transformations = Seq[ProgramTransformation](AddWhile,AddStatementToSSM, AddIfElse, AddBlock,
    AddDoWhile, AddIfElse, AddForLoop, JavaMinus, DefaultConstructor, LessThanC,
    ByteCode, ByteCodeGoTo, JavaBase, TernaryC, SubtractionC, LiteralC, StringLiteralC, AdditionC,
    ConstructorC, ImplicitJavaLangImport, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod)


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
