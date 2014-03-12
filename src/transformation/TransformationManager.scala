package transformation

import languages._
import languages.ssm._
import languages.bytecode._
import languages.javac.base.JavaBase
import languages.javac._

object TransformationManager {
  val ssmTransformations = Set(AddWhile,AddStatementToSSM, AddIfElse, AddBlock,
    AddDoWhile, AddIfElse, AddForLoop)

  val javaTransformations = Set[ProgramTransformation](JavaMinus, DefaultConstructor, LessThanC,
    ByteCode, ByteCodeGoTo, JavaBase, TernaryC, SubtractionC, LiteralC, StringLiteralC, AdditionC,
    NoMaxStack, NoStackFrame, ImplicitThisInPrivateCalls, ConstructorC, ImplicitJavaLangImport,
    ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod)

  val transformations = {

    javaTransformations ++
      ssmTransformations
  }


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
