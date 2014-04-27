package transformation

import languages.ssm._
import languages.bytecode._
import languages.javac.base.JavaBase
import languages.javac._
import grammar.{GrammarWriter, ToPackrat, Grammar, Success}
import grammar.Grammar
import grammar.FailureG

object TransformationManager extends ToPackrat {
  val ssmTransformations = Set(AddWhile, AddStatementToSSM, AddIfElse, AddBlock,
    AddDoWhile, AddIfElse, AddForLoop)

  val javaTransformations = Set[ProgramTransformation](JavaMinus, DefaultConstructor, LessThanC,
    ByteCode, LabelledJumps, JavaBase, TernaryC, SubtractionC, LiteralC, StringLiteralC, AdditionC,
    InferredMaxStack, InferredStackFrames, ImplicitThisInPrivateCalls, ConstructorC, ImplicitJavaLangImport,
    ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod)

  val transformations = {

    javaTransformations ++
      ssmTransformations
  }

  def buildParser(transformations: Seq[GrammarTransformation]): String => ParseResult[Any] = {
    var grammar: Grammar = FailureG.named("program")
    for (transformation <- transformations) {
      transformation.transformDelimiters(lexical.delimiters)
      transformation.transformReserved(lexical.reserved)
      grammar = transformation.transformGrammar(grammar)
    }
    val packratParser = convert(grammar)
    input => packratParser(new PackratReader(new lexical.Scanner(input)))
  }

  def buildCompiler(transformations: Seq[ProgramTransformation]): Compiler = {
    new Compiler {
      def compile(program: MetaObject) = {
        val state = new TransformationState
        for (transformation <- transformations)
          transformation.transform(program, state)
        program
      }
    }
  }
}
