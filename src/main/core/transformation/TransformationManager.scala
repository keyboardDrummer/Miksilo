package core.transformation

import transformations.ssm._
import transformations.bytecode._
import transformations.javac.base.JavaBase
import transformations.javac._
import core.grammar.ToPackrat
import core.grammar.Grammar
import core.grammar.FailureG

class TransformationManager extends ToPackrat {
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
}

object TransformationManager {
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
