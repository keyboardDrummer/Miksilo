package core.transformation

import core.grammar.{FailureG, Grammar, ToPackrat}
import transformations.bytecode._
import transformations.javac._
import transformations.javac.base.JavaBase
import transformations.javac.expressions._
import transformations.ssm._

class TransformationManager extends ToPackrat {
  def buildParser(transformations: Seq[GrammarTransformation]): String => ParseResult[Any] = {
    var grammar: Grammar = FailureG.named("program") //TODO clean this. Probably can do that with Grammar sets.
    for (transformation <- transformations) {
      transformation.transformDelimiters(lexical.delimiters)
      transformation.transformReserved(lexical.reserved)
      grammar = transformation.transformGrammar(grammar)
    }
    val packratParser = phrase(convert(grammar))
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
    val manager = new TransformationManager()
    val parser = manager.buildParser(transformations.collect({ case t: GrammarTransformation => t}).reverse)
    new Compiler {
      def transform(program: MetaObject) = {
        val state = new TransformationState
        for (transformation <- transformations)
          transformation.transform(program, state)
        program
      }

      override def parse(input: String): MetaObject = {
        val parseResult = parser(input)
        if (!parseResult.successful)
          throw new RuntimeException(parseResult.toString)
        parseResult.get.asInstanceOf[MetaObject]
      }
    }
  }
}
