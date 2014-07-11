package core.transformation

import core.grammar.{FailureG, ToPackrat}
import core.transformation.TransformationManager.ProgramGrammar
import transformations.bytecode._
import transformations.javac._
import transformations.javac.base.JavaBase
import transformations.javac.expressions._
import transformations.ssm._

class TransformationManager extends ToPackrat {
  def buildParser(transformations: Seq[GrammarTransformation]): String => ParseResult[Any] = {
    var grammars: GrammarCatalogue = new GrammarCatalogue()
    grammars.create(ProgramGrammar, FailureG)
    for (transformation <- transformations) {
      transformation.transformDelimiters(lexical.delimiters)
      transformation.transformReserved(lexical.reserved)
      transformation.transformGrammars(grammars)
    }
    val packratParser = phrase(convert(grammars.find(ProgramGrammar)))
    input => packratParser(new PackratReader(new lexical.Scanner(input)))
  }
}

object TransformationManager {

  object ProgramGrammar

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
