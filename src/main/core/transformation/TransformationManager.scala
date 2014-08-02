package core.transformation

import core.grammar.{FailureG, ParseException, ToPackrat}
import core.transformation.TransformationManager.ProgramGrammar
import transformations.bytecode._
import transformations.javac._
import transformations.javac.base.JavaMethodC
import transformations.javac.expressions._
import transformations.ssm._

class TransformationManager extends ToPackrat {
  def buildParser(transformations: Seq[GrammarTransformation]): String => ParseResult[Any] = {
    val grammars: GrammarCatalogue = new GrammarCatalogue()
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

case class TransformationDependencyViolation(dependency: Contract, dependent: Contract) extends CompilerException {
  override def toString = s"dependency '${dependency.name}' from '${dependent.name}' is not satisfied"
}

object TransformationManager {

  val ssmTransformations = Set(AddWhile, AddStatementToSSM, AddIfElse, AddBlock,
    AddDoWhile, AddIfElse, AddForLoop)
  val javaTransformations = Set[Contract](JavaMinus, DefaultConstructor, LessThanC,
    ByteCode, LabelledJumps, JavaMethodC, TernaryC, SubtractionC, LiteralC, AdditionC,
    InferredMaxStack, InferredStackFrames, ImplicitThisInPrivateCalls, ConstructorC, ImplicitJavaLangImport,
    ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod)
  val transformations = {
    javaTransformations ++
      ssmTransformations
  }

  def buildCompiler(transformations: Seq[ProgramTransformation]): Compiler = {
    validateDependencies(transformations)
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
          throw new ParseException(parseResult.toString)
        parseResult.get.asInstanceOf[MetaObject]
      }
    }
  }

  def validateDependencies(transformations: Seq[ProgramTransformation]) = {
    var available = Set.empty[Contract]
    for (transformation <- transformations.reverse) {
      transformation.dependencies.collect({ case dependency: ProgramTransformation =>
        if (!available.contains(dependency))
          throw new TransformationDependencyViolation(dependency, transformation)
      })
      available += transformation
    }
  }

  object ProgramGrammar

}
