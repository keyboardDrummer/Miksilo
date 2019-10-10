package languageServer

import core.language.{FileSourceElement, Language, SourceElement, SourceElementFromFileElement}
import core.parsers.CommonStringReaderParser
import core.parsers.editorParsers.LeftRecursiveCorrectingParserWriter
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import util.SourceUtils2

// TODO add a constraintBuilder that's specific to a File, so you add a [Has]SourceRange instead of a SourceElement
// TODO compute the range based on the children, so only the leafs needs a Range.
trait Expression extends FileSourceElement {
  def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope)
}

case class SourceElementFromFileRange(_fileRange: FileRange) extends SourceElement {
  override def range = Some(_fileRange.range)

  override def uriOption = Some(_fileRange.uri)
}

case class Let(range: SourceRange, variableRange: SourceRange, variableName: String, variableValue: Expression, value: Expression) extends Expression {
  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
    val valueScope = builder.newScope(Some(scope))
    builder.declare(variableName, valueScope, SourceElementFromFileRange(new FileRange(uri, variableRange)))
    value.collectConstraints(builder, uri, valueScope)
  }

  override def childElements: Seq[Expression] = Seq(variableValue, value)
}

case class Number(range: SourceRange, value: Int) extends Expression {
  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
  }

  override def childElements: Seq[Nothing] = Seq.empty
}

case class Identifier(range: SourceRange, name: String) extends Expression {
  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
    builder.resolve(name, SourceElementFromFileElement(uri, this), scope)
  }

  override def childElements = Seq.empty
}

case class Addition(range: SourceRange, left: Expression, right: Expression) extends Expression {
  override def collectConstraints(builder: ConstraintBuilder, uri: String, scope: Scope): Unit = {
    left.collectConstraints(builder, uri, scope)
    right.collectConstraints(builder, uri, scope)
  }

  override def childElements = Seq(left, right)
}

object ExpressionParser extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter  {
  val whiteSpace = literalOrKeyword(" ").*

  val numberParser: Self[Expression] = wholeNumber.map(x => Integer.parseInt(x)).withSourceRange[Expression]((range, value) => Number(range, value))
  lazy val expression: Self[Expression] = new Lazy(addition | numberParser | let | variable)
  val addition: Self[Expression] = (expression ~< whiteSpace ~< "+" ~< whiteSpace ~ expression).withSourceRange[Expression]((range, value) => Addition(range, value._1, value._2))
  val variable: Self[Expression] = parseIdentifier.withSourceRange((range, value) => Identifier(range, value))
  val let: Self[Expression] = ("let" ~< whiteSpace ~> parseIdentifier.withSourceRange((r,x) => (r,x)) ~<
    whiteSpace ~< "=" ~< whiteSpace ~ expression ~< whiteSpace ~< "in" ~< whiteSpace ~ expression).withSourceRange[Expression]((range, t) => Let(range, t._1._1._1, t._1._1._2, t._1._2, t._2))

  val root = whiteSpace ~> expression
}

object ExpressionLanguage extends Language {
  private val parsePhase = Language.getParsePhaseFromParser[Expression, _root_.languageServer.ExpressionParser.StringReader](
    stream => {
      val program = SourceUtils2.streamToString(stream)
      new ExpressionParser.StringReader(program)
    },
    (program, uri) => SourceElementFromFileElement(uri, program),
    ExpressionParser.expression.getWholeInputParser)

  private val constraintPhase = Language.getConstraintPhase((compilation, builder) => {
    val rootElement = compilation.program.asInstanceOf[SourceElementFromFileElement]
    val rootExpression = rootElement.element.asInstanceOf[Expression]
    rootExpression.collectConstraints(builder, rootElement.uri, builder.newScope(debugName = "rootScope"))
  })

  compilerPhases = List(parsePhase, constraintPhase)
}

class ExampleExpressionLanguage extends LanguageServerTest {

  val language: Language = ExpressionLanguage
  val program = "let x = 3 in x + 3 + x"
  val server = new MiksiloLanguageServer(language)

  test("parse number") {
    assert(getDiagnostics(server, "3").isEmpty)
  }

  test("parse addition") {
    assert(getDiagnostics(server, "3 + 3").isEmpty)
  }

  test("parse let") {
    assert(getDiagnostics(server, "let x = 3 in x").isEmpty)
  }

  test("go to definition") {
    val firstXUsage = HumanPosition(1, 14)
    val secondXUsage = HumanPosition(1, 22)
    val xDefinition = SourceRange(HumanPosition(1, 5), HumanPosition(1, 6))

    assertResult(xDefinition)(gotoDefinition(server, program, firstXUsage).head.range)
    assertResult(xDefinition)(gotoDefinition(server, program, secondXUsage).head.range)
  }
}
