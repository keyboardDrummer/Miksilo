package transformations.javac.base

import core.grammar.{Grammar, Lazy, seqr}
import core.transformation.{GrammarTransformation, MetaObject, ProgramTransformation, TransformationState}
import transformations.javac.LiteralC
import transformations.javac.base.model.JavaMethodModel.{DefaultVisibility, PrivateVisibility, ProtectedVisibility, PublicVisibility}
import transformations.javac.base.model.JavaTypes.VoidType
import transformations.javac.base.model._

import scala.collection.mutable

object JavaBaseParse extends GrammarTransformation {

  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = {}

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit
  = delimiters ++= Seq("(",")", "{", "}", ";", ".","[","]","[]")

  override def transformReserved(reserved: mutable.HashSet[String]): Unit =
    reserved ++= Seq("void", "class", "package", "public", "static", "String")

  override def transformGrammar(grammar: Grammar): Grammar = {
    val expression: Grammar = getExpressionGrammar
    lazy val statement: Grammar = expression <~ ";"
    lazy val block: Grammar = "{" ~> (statement *) <~ "}"
    lazy val parseReturnType = "void" ^^ (_ => VoidType)

    lazy val parseStringType = "String" ^^ (_ => JavaTypes.StringType)
    lazy val parseArrayType = parseType ~ "[]" ^^ { case _type seqr _ => JavaTypes.arrayType(_type) }
    lazy val parseType : Grammar = new Lazy(parseArrayType | parseStringType)

    lazy val parseParameter = parseType ~ identifier ^^
      {case _type seqr _name => JavaMethodModel.parameter(_name.asInstanceOf[String],_type)}
    lazy val parseParameters = "(" ~ parseParameter.someSeparated(",") ~ ")"
    lazy val parseStatic = "static" ^^ (_ => true) | produce(false)
    lazy val visibilityModifier =
      "public" ^^ (_ => PublicVisibility) |
      "protected" ^^ (_ => ProtectedVisibility) |
      "private" ^^ (_ => PrivateVisibility) |
      produce(DefaultVisibility)
    lazy val classMethod : Grammar = visibilityModifier ~ parseStatic ~ parseReturnType ~ identifier ~
      parseParameters ~ block

    lazy val classMember: Grammar = classMethod
    // lazy val _import = "import" ~> identifier.someSeparated(".") <~ ";"
    lazy val importsP: Grammar = produce(Seq.empty[JavaImport]) //success_import*
    lazy val packageP = keyword("package") ~> identifier.someSeparated(".") <~ ";"
    lazy val _classContent = "class" ~> identifier ~ ("{" ~> (classMember*) <~ "}")
    lazy val clazz = packageP ~ importsP ~ _classContent ^^ {
      case (_package seqr _imports) seqr (name seqr members) =>
        val methods = members
        JavaClassModel.clazz(_package.asInstanceOf[Seq[String]],
          name.asInstanceOf[String],
          methods.asInstanceOf[Seq[MetaObject]],
          _imports.asInstanceOf[List[JavaImport]], None)
    }
    clazz
  }

  def getExpressionGrammar: Grammar = {
    lazy val pNumber = number ^^ (number => LiteralC.literal(Integer.parseInt(number.asInstanceOf[String])))
    lazy val call = expression ~ ("(" ~> expression.manySeparated(",") <~ ")") ^^
      { case callee seqr arguments => JavaBaseModel.call(callee.asInstanceOf[MetaObject], arguments.asInstanceOf[Seq[MetaObject]]) }
    lazy val variable = identifier ^^ (name => JavaBaseModel.variable(name.asInstanceOf[String]))

    lazy val selection = (expression <~ ".") ~ identifier ^^ { case left seqr right => JavaBaseModel.selector(left.asInstanceOf[MetaObject], right.asInstanceOf[String])}

    lazy val expression: Grammar = new Lazy(call | selection | variable | pNumber)
    expression.named(ExpressionGrammar)
  }

  object ExpressionGrammar
}
