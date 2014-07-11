package transformations.javac.base

import core.grammar.{Grammar, Labelled, Lazy, seqr}
import core.transformation.{GrammarTransformation, MetaObject}
import transformations.javac.base.model.JavaMethodModel.{DefaultVisibility, PrivateVisibility, ProtectedVisibility, PublicVisibility}
import transformations.javac.base.model.JavaTypes.VoidType
import transformations.javac.base.model._

import scala.collection.mutable

trait JavaBaseParse extends GrammarTransformation {

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit
  = delimiters ++= Seq("(",")", "{", "}", ";", ".","[","]","[]")

  override def transformReserved(reserved: mutable.HashSet[String]): Unit =
    reserved ++= Seq("void", "class", "package", "public", "static", "int", "return")

  override def transformGrammar(grammar: Grammar): Grammar = {
    val expression: Grammar = getExpressionGrammar
    lazy val parseReturnValue = "return" ~> expression <~ ";" ^^ (expr => JavaMethodModel._return(Some(expr.asInstanceOf[MetaObject])))
    lazy val statement: Grammar = expression <~ ";" | parseReturnValue
    lazy val block: Grammar = "{" ~> (statement *) <~ "}"
    lazy val parseReturnType = "void" ^^ (_ => VoidType) | parseType

    lazy val parseObjectType = identifier.someSeparated(".") ^^ { case ids: Seq[Any] => {
      val stringIds = ids.collect({ case v: String => v})
      if (ids.size > 1)
        JavaTypes.objectType(new QualifiedClassName(stringIds))
      else
        JavaTypes.objectType(stringIds(0))
    }
    }
    lazy val parseIntType = "int" ^^ (_ => JavaTypes.IntType)
    lazy val parseArrayType = parseType ~ "[]" ^^ { case _type seqr _ => JavaTypes.arrayType(_type) }
    lazy val parseType : Grammar = new Lazy(parseArrayType | parseObjectType | parseIntType)

    lazy val parseParameter = parseType ~ identifier ^^
      {case _type seqr _name => JavaMethodModel.parameter(_name.asInstanceOf[String],_type)}
    lazy val parseParameters = "(" ~> parseParameter.someSeparated(",") <~ ")"
    lazy val parseStatic = "static" ^^ (_ => true) | produce(false)
    lazy val visibilityModifier =
      "public" ^^ (_ => PublicVisibility) |
      "protected" ^^ (_ => ProtectedVisibility) |
      "private" ^^ (_ => PrivateVisibility) |
      produce(DefaultVisibility)
    lazy val classMethod : Grammar = (visibilityModifier ~ parseStatic ~ parseReturnType ~ identifier ~
      parseParameters ~ block ^^ { case visibility seqr static seqr returnType seqr name seqr parameters seqr body =>
        JavaMethodModel.method(name.asInstanceOf[String],returnType,parameters.asInstanceOf[Seq[MetaObject]], body.asInstanceOf[Seq[MetaObject]],
          static.asInstanceOf[Boolean], visibility.asInstanceOf[JavaMethodModel.Visibility])
    }).named(MethodGrammar)

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
    val expression = new Labelled(ExpressionGrammar)
    val callArguments: Grammar = "(" ~> expression.manySeparated(",") <~ ")"
    val parseCall = expression ~ callArguments ^^ { case callee seqr arguments => call(callee, arguments) }
    val variable = identifier ^^ (name => JavaBaseModel.variable(name.asInstanceOf[String]))

    val selection = (expression <~ ".") ~ identifier ^^ { case left seqr right => selector(left, right) }

    val parseParenthesis = "(" ~> expression <~ ")"
    expression.inner = new Labelled(ExpressionBaseGrammar, parseCall | selection | variable | parseParenthesis)
    expression
  }

  def call(callee: Any, arguments: Any) = JavaBaseModel.call(callee.asInstanceOf[MetaObject], arguments.asInstanceOf[Seq[MetaObject]])

  def selector(_object: Any, member: Any) : MetaObject = JavaBaseModel.selector(_object.asInstanceOf[MetaObject], member.asInstanceOf[String])

  object MethodGrammar
  object ExpressionGrammar
  object ExpressionBaseGrammar
}
