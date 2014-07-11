package transformations.javac.base

import core.grammar.{Grammar, Labelled, seqr}
import core.transformation.TransformationManager.ProgramGrammar
import core.transformation.{GrammarCatalogue, GrammarTransformation, MetaObject}
import transformations.javac.base.model.JavaMethodModel._
import transformations.javac.base.model.JavaTypes.VoidType
import transformations.javac.base.model._
import transformations.javac.expressions.AddExpression

import scala.collection.mutable

trait JavaBaseParse extends GrammarTransformation {

  override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit
  = delimiters ++= Seq("(", ")", "{", "}", ";", ".", "[", "]", "[]")

  override def transformReserved(reserved: mutable.HashSet[String]): Unit =
    reserved ++= Seq("void", "class", "package", "public", "static", "int", "return")


  override def transformGrammars(grammars: GrammarCatalogue) {
    val expression = grammars.find(AddExpression.ExpressionGrammar)
    extendExpressionGrammar(grammars)
    val parseReturnValue = "return" ~> expression <~ ";" ^^ (expr => JavaMethodModel._return(Some(expr.asInstanceOf[MetaObject])))
    val statement: Grammar = expression <~ ";" | parseReturnValue
    val block: Grammar = "{" ~> (statement *) <~ "}"

    val parseType: Labelled = createParseType(grammars)

    val parseReturnType = "void" ^^ (_ => VoidType) | parseType

    val parseParameter = parseType ~ identifier ^^ { case _type seqr _name => JavaMethodModel.parameter(_name.asInstanceOf[String], _type)}
    val parseParameters = "(" ~> parseParameter.someSeparated(",") <~ ")"
    val parseStatic = "static" ^^ (_ => true) | produce(false)
    val visibilityModifier =
      "public" ^^ (_ => PublicVisibility) |
        "protected" ^^ (_ => ProtectedVisibility) |
        "private" ^^ (_ => PrivateVisibility) |
        produce(DefaultVisibility)
    val classMethod = grammars.create(MethodGrammar, visibilityModifier ~ parseStatic ~ parseReturnType ~ identifier ~
      parseParameters ~ block ^^ { case visibility seqr static seqr returnType seqr name seqr parameters seqr body =>
      JavaMethodModel.method(name.asInstanceOf[String], returnType, parameters.asInstanceOf[Seq[MetaObject]], body.asInstanceOf[Seq[MetaObject]],
        static.asInstanceOf[Boolean], visibility.asInstanceOf[Visibility])
    })

    val classMember: Grammar = classMethod
    // val _import = "import" ~> identifier.someSeparated(".") <~ ";"
    val importsP: Grammar = produce(Seq.empty[JavaImport]) //success_import*
    val packageP = keyword("package") ~> identifier.someSeparated(".") <~ ";"
    val _classContent = "class" ~> identifier ~ ("{" ~> (classMember *) <~ "}")
    val classGrammar = grammars.create(ClassGrammar, packageP ~ importsP ~ _classContent ^^ {
      case (_package seqr _imports) seqr (name seqr members) =>
        val methods = members
        JavaClassModel.clazz(_package.asInstanceOf[Seq[String]],
          name.asInstanceOf[String],
          methods.asInstanceOf[Seq[MetaObject]],
          _imports.asInstanceOf[List[JavaImport]], None)
    })
    grammars.find(ProgramGrammar).inner = classGrammar
  }

  def createParseType(grammars: GrammarCatalogue): Labelled = {
    val parseType = grammars.create(TypeGrammar)

    val parseObjectType = identifier.someSeparated(".") ^^ { case ids: Seq[Any] => {
      val stringIds = ids.collect({ case v: String => v})
      if (ids.size > 1)
        JavaTypes.objectType(new QualifiedClassName(stringIds))
      else
        JavaTypes.objectType(stringIds(0))
    }
    }
    val parseIntType = "int" ^^ (_ => JavaTypes.IntType)
    val parseArrayType = parseType ~ "[]" ^^ { case _type seqr _ => JavaTypes.arrayType(_type)}
    parseType.inner = parseArrayType | parseObjectType | parseIntType
    parseType
  }

  object TypeGrammar

  object ClassGrammar

  def extendExpressionGrammar(grammars: GrammarCatalogue) = {
    val expression = grammars.find(AddExpression.ExpressionGrammar)
    val callArguments: Grammar = "(" ~> expression.manySeparated(",") <~ ")"
    val parseCall = expression ~ callArguments ^^ { case callee seqr arguments => call(callee, arguments)}
    val variable = identifier ^^ (name => JavaBaseModel.variable(name.asInstanceOf[String]))

    val selection = (expression <~ ".") ~ identifier ^^ { case left seqr right => selector(left, right)}

    val parseParenthesis = "(" ~> expression <~ ")"
    expression.inner = grammars.create(ExpressionBaseGrammar, parseCall | selection | variable | parseParenthesis)
  }

  def call(callee: Any, arguments: Any) = JavaBaseModel.call(callee.asInstanceOf[MetaObject], arguments.asInstanceOf[Seq[MetaObject]])

  def selector(_object: Any, member: Any): MetaObject = JavaBaseModel.selector(_object.asInstanceOf[MetaObject], member.asInstanceOf[String])

  object MethodGrammar

  object ExpressionBaseGrammar

}
