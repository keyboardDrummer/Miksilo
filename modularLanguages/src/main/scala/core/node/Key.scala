package core.language.node

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

//@compileTimeOnly("enable macro paradise to expand macro annotations")
//class named extends StaticAnnotation {
//  def macroTransform(annottees: Any*): Any = macro AddNamed.impl
//}
//
//object AddNamed {
//  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
//    import c.universe._
//
//    val result = {
//      annottees.map(_.tree).toList match {
//        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" :: Nil =>
//          val className = tpname.toString()
//          q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents {
//            $stats
//
//            override lazy val toString: String = $className
//            override def hashCode(): Int = ${className}
//          }"""
//        case _ => c.abort(c.enclosingPosition, "Annotation @named can be used only on classes")
//      }
//    }
//    c.Expr[Any](result)
//  }
//}

trait Key extends AnyRef
{
//  override lazy val toString: String = debugRepresentation

  /**
    * This hashcode does not change over runs, while the default hashcode does.
    * This makes the compilation process more deterministic.
    */
  override def hashCode(): Int = this.getClass.toString.hashCode
//
//  def debugRepresentation: String = this match {
//    case anyRef: AnyRef =>
//      try
//      {
//        val shape = anyRef.getClass
//        getClassName(shape)
//      }
//      catch
//        {
//          case e: java.lang.InternalError => e.toString
//        }
//    case _ => this.toString
//  }
//
//  private def getClassName(shape: Class[_]): String = {
//    "blurp"
////    val enclosing: Class[_] = shape.getEnclosingClass
////    val addition = if (enclosing == null) "" else getClassName(enclosing) + "."
////    addition + getDirectClassName(shape)
//  }
//
//  protected def getDirectClassName(shape: Class[_]): String = {
//    val simpleName: String = shape.getSimpleName
//    if (simpleName.last == '$')
//      simpleName.dropRight(1)
//    else
//      simpleName
//  }
}

/**
  * Defines a field for a Node
  */
trait NodeField extends GrammarKey {
//  override def debugRepresentation: String = this match {
//    case anyRef: AnyRef =>
//      try
//      {
//        val shape = anyRef.getClass
//        getDirectClassName(shape)
//      }
//      catch
//        {
//          case e: java.lang.InternalError => e.toString
//        }
//    case _ => this.toString
//  }
}

trait GrammarKey extends Key