package miksilo.languageServer.core.smarts

import miksilo.languageServer.core.smarts.language.Program
import miksilo.languageServer.core.smarts.language.expressions.{Const, Variable}
import miksilo.languageServer.core.smarts.language.modules.{Binding, Module, ModuleImport}
import miksilo.languageServer.core.smarts.language.types.IntType
import org.scalatest.funsuite.AnyFunSuite

class Modules extends AnyFunSuite with LanguageWriter  {

  test("module") {
    val moduleWithX = Module("hasX", Seq(Binding("x", Const(3), Some(IntType))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(Binding("y", Variable("x"), Some(IntType))), imports = Seq(new ModuleImport("hasX")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    Checker.check(program)
  }

  test("moduleFail") {
    val moduleWithX = Module("hasX", Seq(Binding("x", Const(3), Some(IntType))))
    val moduleThatImportsHasX = Module("importsHasX", Seq(Binding("y", Variable("x"), Some(IntType))), imports = Seq(new ModuleImport("hasY")))
    val program = Program(Seq(moduleThatImportsHasX, moduleWithX))
    Checker.fail(program)
  }
}
