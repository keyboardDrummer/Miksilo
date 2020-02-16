package core.smarts


import core.smarts.language.Program
import core.smarts.language.expressions.{Application, Const, Lambda, Variable}
import core.smarts.language.modules.{Binding, Module}
import core.smarts.language.types.IntType
import org.scalatest.funsuite.AnyFunSuite

class ReferenceEqualityTest extends AnyFunSuite with LanguageWriter {

  test("duplicateReference") {
    val identityType = IntType ==> IntType
    val moduleX = Module("moduleX", Seq(
      Binding("x", Const(3), Some(IntType)),
      Binding("y", Variable("x"), Some(IntType))))
    val moduleY = Module("moduleY", Seq(
      Binding("x", Lambda("y", Const(3), Some(IntType)), Some(identityType)),
      Binding("z", Application(Variable("x"), Const(2)), Some(IntType))))

    val program = Program(Seq(moduleX, moduleY))
    Checker.check(program)
  }
}
