package core.smarts

import core.smarts.language.expressions._
import core.smarts.language.types.{IntType, LanguageForAllType, LanguageTypeVariable}
import org.scalatest.FunSuite

class PolymorphicFunctions extends FunSuite with LanguageWriter {

  test("boolIntoIdentity") {
    val program = Application(Lambda("x", Variable("x")), BoolConst(true))
    Checker.failExpression(program)
  }

  test("lambda") {
    val program = Lambda("x", Variable("x"))
    Checker.failExpression(program)
  }

  test("lambda2") {
    val program = Lambda("x", Variable("x"))
    Checker.checkExpression(program, LanguageTypeVariable("jo"))
  }

  test("letIdentity") {
    val program = Let("identity", Lambda("x", Variable("x")), Const(3))
    Checker.checkExpression(program)
  }

  test("letIdentity2") {
    val program = Let("identity", Lambda("x", Variable("x")), Const(3))
    Checker.checkExpression(program)
  }

  test("identitySquareIsNoInt") {
    val identity = Lambda("x", Variable("x"))
    val identity2 = Lambda("x", Variable("x"))
    val program = Application(identity, identity2)
    Checker.failExpression(program)
  }

  test("identitySquareIsNoInt2") {
    val identity = Lambda("x", Variable("x"))
    val identity2 = Lambda("x", Variable("x"))
    val program = Add(Application(identity, identity2), Const(2))
    Checker.failExpression(program)
  }

  test("lambdaApplication") {
    val program = Application(Lambda("x", Variable("x")), Const(3))
    Checker.checkExpression(program)
  }

  test("reuseIdentity") {
    val identity = Lambda("x", Variable("x"))
    val program = Let("identity", identity, Application(Application(Variable("identity"), Variable("identity")), Const(3)))
    Checker.checkExpression(program, IntType)
  }

  test("reuseIdentityFail") {
    val identity = Lambda("x", Variable("x"))
    val program = Let("identity", identity, Application(Application(Variable("identity"), Variable("identity")), BoolConst(true)))
    Checker.failExpression(program, IntType)
  }

  test("lambdaDoesNotGeneralize") {
    val identity = Lambda("x", Variable("x"))
    val program = Application(Lambda("identity", Application(Application(Variable("identity"), Variable("identity")), Const(3))), identity)
    Checker.checkExpression(program)
  }

  test("referenceIdentityChangeType") {
    val identity = Lambda("x", Variable("x"))
    val program = Let("identity", identity, Application(Variable("identity"), BoolConst(true)))
    Checker.failExpression(program)
  }

  test("referenceIdentityChangeType2") {
    val identity = Lambda("x", Variable("x"))
    val program = Let("identity", identity, Application(Variable("identity"), BoolConst(true)))
    Checker.failExpression(program)
  }

  test("referenceIdentityChangeType3") {
    val identity = Lambda("x", Variable("x"))
    val program = Let("identity", identity, Application(Variable("identity"), BoolConst(true)))
    Checker.failExpression(program)
  }

  test("reuseIdentity2") {
    val identity = Lambda("x", Variable("x"))
    val program = Let("identity", identity, Application(Variable("identity"), Application(Variable("identity"), Const(3))))
    Checker.checkExpression(program)
  }

  test("const" ) {
    val const = Lambda("x", Lambda("y", Variable("x")))
    val program = Let("const", const, Application(Application(Variable("const"), Const(3)), BoolConst(true)))
    Checker.checkExpression(program)
  }

  test("const2" ) {
    val const = Lambda("x", Lambda("y", Variable("x")))
    val program = Let("const", const, Let("constSquare", Application(Variable("const"), Variable("const")),
      Application(Application(Application(Variable("constSquare"), BoolConst(true)), Const(3)), LongConst(4))))
    Checker.checkExpression(program)
  }

  test("constFail" ) {
    val const = Lambda("x", Lambda("y", Variable("x")))
    val program = Let("const", const, Let("constSquare", Application(Variable("const"), Variable("const")),
      Application(Application(Variable("constSquare"), Const(2)), Const(3))))
    Checker.failExpression(program)
  }

  test("identity with let type")
  {
    val identity = Lambda("x", Variable("x"))
    val program = Let("identity", identity, Application(Variable("identity"), Const(3)),
      Some(LanguageForAllType("a", LanguageForAllType("a", LanguageTypeVariable("a") ==> LanguageTypeVariable("a")))))
    Checker.checkExpression(program)
  }
}
