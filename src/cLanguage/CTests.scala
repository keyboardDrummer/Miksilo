package cLanguage

import org.junit.{Assert, Test}
import cLanguage.LiteralInt._
import cLanguage.Variable._
class CTests {

  def runProgram(program: CProgram) {
    new CMachine().run(program)
  }

  def runStatementsAsMain(statements: Seq[Statement]) {
    val main = new Function("main",CInt,Seq(),new Block(statements))
    runProgram(new CProgram(Seq(main),Seq()))
  }

  @Test
  def testPointers1() {
    object TestStatement extends Statement {
      def execute(machine: CMachine): StatementResult = {
        Assert.assertEquals(2, machine.memory(machine.env("j").location))
        Done
      }
    }
    val globalVariables = Seq(new VariableDeclaration("j", CInt), new VariableDeclaration("k", CInt), new VariableDeclaration("ptr", new PointerType(CInt)))
    val statements = Seq[Statement](
      new Assignment("k", 2),
      new Assignment("ptr", new GetAddress("k")),
      new Assignment("j", new Dereference("ptr")),
      TestStatement,
      new Return(0)
    )
    val main = new Function("main",CInt,Seq(),new Block(statements))
    runProgram(new CProgram(Seq(main),globalVariables))
  }

  @Test
  def testSubtraction() {
    val statements = Seq[Statement](new VariableDeclaration("arr",new ArrayType(CInt,10))
      , new VariableDeclaration("p1", new PointerType(CInt), Some(new Add("arr", 2)))
      , new VariableDeclaration("p2", new PointerType(CInt), Some(new Add("arr", 2)))
      , new Statement {
        def execute(machine: CMachine): StatementResult = {
          val p2MinusP1 = new Subtract("p2", "p1").evaluate(machine)
          val p1MinusP2 = new Subtract("p1", "p2").evaluate(machine)
          Assert.assertEquals(p2MinusP1, 3)
          Assert.assertEquals(p1MinusP2, -3)
          Done
        }
      }
    )
    runStatementsAsMain(statements)
  }
}
