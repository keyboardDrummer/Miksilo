package deltas.verilog

import core.language.node.NodeComparer
import deltas.expression.IntLiteralDelta
import deltas.expressions.VariableDelta
import deltas.statement.{IfThenDelta, IfThenElseDelta}
import org.scalatest.FunSuite
import util.TestLanguageBuilder

class VerilogTest extends FunSuite {

  val code = """ module arbiter (
               | clock,
               | reset,
               | req_0,
               | req_1,
               | gnt_0,
               | gnt_1
               | );
               |
               | input clock, reset, req_0, req_1;
               | output gnt_0, gnt_1;
               |
               | reg gnt_0, gnt_1;
               |
               | always @ (posedge clock or posedge reset)
               | if (reset) begin
               |   gnt_0 <= 0;
               |   gnt_1 <= 0;
               | end else if (req_0) begin
               |   gnt_0 <= 1;
               |   gnt_1 <= 0;
               | end else if (req_1) begin
               |   gnt_0 <= 0;
               |   gnt_1 <= 1;
               | end
               |
               | endmodule""".stripMargin

  test("Can parse") {
    val language = TestLanguageBuilder.build(VerilogLanguage.deltas)
    val actual = language.parse(code)

    val thirdIf = IfThenDelta.neww(VariableDelta.neww("req_1"), BeginEndDelta.neww(Seq(
      NonBlockingAssignmentDelta.neww("gnt_0", IntLiteralDelta.neww(0)),
      NonBlockingAssignmentDelta.neww("gnt_1", IntLiteralDelta.neww(1)))))
    val secondIf = IfThenElseDelta.neww(VariableDelta.neww("req_0"), BeginEndDelta.neww(Seq(
      NonBlockingAssignmentDelta.neww("gnt_0", IntLiteralDelta.neww(1)),
      NonBlockingAssignmentDelta.neww("gnt_1", IntLiteralDelta.neww(0)))),
      thirdIf)
    val firstIf = IfThenElseDelta.neww(VariableDelta.neww("reset"), BeginEndDelta.neww(Seq(
      NonBlockingAssignmentDelta.neww("gnt_0", IntLiteralDelta.neww(0)),
      NonBlockingAssignmentDelta.neww("gnt_1", IntLiteralDelta.neww(0)))),
      secondIf)
    val moduleMembers = Seq(
      PortTypeSpecifierDelta.neww("input", Seq("clock", "reset", "req_0", "req_1")),
      PortTypeSpecifierDelta.neww("output", Seq("gnt_0", "gnt_1")),
      PortTypeSpecifierDelta.neww("reg", Seq("gnt_0", "gnt_1")),
      AlwaysDelta.neww(
        Seq(SensitivityVariable.neww("posedge", "clock"),
          SensitivityVariable.neww("posedge", "reset")),
        firstIf)
    )
    val ports = Seq("clock", "reset", "req_0", "req_1", "gnt_0", "gnt_1")
    val expectation = VerilogModuleDelta.neww("arbiter", ports, moduleMembers)

    assert(NodeComparer().deepEquality(expectation, actual))
  }
}
