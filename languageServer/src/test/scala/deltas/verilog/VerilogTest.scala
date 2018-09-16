package deltas.verilog

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
    val node = language.parse(code)
  }
}
