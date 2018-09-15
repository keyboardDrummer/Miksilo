package deltas.verilog

import core.deltas.Delta
import core.language.Language
import deltas.statement.BlockDelta

object VerilogLanguage {
  val deltas = Seq(AlwaysDelta, NonBlockingAssignmentDelta, BeginEndDelta, PortTypeSpecifierDelta, VerilogModuleDelta, BlockDelta)
  val language: Language = Delta.buildLanguage(deltas)
}
