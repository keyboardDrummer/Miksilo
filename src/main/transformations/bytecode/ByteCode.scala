package transformations.bytecode

import core.transformation.Contract
import transformations.bytecode.instructions._

object ByteCode extends Contract {
  override def dependencies: Set[Contract] = Set(AddIntegersC, GetStaticC, GotoC, IfIntegerCompareGreaterC,
    IfZeroC, IncrementIntegerC, IntegerConstantC, IntegerReturnC, InvokeSpecialC, InvokeVirtualC, InvokeStaticC,
    LoadAddressC, LoadIntegerC, PushNullC, StoreAddressC, StoreIntegerC, SubtractIntegerC, VoidReturnC)
}
