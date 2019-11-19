package deltas

import deltas.javac.JavaLanguage
import deltas.smithy.SmithyLanguage
import deltas.solidity.SolidityLanguage
import deltas.verilog.VerilogLanguage
import jsonRpc.{JVMJsonRpcConnection, JsonRpcConnection}
import languageServer.{LanguageBuilder, LanguageServerMain}

object VerilogLanguageBuilder extends LanguageBuilder {
  override def key = "verilog"
  override def build(arguments: Seq[String]) = VerilogLanguage.language
}

object JavaLanguageBuilder extends LanguageBuilder {
  override def key = "java"

  override def build(arguments: Seq[String]) = JavaLanguage.java
}

object SolidityLanguageBuilder extends LanguageBuilder {
  override def key = "solidity"

  override def build(arguments: Seq[String]) = SolidityLanguage.language
}

object SmithyLanguageBuilder extends LanguageBuilder {
  override def key = "smithy"

  override def build(arguments: Seq[String]) = SmithyLanguage.language
}

object Program extends LanguageServerMain(Seq(
  VerilogLanguageBuilder,
  JavaLanguageBuilder,
  SolidityLanguageBuilder,
  SmithyLanguageBuilder
), new JVMJsonRpcConnection(System.in, System.out))

