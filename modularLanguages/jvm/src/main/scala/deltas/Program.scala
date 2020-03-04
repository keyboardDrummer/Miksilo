package deltas

import deltas.javac.JavaLanguage
import deltas.json.JsonLanguage
import deltas.smithy.SmithyLanguage
import deltas.solidity.SolidityLanguage
import deltas.verilog.VerilogLanguage
import deltas.yaml.YamlLanguage
import jsonRpc.{JVMMessageReader, JVMMessageWriter, JVMQueue, JsonRpcConnection, WorkItem}
import languageServer.{LanguageBuilder, LanguageServerMain}

object VerilogLanguageBuilder extends LanguageBuilder {
  override def key = "verilog"
  override def build(arguments: collection.Seq[String]) = VerilogLanguage.language
}

object JavaLanguageBuilder extends LanguageBuilder {
  override def key = "java"

  override def build(arguments: collection.Seq[String]) = JavaLanguage.java
}

object SolidityLanguageBuilder extends LanguageBuilder {
  override def key = "solidity"

  override def build(arguments: collection.Seq[String]) = SolidityLanguage.language
}

object SmithyLanguageBuilder extends LanguageBuilder {
  override def key = "smithy"

  override def build(arguments: collection.Seq[String]) = SmithyLanguage.language
}

object JsonLanguageBuilder extends LanguageBuilder {
  override def key = "json"

  override def build(arguments: collection.Seq[String]) = JsonLanguage.language
}

object YamlLanguageBuilder extends LanguageBuilder {
  override def key = "yaml"

  override def build(arguments: collection.Seq[String]) = YamlLanguage.language
}

object Program extends LanguageServerMain(Seq(
  VerilogLanguageBuilder,
  JavaLanguageBuilder,
  SolidityLanguageBuilder,
  SmithyLanguageBuilder,
  JsonLanguageBuilder,
  YamlLanguageBuilder
), new JsonRpcConnection(new JVMMessageReader(System.in), new JVMMessageWriter(System.out)),
  new JVMQueue[WorkItem])

