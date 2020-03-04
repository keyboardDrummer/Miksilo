package deltas

import core.{LambdaLogger, LazyLogging}
import deltas.javac.JavaLanguage
import jsonRpc._
import languageServer.{LanguageBuilder, LanguageServerMain}

object JavaLanguageBuilder extends LanguageBuilder {
  override def key = "java"

  override def build(arguments: collection.Seq[String]) = JavaLanguage.java
}

object Program extends LanguageServerMain(Languages.languages ++ Seq(JavaLanguageBuilder),
  new JsonRpcConnection(new JVMMessageReader(System.in), new JVMMessageWriter(System.out)),
  new JVMQueue[WorkItem]) {

  LazyLogging.logger = new LambdaLogger(s => System.err.println(s))
}

