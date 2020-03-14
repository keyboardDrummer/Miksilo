package languageServer

import core.{LambdaLogger, LazyLogging}
import jsonRpc._

class JVMLanguageServer(builders: Seq[LanguageBuilder]) extends LanguageServerMain(builders,
  new JsonRpcConnection(new JVMMessageReader(System.in), new JVMMessageWriter(System.out)),
  new JVMQueue[WorkItem]) {
  LazyLogging.logger = new LambdaLogger(s => System.err.println(s))
}
