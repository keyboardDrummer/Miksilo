package languageServer

import jsonRpc._
import miksilo.editorParser.{LambdaLogger, LazyLogging}
import miksilo.lspprotocol.jsonRpc.{JVMMessageReader, JVMMessageWriter, JVMQueue}

class JVMLanguageServer(builders: Seq[LanguageBuilder]) extends LanguageServerMain(builders,
  new JsonRpcConnection(new JVMMessageReader(System.in), new JVMMessageWriter(System.out)),
  new JVMQueue[WorkItem]) {
  LazyLogging.logger = new LambdaLogger(s => System.err.println(s))
}
