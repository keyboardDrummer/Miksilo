package languageServer

import core.{LambdaLogger, LazyLogging}
import jsonRpc.{JSQueue, JsonRpcConnection, NodeMessageReader, NodeMessageWriter, WorkItem}

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}

class JSLanguageServer(builders: Seq[LanguageBuilder]) extends LanguageServerMain(builders,
  new JsonRpcConnection(
    new NodeMessageReader(g.process.stdin),
    new NodeMessageWriter(g.process.stdout)),
  new JSQueue[WorkItem]) {

  override def main(args: Array[String]): Unit = {
    LazyLogging.logger = new LambdaLogger(s => g.process.stderr.write(s))
    val nodeArgs = g.process.argv.asInstanceOf[js.Array[String]].drop(2).toArray
    super.main(nodeArgs)
  }
}
