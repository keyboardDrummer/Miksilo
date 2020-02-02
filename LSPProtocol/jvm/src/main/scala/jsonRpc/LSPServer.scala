package jsonRpc

import lsp.{LanguageServer, SharedLSPServer}

class LSPServer(languageServer: LanguageServer,
                connection: JsonRpcConnection)
  extends SharedLSPServer(languageServer, connection, new JVMQueue[WorkItem]) {

}
