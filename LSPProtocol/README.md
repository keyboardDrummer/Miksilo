LSPProtocol
=====
A library for implementing the communication layer of both clients and servers of the [language server protocol](https://microsoft.github.io/language-server-protocol/). This protocol defines how text editors can query language servers for information to provide a language aware editing experience.
 
Supports typical LSP streaming connections, where each message is wrapped in an envelope that describes its size. Also supports message passing connections where the messages are exchanged without envelop.
  
Supports defining servers and clients that run in the JVM, Node and the browser.