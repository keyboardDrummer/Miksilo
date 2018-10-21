---
title: Build language tooling
category: Practical
order: 2
---

Creating language tooling with features like inline errors, code completion, code navigation and rename refactoring is generally a giant undertaking that takes months. Miksilo greatly simplifies this process, and allows you to create tooling for a small language in a day. All that's needed is to specify a language definition.

Miksilo provides tooling for many different editors by leveraging the [language server protocol](https://langserver.org/), which is a way for editors to connect to language tooling providers like Miksilo. Given a language definition, Miksilo can start an LSP server for this language, which other editors can connect to.

The easiest way to preview language tooling built with Miksilo is by using the Miksilo extension for [Visual Studio Code](https://code.visualstudio.com/). To do that, follow these steps:

1. Let the Miksilo LSP server know your language by adding it to the `languages` variable in `languageServer/src/main/scala/languageServer/Program.scala`.
1. Let the Miksilo VS Code extension know your language by configuring `vscode-extension/package.json` and the `languages` variable in `vscode-extension/src/extension.ts`
1. Make VS Code available on your path by following [these instructions](https://code.visualstudio.com/docs/setup/mac#_launching-from-the-command-line).
1. Go to the Miksilo root folder and run `sbt vscode`. This will:
   - Assemble the Miksilo LSP server as a `.jar` file.
   - Compile the Miksilo extension for VS Code in your repository.
   - Start VS Code with the VS Code Miksilo extension enabled and configured with the path to the assembled `.jar`.