---
title: Build language tooling
category: Practical
order: 2
---

Miksilo greatly simplifies creating language tooling. Creating language tooling with features like inline errors, code completion, code navigation and rename refactoring generally is a giant undertaking that takes months. Miksilo allows you to create tooling for a small language in a day. All that's needed is to specify a language definition in Miksilo.

Miksilo can provide tooling for many different editors by leveraging the [language server protocol](https://langserver.org/), which is a way for editors to connect to language tooling providers like Miksilo.

[Visual Studio Code](https://code.visualstudio.com/) is the easiest editor to use Miksilo with, since Miksilo comes with a VS Code extension.

If you're building language tooling for some language, the easiest way to preview what you built is by using the Miksilo extension for Visual Studio Code. You can do this with the following steps:

- Let the Miksilo LSP server know your language by adding it to the `languages` variable in `languageServer/src/main/scala/languageServer/Program.scala`.
- Let the Miksilo VS Code extension know your language by configuring `vscode-extension/package.json` and the `languages` variable in `vscode-extension/src/extension.ts`
- Make VS Code available on your path by following [these instructions](https://code.visualstudio.com/docs/setup/mac#_launching-from-the-command-line).
- Go to the Miksilo root folder and run `sbt vscode`. This will:
  - Assemble the Miksilo LSP server as a `.jar` file.
  - Compile the Miksilo extension for VS Code in your repository.
  - Start VS Code with the VS Code Miksilo extension enabled and passing the path to the assembled `.jar`.