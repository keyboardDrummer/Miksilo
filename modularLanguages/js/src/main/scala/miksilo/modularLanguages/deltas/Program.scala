package miksilo.modularLanguages.deltas

import miksilo.languageServer.JSLanguageServer

object Program extends JSLanguageServer(Seq(
  SmithyLanguageBuilder,
  JsonLanguageBuilder,
  YamlLanguageBuilder
))