package miksilo.modularLanguages.deltas

import languageServer.JSLanguageServer

object Program extends JSLanguageServer(Seq(
  SmithyLanguageBuilder,
  JsonLanguageBuilder,
  YamlLanguageBuilder
))