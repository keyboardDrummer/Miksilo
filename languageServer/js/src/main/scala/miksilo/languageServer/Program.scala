package miksilo.languageServer

import miksilo.languageServer.JSLanguageServer
import miksilo.languageServer.languages.{JsonLanguage, YamlLanguage}

object Program extends JSLanguageServer(Seq(
  SimpleLanguageBuilder("json", JsonLanguage),
  SimpleLanguageBuilder("yaml", YamlLanguage)))
