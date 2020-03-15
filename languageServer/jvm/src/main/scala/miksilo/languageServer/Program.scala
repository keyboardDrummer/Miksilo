package miksilo.languageServer

import languages.{JsonLanguage, YamlLanguage}
import miksilo.languageServer.server.SimpleLanguageBuilder

object Program extends JVMLanguageServer(Seq(
  SimpleLanguageBuilder("json", JsonLanguage),
  SimpleLanguageBuilder("yaml", YamlLanguage)))
