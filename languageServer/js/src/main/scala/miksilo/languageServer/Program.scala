package miksilo.languageServer

dd .
import miksilo.languageServer.languages.{JsonLanguage, YamlLanguage}
import miksilo.languageServer.server.SimpleLanguageBuilder

object Program extends JSLanguageServer(Seq(
  SimpleLanguageBuilder("json", JsonLanguage),
  SimpleLanguageBuilder("yaml", YamlLanguage)))
