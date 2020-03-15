package languageServer

import languages.{JsonLanguage, YamlLanguage}

object Program extends JSLanguageServer(Seq(
  SimpleLanguageBuilder("json", JsonLanguage),
  SimpleLanguageBuilder("yaml", YamlLanguage)))
