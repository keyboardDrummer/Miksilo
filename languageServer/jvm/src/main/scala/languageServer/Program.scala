package languageServer

import languages.{JsonLanguage, YamlLanguage}

object Program extends JVMLanguageServer(Seq(
  SimpleLanguageBuilder("json", JsonLanguage),
  SimpleLanguageBuilder("yaml", YamlLanguage)))
