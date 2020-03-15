package miksilo.modularLanguages.deltas

import miksilo.modularLanguages.deltas.javac.JavaLanguage
import languageServer.{JVMLanguageServer, SimpleLanguageBuilder}

object JavaLanguageBuilder extends SimpleLanguageBuilder("java", JavaLanguage.java)

object Program extends JVMLanguageServer(Languages.languages ++ Seq(JavaLanguageBuilder))

