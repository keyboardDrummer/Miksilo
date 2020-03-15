package miksilo.modularLanguages.deltas

import miksilo.modularLanguages.deltas.javac.JavaLanguage
import miksilo.languageServer.JVMLanguageServer
import miksilo.languageServer.server.SimpleLanguageBuilder

object JavaLanguageBuilder extends SimpleLanguageBuilder("java", JavaLanguage.java)

object Program extends JVMLanguageServer(Languages.languages ++ Seq(JavaLanguageBuilder))

