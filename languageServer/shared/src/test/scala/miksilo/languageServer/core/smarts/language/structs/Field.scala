package miksilo.languageServer.core.smarts.language.structs

import miksilo.languageServer.core.smarts.language.modules.FakeSourcePath
import miksilo.languageServer.core.smarts.language.types.LanguageType

case class Field(name: String, _type: LanguageType) extends FakeSourcePath
