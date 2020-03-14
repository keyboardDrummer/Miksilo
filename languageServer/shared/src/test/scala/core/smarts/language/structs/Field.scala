package core.smarts.language.structs

import core.smarts.language.modules.FakeSourcePath
import core.smarts.language.types.LanguageType

case class Field(name: String, _type: LanguageType) extends FakeSourcePath
