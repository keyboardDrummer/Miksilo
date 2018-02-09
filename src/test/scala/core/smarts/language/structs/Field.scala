package core.smarts.language.structs

import core.smarts.language.modules.FakeSourceElement
import core.smarts.language.types.LanguageType

case class Field(name: String, _type: LanguageType) extends FakeSourceElement
