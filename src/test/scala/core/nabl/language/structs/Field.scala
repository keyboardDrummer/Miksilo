package core.nabl.language.structs

import core.nabl.language.modules.FakeSourceElement
import core.nabl.language.types.LanguageType

case class Field(name: String, _type: LanguageType) extends FakeSourceElement
