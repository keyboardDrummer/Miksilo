//package core.nabl
//
//import core.deltas.{Delta, WithLanguageRegistry}
//import core.language.Language
//
//class AddGoToDefinition extends Delta with WithLanguageRegistry {
//
//
//  override def inject(language: Language): Unit = {
////    language.capabilities += new GotoDefinition {
////      override def apply(position: Position): Range = ???
////    }
//    super.inject(language)
//  }
//
//  override def description: String = "Adds the go to definition capability"
//
//  override def createRegistry: AddGoToDefinition.this.type = ???
//}
