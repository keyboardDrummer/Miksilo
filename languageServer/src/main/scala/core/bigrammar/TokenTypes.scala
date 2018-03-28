package core.bigrammar

object TokenTypes { //TODO this should be replaced by something more disciplined.
  /**
    * Tokens of type <code>NULL</code> mark the end of lines with no
    * multi-line token at the end (such as a block comment in C++).
    */
  val NULL = 0
  val COMMENT_EOL = 1
  val COMMENT_MULTILINE = 2
  val COMMENT_DOCUMENTATION = 3
  val COMMENT_KEYWORD = 4
  val COMMENT_MARKUP = 5
  val RESERVED_WORD = 6
  val RESERVED_WORD_2 = 7
  val FUNCTION = 8
  val LITERAL_BOOLEAN = 9
  val LITERAL_NUMBER_DECIMAL_INT = 10
  val LITERAL_NUMBER_FLOAT = 11
  val LITERAL_NUMBER_HEXADECIMAL = 12
  val LITERAL_STRING_DOUBLE_QUOTE = 13
  val LITERAL_CHAR = 14
  val LITERAL_BACKQUOTE = 15
  val DATA_TYPE = 16
  val VARIABLE = 17
  val REGEX = 18
  val ANNOTATION = 19
  val IDENTIFIER = 20
  val WHITESPACE = 21
  val SEPARATOR = 22
  val OPERATOR = 23
  val PREPROCESSOR = 24
  val MARKUP_TAG_DELIMITER = 25
  val MARKUP_TAG_NAME = 26
  val MARKUP_TAG_ATTRIBUTE = 27
  val MARKUP_TAG_ATTRIBUTE_VALUE = 28
  val MARKUP_COMMENT = 29
  val MARKUP_DTD = 30
  val MARKUP_PROCESSING_INSTRUCTION = 31
  val MARKUP_CDATA_DELIMITER = 32
  val MARKUP_CDATA = 33
  val MARKUP_ENTITY_REFERENCE = 34
  val ERROR_IDENTIFIER = 35
  val ERROR_NUMBER_FORMAT = 36
  val ERROR_STRING_DOUBLE = 37
  val ERROR_CHAR = 38
  val DEFAULT_NUM_TOKEN_TYPES = 39
}
