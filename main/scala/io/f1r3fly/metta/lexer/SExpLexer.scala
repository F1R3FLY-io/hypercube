package io.f1r3fly.metta.lexer

/**
 * The S-expression lexer.
 *
 * It defines a simple token model for S-expressions:
 * - LParen: "("
 * - RParen: ")"
 * - Atom: any sequence of non-whitespace, non-parenthesis characters.
 */
object SExprLexer {

  // Define our token ADT.
  sealed trait Token
  case object LParen extends Token
  case object RParen extends Token
  case class Atom(value: String) extends Token

  /**
   * Tokenize an input string into a List of Tokens.
   *
   * This lexer uses a regular expression that matches:
   * - Left parenthesis "("
   * - Right parenthesis ")"
   * - Or an atom (a sequence of characters that isn’t whitespace or a parenthesis)
   *
   * @param input the string to tokenize.
   * @return a List of Token values.
   */
  def tokenize(input: String): List[Token] = {
    // The regex: skip any leading whitespace; then match one of:
    // - a "(" captured in group 1
    // - a ")" captured in group 2
    // - or an atom captured in group 3.
    val tokenRegex = """\s*(?:(\()|(\))|([^()\s]+))""".r

    tokenRegex.findAllMatchIn(input).flatMap { m =>
      if (m.group(1) != null) Some(LParen)
      else if (m.group(2) != null) Some(RParen)
      else if (m.group(3) != null) Some(Atom(m.group(3)))
      else None
    }.toList
  }
}
