package io.f1r3fly.metta.parser

import scala.util.parsing.combinator.RegexParsers

/**
 * A class-based S-expression parser using Scala's parser combinators.
 */
class SExprParser extends RegexParsers {

  /**
   * Parser for an atom.
   * An atom is defined as a sequence of characters that is not whitespace or parentheses.
   */
  def atom: Parser[SAtom] =
    """[^()\s]+""".r ^^ { str => SAtom(str) }

  /**
   * Parser for a list of S-expressions.
   * A list is enclosed in parentheses and may contain zero or more S-expressions.
   */
  def list: Parser[SList] =
    "(" ~> rep(sexpr) <~ ")" ^^ { exprs => SList(exprs) }

  /**
   * Top-level parser for an S-expression.
   * An S-expression is either an atom or a list.
   */
  def sexpr: Parser[SExpr] = list | atom

  /**
   * Parser for a program.
   *
   * For the moment, a program is defined as a non-empty sequence of S-expressions.
   * (The actual check that there is at least one newline is done in the companion object's helper method.)
   */
  def program: Parser[List[SExpr]] = rep1(sexpr)
}

/**
 * Companion object providing convenient parsing methods.
 */
object SExprParser {

  /**
   * Parse a single S-expression from the input.
   *
   * @param input the input string to parse
   * @return either a Right(SExpr) on success, or a Left(error message) on failure.
   */
  def apply(input: String): Either[String, SExpr] = {
    val parser = new SExprParser()
    parser.parseAll(parser.sexpr, input) match {
      case parser.Success(result, _) => Right(result)
      case parser.NoSuccess(msg, _)  => Left(s"Error parsing s-expression: $msg")
    }
  }

  /**
   * Parse a program (a sequence of S-expressions) from the input.
   *
   * This method enforces that the input contains at least one newline (or carriage return)
   * to distinguish a program (which is expected to span multiple lines) from a single S-expression.
   *
   * @param input the input string to parse
   * @return either a Right(List[SExpr]) on success, or a Left(error message) on failure.
   */
  def parseProgram(input: String): Either[String, List[SExpr]] = {
    // Check that the input contains at least one newline or carriage return.
    if (!input.exists(ch => ch == '\n' || ch == '\r'))
      Left("Error parsing program: input must contain at least one newline")
    else {
      val parser = new SExprParser()
      parser.parseAll(parser.program, input) match {
        case parser.Success(result, _) => Right(result)
        case parser.NoSuccess(msg, _)  => Left(s"Error parsing program: $msg")
      }
    }
  }
}
