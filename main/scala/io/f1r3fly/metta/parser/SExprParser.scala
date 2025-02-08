package io.f1r3fly.metta.parser

import io.f1r3fly.metta.lexer.SExprLexer
import io.f1r3fly.metta.lexer.SExprLexer.{Atom => LexerAtom, LParen, RParen, Token}
import scala.util.parsing.combinator.Parsers

/**
 * A token-based S-expression parser.
 *
 * This parser extends the low-level Parsers trait and specifies
 * that the elementary input type (Elem) is our Token.
 */
class SExprParser extends Parsers {
  override type Elem = Token

  /**
   * Parser for an atom.
   * It accepts a token of type LexerAtom and returns an SAtom.
   */
  def atom: Parser[SAtom] = accept("atom", { case LexerAtom(value) => SAtom(value) })

  /**
   * Parser for a list of S-expressions.
   * It expects a LParen, then zero or more S-expressions, and then a RParen.
   */
  def list: Parser[SList] =
    accept("(", LParen) ~> rep(sexpr) <~ accept(")", RParen) ^^ { exprs => SList(exprs) }

  /**
   * A top-level S-expression parser.
   * An S-expression is either a list or an atom.
   */
  def sexpr: Parser[SExpr] = list | atom

  /**
   * Parser for a program: a non-empty sequence of S-expressions.
   */
  def program: Parser[List[SExpr]] = rep1(sexpr)
}

/**
 * Companion object providing convenience methods to parse S-expressions or programs.
 *
 * It first tokenizes the input string using SExprLexer.tokenize and then parses the resulting token stream.
 */
object SExprParser {

  /**
   * Parse a single S-expression from the input string.
   *
   * @param input the string to parse.
   * @return Either an error message or a successfully parsed SExpr.
   */
  def apply(input: String): Either[String, SExpr] = {
    val tokens = SExprLexer.tokenize(input)
    val parser = new SExprParser()
    parser.parseAll(parser.sexpr, tokens) match {
      case parser.Success(result, _) => Right(result)
      case parser.NoSuccess(msg, _)  => Left(s"Error parsing s-expression: $msg")
    }
  }

  /**
   * Parse a program (a sequence of S-expressions) from the input string.
   *
   * This method enforces that the input contains at least one newline (or carriage return)
   * to distinguish a multi-expression program from a single S-expression.
   *
   * @param input the string to parse.
   * @return Either an error message or a List of parsed SExprs.
   */
  def parseProgram(input: String): Either[String, List[SExpr]] = {
    // Check for at least one newline/carriage return.
    if (!input.exists(ch => ch == '\n' || ch == '\r'))
      Left("Error parsing program: input must contain at least one newline")
    else {
      val tokens = SExprLexer.tokenize(input)
      val parser = new SExprParser()
      parser.parseAll(parser.program, tokens) match {
        case parser.Success(result, _) => Right(result)
        case parser.NoSuccess(msg, _)  => Left(s"Error parsing program: $msg")
      }
    }
  }
}
