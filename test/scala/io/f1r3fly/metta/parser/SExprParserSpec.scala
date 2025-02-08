package io.f1r3fly.metta.parser

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SExprParserSpec extends AnyFunSuite with Matchers {

  test("parses a single atom") {
    val input = "foo"
    val expected = SAtom("foo")
    SExprParser(input) shouldEqual Right(expected)
  }

  test("parses a list of atoms") {
    val input = "(foo bar baz)"
    val expected = SList(List(SAtom("foo"), SAtom("bar"), SAtom("baz")))
    SExprParser(input) shouldEqual Right(expected)
  }

  test("parses a nested s-expression") {
    val input = "(foo (bar baz) 123)"
    val expected = SList(List(
      SAtom("foo"),
      SList(List(SAtom("bar"), SAtom("baz"))),
      SAtom("123")
    ))
    SExprParser(input) shouldEqual Right(expected)
  }

  test("returns error on unbalanced parentheses") {
    val input = "(foo (bar baz)"
    SExprParser(input) match {
      case Left(error) =>
        error should include ("Error parsing")
      case Right(ast) =>
        fail(s"Expected parsing to fail, but got AST: $ast")
    }
  }

  test("parses a valid program with newline") {
    // A valid program must contain at least one newline.
    // Here, we define a program as two S-expressions on separate lines.
    val input =
      """(foo bar)
        |(baz qux)""".stripMargin
    val expected = List(
      SList(List(SAtom("foo"), SAtom("bar"))),
      SList(List(SAtom("baz"), SAtom("qux")))
    )
    SExprParser.parseProgram(input) shouldEqual Right(expected)
  }

  test("fails to parse program if input has no newline") {
    // The input does not contain a newline (or carriage return),
    // so the parser should return an error.
    val input = "(foo bar) (baz qux)"
    SExprParser.parseProgram(input) match {
      case Left(error) =>
        error should include ("input must contain at least one newline")
      case Right(ast) =>
        fail(s"Expected parsing failure, but got AST: $ast")
    }
  }
}
