package io.f1r3fly.metta.parser

/**
 * A simple AST (Abstract Syntax Tree) representation for S-expressions.
 */
sealed trait SExpr

/**
 * Represents a single atom.
 * 
 * Example: "foo", "bar", "123"
 */
case class SAtom(value: String) extends SExpr

/**
 * Represents a list of S-expressions.
 * 
 * Example: (foo bar (baz 42))
 */
case class SList(exprs: List[SExpr]) extends SExpr
