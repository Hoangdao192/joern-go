package io.joern.gosrccpg.ast

object Token extends Enumeration {

  val Break = 61
  val Continue = 65
  val Goto = 73

  // Operators and delimiters
  val Addition = 12 // +
  val Subtraction = 13 // -
  val Multiplication = 14 // *
  val Division = 15 // /
  val Modulo = 16 // %
  val And = 17
  val Or = 18
  val Xor = 19
  val ShiftLeft = 20 // <<
  val ShiftRight = 21 // >>

  val LessThan = 40
  val GreaterThan = 41
  val LessThanEqual = 45
  val GreaterThanEqual = 46

  val LogicalAnd = 34
  val LogicalOr = 35
  val Equals = 39
  val NotEquals = 44
  
  val Not = 43
//  AND // &
//  OR // |
//  XOR // ^
//  AND_NOT // &^
//
//  ADD_ASSIGN // +=
//  SUB_ASSIGN // -=
//  MUL_ASSIGN // *=
//  QUO_ASSIGN // /=
//  REM_ASSIGN // %=
//
//  AND_ASSIGN // &=
//  OR_ASSIGN // |=
//  XOR_ASSIGN // ^=
//  SHL_ASSIGN // <<=
//  SHR_ASSIGN // >>=
//  AND_NOT_ASSIGN // &^=
//
//  LAND // &&
//  LOR // ||
//  ARROW // <-
//  INC // ++
//  DEC // --
//
//  EQL // ==
//  ASSIGN // =
//  NOT // !
//
//  NEQ // !=
//  DEFINE // :=
//  ELLIPSIS // ...
//
//  LPAREN // (
//  LBRACK // [
//  LBRACE // {
//  COMMA // ,
//  PERIOD // .
//
//  RPAREN // )
//  RBRACK // ]
//  RBRACE // }
//  SEMICOLON // ;
//  COLON // :
//  operator_end
//
//  keyword_beg
//  // Keywords
//  BREAK
//  CASE
//  CHAN
//  CONST
//  CONTINUE
//
//  DEFAULT
//  DEFER
//  ELSE
//  FALLTHROUGH
//  FOR
//
//  FUNC
//  GO
//  GOTO
//  IF
//  IMPORT
//
//  INTERFACE
//  MAP
//  PACKAGE
//  RANGE
//  RETURN
//
//  SELECT
//  STRUCT
//  SWITCH
//  TYPE
//  VAR
//  keyword_end
//
//  additional_beg
//  // additional tokens, handled in an ad-hoc manner
//  TILDE
//  additional_end

}
