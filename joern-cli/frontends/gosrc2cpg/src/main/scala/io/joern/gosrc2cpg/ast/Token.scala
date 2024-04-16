package io.joern.gosrc2cpg.ast

object Token extends Enumeration {

  val Break = 61
  val Continue = 65
  val Goto = 73

  //  Basic literal
  val Int = 5 // 12345
  val Float = 6 // 123.45
  val Imag = 7 // 123.45i
  val Char = 8 // 'a'
  val String = 9 // "abc"

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
  
  //  Generic declaration
  val Import = 75
  val Constant = 64
  val Type = 84
  val Var = 85

  //  Assignment token
  val Assign    = 42 // =
  val Define    = 47 // :=
  val AddAssign = 23 // "+="
  val SubAssign = 24 // -=
  val MulAssign = 25 // *=
  val QuoAssign = 26 // /=
  val RemAssign = 27 // "%="
  val AndAssign = 28 // &=
  val OrAssign  = 29 // |=
  val XorAssign = 30 // ^=
  val ShlAssign = 31 // <<=
  val ShrAssign = 32 // >>=

  val Inc = 37
  val Dec = 38
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
