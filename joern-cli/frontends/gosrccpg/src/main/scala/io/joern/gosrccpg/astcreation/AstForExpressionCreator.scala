package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.Token
import io.joern.x2cpg.ValidationMode
import io.joern.gosrccpg.ast.nodes.{BinaryExpression, Expression}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

trait AstForExpressionCreator(implicit validationMode: ValidationMode) { this: AstCreator =>
  
  def astForExpression(fileName: String, expression: Expression): Ast = {
    Ast()
  }

  private def astForBinaryExpression(fileName: String, binaryExpression: BinaryExpression): Ast = {
    val leftExpressionAst = astForExpression(fileName, binaryExpression.leftExpression.get)
    val rightExpressionAst = astForExpression(fileName, binaryExpression.rightExpression.get)
    val operator = binaryExpression.operator match {
      case Token.Multiplication => Operators.multiplication
      case Token.Division => Operators.division
      case Token.Modulo => Operators.modulo
      case Token.Addition => Operators.addition
      case Token.Subtraction => Operators.subtraction
      case Token.ShiftLeft => Operators.shiftLeft
      case Token.ShiftRight => Operators.arithmeticShiftRight
      case Token.LessThan => Operators.lessThan
      case Token.GreaterThan => Operators.greaterThan
      case Token.LessThanEqual => Operators.lessEqualsThan
      case Token.GreaterThanEqual => Operators.greaterEqualsThan
      case Token.And => Operators.and
      case Token.Or => Operators.or
      case Token.Xor => Operators.xor
      case Token.LogicalOr => Operators.logicalOr
      case Token.LogicalAnd => Operators.logicalAnd
      case Token.Equals => Operators.equals
      case Token.NotEquals => Operators.notEquals
      case _ => "unknown"
    }

    val call = callNode(
      binaryExpression, binaryExpression.toString, operator, operator, DispatchTypes.STATIC_DISPATCH, None, None
    )

    callAst(call, Seq(leftExpressionAst, rightExpressionAst))
  }


}
