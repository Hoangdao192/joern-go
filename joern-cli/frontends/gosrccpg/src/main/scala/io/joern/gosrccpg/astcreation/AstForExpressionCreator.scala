package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.Token
import io.joern.x2cpg.ValidationMode
import io.joern.gosrccpg.ast.nodes.{BinaryExpression, Expression, IndexExpression, StarExpression, TypeAssertExpression, UnaryExpression}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

trait AstForExpressionCreator(implicit validationMode: ValidationMode) { this: AstCreator =>
  
  def astForExpression(fileName: String, expression: Expression): Ast = {
    Ast()
  }

  //  Example: array[0]
  private def astForIndexExpression(fileName: String, indexExpression: IndexExpression): Ast = {
    val call = callNode(
      indexExpression, indexExpression.code, Operators.indirectIndexAccess,
      Operators.indirectIndexAccess, DispatchTypes.STATIC_DISPATCH
    )

    val expression = astForExpression(fileName, indexExpression.expression.get)
    val index = astForExpression(fileName, indexExpression.index.get)
    callAst(call, Seq(expression, index))
  }

  private def astForStarExpression(fileName: String, starExpression: StarExpression): Ast = {
    val operand = astForExpression(fileName, starExpression.expression.get)
    val call = callNode(
      starExpression, starExpression.code, Operators.indirection, Operators.indirection,
      DispatchTypes.STATIC_DISPATCH
    )
    callAst(call, Seq(operand))
  }
  
  private def astForTypeAssertExpression(fileName: String, typeAssertExpression: TypeAssertExpression): Ast = {
    val call = callNode(
      typeAssertExpression, typeAssertExpression.code, Operators.cast, Operators.cast,
      DispatchTypes.STATIC_DISPATCH
    )
    
    val expression = astForExpression(fileName, typeAssertExpression.expression.get)
    val argumentType = astForExpression(fileName, typeAssertExpression.typeExpression.get)
    
    callAst(call, Seq(expression, argumentType))
  }

  //  Unary expression is an expression that have only one operand
  private def astForUnaryExpression(fileName: String, unaryExpression: UnaryExpression): Ast = {
    val operand = astForExpression(fileName, unaryExpression.expression.get)
    val operator = unaryExpression.operator match {
      case Token.Addition => Operators.plus
      //  a = -b
      case Token.Subtraction => Operators.subtraction
      //  a = +b
      case Token.Multiplication => Operators.minus
      //  a = &b
      case Token.And => Operators.addressOf
      //  a = !b
      case Token.Not => Operators.logicalNot
      case _ => "<operator>.unknown"
    }

    val call = callNode(
      unaryExpression, unaryExpression.code, operator, operator,
      DispatchTypes.STATIC_DISPATCH
    )
    
//    operand.t
    callAst(call, Seq(operand))
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
      case _ => "<operator>.unknown"
    }

    val call = callNode(
      binaryExpression, binaryExpression.toString, operator, operator, DispatchTypes.STATIC_DISPATCH, None, None
    )

    callAst(call, Seq(leftExpressionAst, rightExpressionAst))
  }


}
