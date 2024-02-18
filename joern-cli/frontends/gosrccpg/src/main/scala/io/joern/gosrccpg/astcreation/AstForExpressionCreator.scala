package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.Token
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.joern.gosrccpg.ast.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

import scala.collection.mutable.ListBuffer

trait AstForExpressionCreator(implicit validationMode: ValidationMode) {
    this: AstCreator =>

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

    private def astForFunctionLiteral(fileName: String, functionLiteral: FunctionLiteral): Ast = {
        val method = methodNode(
            functionLiteral, "", functionLiteral.code,
            "", None, fileName
        )
        val (parameters, returnNode) = generateNodeFromFunctionType(
            fileName, functionLiteral.functionType.get
        )
        val bodyAst = astForStatement(fileName, functionLiteral.body.get)
        methodAst(
            method,
            parameters.map(parameter => Ast(parameter)),
            bodyAst,
            returnNode
        )
    }

    private def astForCallExpression(fileName: String, callExpression: CallExpression): Ast = {
        val (methodName, methodFullName) = callExpression.function.get match {
            case identifier: Identifier => (identifier.name.get, identifier.name.get)
            case selector: SelectorExpression => {
                val selectorName = selector.selector.get.name.get
                val expressionName = selector.expression.get match {
                    case identifier: Identifier => identifier.name.get
                    case expression => {
                        logger.warn(s"Unhandled selector expression ${expression.getClass.toString}")
                        ""
                    }
                }
                if (!expressionName.equals("")) {
                    (selectorName, s"$expressionName.$selectorName")
                } else {
                    (selectorName, selectorName)
                }
            }
        }
        val call = callNode(
            callExpression,
            callExpression.code,
            methodName, methodFullName, DispatchTypes.STATIC_DISPATCH
        )

        val argumentAsts = callExpression.args.map(arg => astForExpression(fileName, arg)).toList
        callAst(
            call, argumentAsts
        )
    }

    private def astForCompositeLiteral(fileName: String, compositeLiteral: CompositeLiteral): Ast = {
        compositeLiteral.typeExpression.get match {
            case arrayType: ArrayType => astForArrayInitialization(fileName, compositeLiteral)
            case identifier: Identifier => astForConstructor(fileName, compositeLiteral)
            case expression => {
                logger.warn(s"Unhandled type of composite literal ${expression.nodeType}")
                Ast()
            }
        }
    }

    private def astForArrayInitialization(fileName: String, compositeLiteral: CompositeLiteral): Ast = {
        val call = callNode(
            compositeLiteral,
            compositeLiteral.code,
            Operators.arrayInitializer,
            Operators.arrayInitializer,
            DispatchTypes.STATIC_DISPATCH
        )

        val elements = compositeLiteral.elements.map(element => astForExpression(fileName, element)).toList
        callAst(call, elements)
    }

    private def astForConstructor(fileName: String, compositeLiteral: CompositeLiteral): Ast = {
        val callName = compositeLiteral.typeExpression.get match {
            case identifier: Identifier => identifier.name.get
            case x => {
                logger.warn(s"Unhandled composite literal type ${x.getClass.toString}")
                Defines.Unknown
            }
        }

        val call = callNode(
            compositeLiteral,
            compositeLiteral.code,
            callName,
            callName + "." + Defines.ConstructorMethodName,
            DispatchTypes.STATIC_DISPATCH
        )
        val arguments = compositeLiteral.elements.map(elt => astForExpression(fileName, elt)).toList
        callAst(
            call, arguments
        )
    }

    private def astForBasicLiteral(fileName: String, basicLiteral: BasicLiteralExpression): Ast = {
        val typeName = basicLiteral.kind match {
            case Token.Int => "int"
            case Token.Float => "float32"
            case Token.Imag => "imag"
            case Token.Char => "char"
            case Token.String => "string"
            case _ => "any"
        }
        val literal = literalNode(
            basicLiteral,
            basicLiteral.code,
            typeName
        )
        Ast(literal)
    }

    private def astForIdentifier(fileName: String, identifier: Identifier): Ast = {
        if (identifier.name.get.equals("true") || identifier.name.get.equals("false")) {
            val literal = literalNode(
                identifier,
                identifier.code,
                "bool"
            )
            Ast(literal)
        } else {
            Ast()
        }
    }
}
