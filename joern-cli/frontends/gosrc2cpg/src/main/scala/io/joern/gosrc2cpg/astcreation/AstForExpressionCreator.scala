package io.joern.gosrc2cpg.astcreation

import dotty.tools.dotc.ast.Trees.Ident
import io.joern.gosrc2cpg.Constant.PrimitiveTypes
import io.joern.gosrc2cpg.ast.Token
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.joern.gosrc2cpg.ast.nodes.{Identifier, MapType, SelectorExpression, *}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, NodeTypes, Operators, PropertyNames}
import io.joern.x2cpg.utils.NodeBuilders.{newFieldIdentifierNode, newIdentifierNode, newOperatorCallNode}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait AstForExpressionCreator(implicit validationMode: ValidationMode) {
    this: AstCreator =>

    val mapMethodAndSignature: mutable.Map[String, Seq[String]] = mutable.HashMap()

    def astForExpression(fileName: String, expression: Expression): Ast = {
        expression match {
            case indexExpression: IndexExpression => astForIndexExpression(
                fileName, indexExpression
            )
            case starExpression: StarExpression => astForStarExpression(
                fileName, starExpression
            )
            case typeAssertExpression: TypeAssertExpression => astForTypeAssertExpression(
                fileName, typeAssertExpression
            )
            case unaryExpression: UnaryExpression => astForUnaryExpression(
                fileName, unaryExpression
            )
            case binaryExpression: BinaryExpression => astForBinaryExpression(
                fileName, binaryExpression
            )
            case functionLiteral: FunctionLiteral => astForFunctionLiteral(
                fileName, functionLiteral
            )
            case callExpression: CallExpression => astForCallExpression(
                fileName, callExpression
            )
            case compositeLiteral: CompositeLiteral => astForCompositeLiteral(
                fileName, compositeLiteral
            )
            case basicLiteralExpression: BasicLiteralExpression => astForBasicLiteral(
                fileName, basicLiteralExpression
            )
            case identifier: Identifier => astForIdentifier(
                fileName, identifier
            )
            case parenthesizedExpression: ParenthesizedExpression =>
                parenthesizedExpression.expression match {
                    case Some(expr) => astForExpression(fileName, expr)
                    case None => Ast()
                }
            case selectorExpression: SelectorExpression => astForSelectorExpression(
                fileName, selectorExpression
            )
            case keyValueExpression: KeyValueExpression  => astForExpression(
                fileName, keyValueExpression.value.get
            )
            case arrayType: ArrayType => astForArrayType(fileName, arrayType)
//            case ellipsisExpression: EllipsisExpression
            case unknown =>
                logger.error(s"Unhandled expression node ${unknown.nodeType} on file ${fileName}")
                logger.error(unknown.code)
                Ast()
        }
    }

    private def astForArrayType(fileName: String, arrayType: ArrayType): Ast = {
//        val typeFullName = "[]" + arrayType.element match {
//            case Some(element) => element match {
//                case identifier: Identifier => identifier.typeFullName
//                case arrType: ArrayType => {
//                    val typeAst = astForArrayType(fileName, arrType)
//                    typeAst.root match {
//                        case Some(node) => node.properties.getOrElse(PropertyNames.TYPE_FULL_NAME, "<unknown>")
//                        case None => "<unknown>"
//                    }
//                }
//                case structType: StructType => structType.
//            }
//        }
        Ast(newIdentifierNode(arrayType.code, arrayType.code))
    }

    private def astForSelectorExpression(fileName: String, selectorExpression: SelectorExpression): Ast = {
        val typeFullName = selectorExpression.selector match {
            case Some(selector) => selector.typeFullName
            case None => ""
        }
        val (identifierAst, fieldTypeFullName) = selectorExpression.expression match {
            case Some(expression) => expression match {
                case identifier: Identifier => (astForExpression(fileName, identifier), typeFullName)
                case _ => (astForExpression(fileName, expression), typeFullName)
            }
            case None => (Ast(), "")
        }
        val fieldName = selectorExpression.selector match {
            case Some(selector) => selector.name match {
                case Some(name) => name
                case None => ""
            }
            case None => ""
        }
        val callNode = newOperatorCallNode(
            Operators.fieldAccess,
            selectorExpression.code,
            Some(fieldTypeFullName)
        )
        val fieldIdentifierNode = newFieldIdentifierNode(fieldName)

        callAst(callNode, Seq(identifierAst, Ast(fieldIdentifierNode)))
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

        val expression = typeAssertExpression.expression match {
            case Some(expr) => astForExpression(fileName, expr)
            case None => Ast()
        }
        val argumentType = typeAssertExpression.typeExpression match {
            case Some(expr) => astForExpression (fileName, expr)
            case None => Ast()
        }

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
            binaryExpression, binaryExpression.code, operator, operator, DispatchTypes.STATIC_DISPATCH,
            None, None
        )

        callAst(call, Seq(leftExpressionAst, rightExpressionAst))
    }

    private def astForFunctionLiteral(fileName: String, functionLiteral: FunctionLiteral): Ast = {
        val fullname = nextClosureName()

        val method = methodNode(
            functionLiteral, fullname, functionLiteral.code,
            fullname, Option(fullname), fileName
        )
        scope.pushNewScope(method)
        val (parameters, returnNode) = generateNodeFromFunctionType(
            fileName, functionLiteral.functionType.get
        )
        val bodyAst = astForStatement(fileName, functionLiteral.body.get)
        val methodAst_ = methodAst(
            method,
            parameters.map(parameter => Ast(parameter)),
            bodyAst.head,
            returnNode
        )
        scope.popScope()
        val funcDecl = typeDeclNode(functionLiteral,
            fullname, fullname, fileName, functionLiteral.code
        )
        Ast.storeInDiffGraph(Ast(funcDecl), diffGraph)
        // Setting Lambda TypeDecl as its parent.
        method.astParentType(NodeTypes.TYPE_DECL)
        method.astParentFullName(fullname)
        Ast.storeInDiffGraph(methodAst_, diffGraph)
        Ast(methodRefNode(functionLiteral, functionLiteral.code, fullname, fullname))
    }

    private def astForCallExpression(fileName: String, callExpression: CallExpression): Ast = {
        val (methodName, signature, fullName, typeFullName, receiverAst) =
            preReqForCallNode(fileName, callExpression.function.get, callExpression)
        mapMethodAndSignature.put(fullName, Seq(signature, typeFullName))
        val cpgCall = callNode(
            callExpression,
            callExpression.code,
            methodName,
            fullName,
            DispatchTypes.STATIC_DISPATCH,
            Some(signature),
            Some(typeFullName)
        )

        val args = callExpression.args.map(arg => {
            arg match {
                case mapType: MapType => Ast(literalNode(arg, arg.code, "map"))
                case chanelType: ChanelType => Ast(literalNode(arg, arg.code, "chanel"))
                case _ => astForExpression(fileName, arg)
            }
        })
        callAst(cpgCall, args.toSeq, receiverAst.headOption)
    }

    private def preReqForCallNode(fileName: String, function: Expression, callExpression: CallExpression): (String, String, String, String, Seq[Ast]) = {
        function match
            case identifier: Identifier =>
                var typeFullName = ""
                if (callExpression.results != null && callExpression.results.nonEmpty) {
                    typeFullName = callExpression.results.head
                }
                (
                    identifier.name.get, callExpression.functionSignature,
                    callExpression.functionFullName,
                    typeFullName,
                    Seq()
                )
            case selectorExpression: SelectorExpression =>
                val xNode = selectorExpression.expression.get
                xNode match {
                    case identifier: Identifier =>
                        var resultType = ""
                        if (selectorExpression.results != null && selectorExpression.results.nonEmpty) {
                            resultType = selectorExpression.results.head
                        }
                        var functionName = ""
                        if (selectorExpression.selector.isDefined && selectorExpression.selector.get.name.isDefined) {
                            functionName = selectorExpression.selector.get.name.get
                        }
                        (functionName, selectorExpression.functionSignature,
                            selectorExpression.functionFullName,
                            resultType,
                            Seq(astForExpression(fileName, identifier))
                        )
                    case _ =>
                        val receiverAst = astForExpression(fileName, xNode)
                        var typeFullName = ""
                        if (selectorExpression.results != null && selectorExpression.results.nonEmpty) {
                            typeFullName = selectorExpression.results.head
                        }
                        (
                            selectorExpression.selector.get.name.get,
                            selectorExpression.functionSignature,
                            selectorExpression.functionFullName,
                            typeFullName,
                            Seq(receiverAst)
                        )
//                        processReceiverAst(fileName, selectorExpression.selector.get.name.get, xNode)
                }
            case parenthesizedExpression: ParenthesizedExpression =>
                parenthesizedExpression.expression match {
                    case Some(expr) => expr match {
                        case identifier: Identifier =>
                            (
                                Operators.cast,
                                Operators.cast,
                                Operators.cast,
                                identifier.typeFullName,
                                Seq()
                            )
                        case _ => ("", "", "" , "", Seq())
                    }
                    case None => ("", "", "" , "", Seq())
                }
            case arrayType: ArrayType =>
                (
                    arrayType.code + "()",
                    arrayType.code + "()",
                    arrayType.code + "()",
                    arrayType.code,
                    Seq()
                )
            case x =>
                logger.warn(callExpression.code)
                logger.warn(s"Unhandled class ${x.getClass}")
                ("", "", "" , "", Seq())
    }


    private def processReceiverAst(fileName: String, methodName: String, expression: Expression)
    : (String, String, String, String, Seq[Ast]) = {
        val receiverAst = astForExpression(fileName, expression)
        val receiverTypeFullName = receiverAst.root.get.properties(PropertyNames.TYPE_FULL_NAME).toString
        val callMethodFullName: String = s"$receiverTypeFullName.$methodName"
        var returnTypeFullNameCache: String = ""
        var signatureCache: String = ""
        if (mapMethodAndSignature.contains(callMethodFullName)) {
            val seq = mapMethodAndSignature(callMethodFullName)
            returnTypeFullNameCache = seq.head
            signatureCache = seq.last
        }
        (methodName, signatureCache, callMethodFullName, returnTypeFullNameCache, Seq(receiverAst))
    }

    //    private def astForCallExpression(fileName: String, callExpression: CallExpression): Ast = {
//
//        val (methodName, methodFullName, signature) = callExpression.function.get match {
//            case identifier: Identifier =>
//                (identifier.name.get, callExpression.functionFullName, callExpression.functionSignature)
//            case selector: SelectorExpression => {
//                val selectorName = selector.selector match
//                    case Some(selector) => selector.name.get
//                    case None => ""
//                (selectorName, selector.functionFullName, selector.functionSignature)
//            }
//        }
//        val call = callNode(
//            callExpression,
//            callExpression.code,
//            methodName,
//            methodFullName,
//            DispatchTypes.STATIC_DISPATCH,
//            Option(signature),
//            None
//        )
//
//        val argumentAsts = callExpression.args.map(arg => astForExpression(fileName, arg)).toList
//        callAst(
//            call, argumentAsts
//        )
//    }

    private def astForCompositeLiteral(fileName: String, compositeLiteral: CompositeLiteral): Ast = {
        compositeLiteral.typeExpression match {
            case Some(typeExpression) => compositeLiteral.typeExpression.get match {
                case arrayType: ArrayType => astForArrayInitialization(fileName, compositeLiteral)
                case identifier: Identifier => astForConstructor(fileName, compositeLiteral)
                case selectorExpression: SelectorExpression => astForConstructor(fileName, compositeLiteral)
                case expression => {
                    logger.warn(s"Unhandled type of composite literal ${expression.nodeType}")
                    Ast()
                }
            }
            case None => Ast()
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
            case selectorExpression: SelectorExpression => selectorExpression.selector.get.typeFullName
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
            DispatchTypes.STATIC_DISPATCH,
            Option(callName + "." + Defines.ConstructorMethodName),
            Option(callName)
        )
        val arguments = compositeLiteral.elements.map(elt => astForExpression(fileName, elt)).toList
        callAst(
            call, arguments
        )
    }

    private def astForBasicLiteral(fileName: String, basicLiteral: BasicLiteralExpression): Ast = {
        val typeName = basicLiteral.kind match {
            case Token.Int =>
                usedPrimitiveTypes.add("int")
                "int"
            case Token.Float =>
                usedPrimitiveTypes.add("float32")
                "float32"
            case Token.Imag =>
                usedPrimitiveTypes.add("imag")
                "imag"
            case Token.Char =>
                usedPrimitiveTypes.add("char")
                "char"
            case Token.String =>
                usedPrimitiveTypes.add("string")
                "string"
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
        identifier.name match {
            case Some(name) =>
                if (identifier.name.get.equals("true") || identifier.name.get.equals("false")) {
                    val literal = literalNode(
                        identifier,
                        identifier.code,
                        PrimitiveTypes.BOOLEAN
                    )
                    Ast(literal)
                }
                else if (!name.equals("_")) {
                    scope.lookupVariable(name) match {
                        case Some((localNode, typeFullname)) =>
                            val identNode = identifierNode(
                                identifier, name, identifier.code,
                                typeFullname
                            )
                            Ast(identNode).withRefEdge(identNode, localNode)
                        case None =>
                            if (identifier.typeFullName != "") {
                                Ast(newIdentifierNode(
                                    identifier.name.getOrElse(""),
                                    identifier.typeFullName
                                ))
                            } else {
                                Ast()
                            }
                    }
                } else {
                    Ast()
                }
            case None => Ast()
        }

        //        if (identifier.name.get.equals("true") || identifier.name.get.equals("false")) {
        //            val literal = literalNode(
        //                identifier,
        //                identifier.code,
        //                "bool"
        //            )
        //            Ast(literal)
        //        } else {
        //            Ast()
        //        }
    }

}
