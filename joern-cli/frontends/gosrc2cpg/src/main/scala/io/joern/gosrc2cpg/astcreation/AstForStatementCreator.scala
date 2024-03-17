package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.ast.Token
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.joern.gosrc2cpg.ast.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewControlStructure, NewReturn}

import scala.collection.mutable.ListBuffer

trait AstForStatementCreator(implicit withSchemaValidation: ValidationMode) {
    this: AstCreator =>

    def astForStatement(fileName: String, statement: Statement): Seq[Ast] = {
        statement match {
            case assignStatement: AssignStatement => astForAssignStatement(fileName, assignStatement)
            case declarationStatement: DeclarationStatement => astForDeclarationStatement(fileName, declarationStatement)
            case ifStatement: IfStatement => Seq(astForIfStatement(fileName, ifStatement))
            case switchStatement: SwitchStatement => Seq(astForSwitchStatement(fileName, switchStatement))
            case typeSwitchStatement: TypeSwitchStatement => Seq(astForTypeSwitchStatement(fileName, typeSwitchStatement))
            case forStatement: ForStatement => Seq(astForForStatement(fileName, forStatement))
            case rangeStatement: RangeStatement => Seq(astForRangeStatement(fileName, rangeStatement))
            case blockStatement: BlockStatement => Seq(astForBlockStatement(fileName, blockStatement))
            case branchStatement: BranchStatement => Seq(astForBranchStatement(fileName, branchStatement))
            case returnStatement: ReturnStatement => Seq(astForReturnStatement(fileName, returnStatement))
            case expressionStatement: ExpressionStatement => expressionStatement.expression match {
                case Some(expression) => Seq(astForExpression(fileName, expression))
                case None => Seq()
            }
            case unknown =>
                logger.error(s"Unhandled expression node ${unknown.nodeType}")
                Seq()
        }
    }

    private def astForIfStatement(fileName: String, ifStatement: IfStatement): Ast = {
        val conditionAst = astForExpression(fileName, ifStatement.condition.get)
        val ifNode = controlStructureNode(
            ifStatement, ControlStructureTypes.IF, ifStatement.code
        )

        val bodyAst = ifStatement.body match {
            case Some(body) => astForBlockStatement(fileName, ifStatement.body.get)
            case None => Ast()
        }
        val elseAst = ifStatement.elseStatement match {
            case Some(elseStatement) => elseStatement match {
                case blockStatement: BlockStatement => {
                    val elseNode = controlStructureNode(elseStatement, ControlStructureTypes.ELSE, "else")
                    val elseAst = astForStatement(fileName, blockStatement)
                    Ast(elseNode).withChildren(elseAst)
                }
                case childIfStatement: IfStatement => {
                    val elseNode = controlStructureNode(childIfStatement, ControlStructureTypes.ELSE, "else")
                    val elseBlock = blockNode(childIfStatement, childIfStatement.code, "unit")
                    scope.pushNewScope(elseBlock)
                    val a = astForStatement(fileName, childIfStatement)
                    setArgumentIndices(a)
                    scope.popScope()
                    Ast(elseNode).withChild(blockAst(elseBlock, a.toList))
                }
                case unknown =>
                    logger.warn(s"[astForIfStatement] Unhandled else statement ${unknown.nodeType}")
                    Ast()
            }
            case None => Ast()
        }

        Ast(ifNode)
            .withChildren(Seq(conditionAst, bodyAst, elseAst))
            .withConditionEdge(ifNode, conditionAst.root.get)
    }

    private def astForSwitchStatement(fileName: String, switchStatement: SwitchStatement): Ast = {
        val conditionAst = astForExpression(fileName: String, switchStatement.tag.get)

        val switchNode = NewControlStructure()
            .controlStructureType(ControlStructureTypes.SWITCH)
            .code(conditionAst.toString)

        val switchBodyAst = astForStatement(fileName, switchStatement.body.get)
        Ast(switchNode)
            .withChildren(Seq(conditionAst, switchBodyAst.head))
            .withConditionEdge(switchNode, conditionAst.root.get)
    }

    private def astForTypeSwitchStatement(fileName: String, typeSwitchStatement: TypeSwitchStatement): Ast = {
        val assignAst = astForStatement(fileName, typeSwitchStatement.assign.get)

        val switchNode = NewControlStructure()
            .controlStructureType(ControlStructureTypes.SWITCH)
            .code(assignAst.toString)

        val switchBodyAst = astForStatement(fileName, typeSwitchStatement.body.get)

        Ast(switchNode)
            .withChildren(Seq(switchBodyAst.head))
            .withConditionEdge(switchNode, assignAst.head.root.get)
    }

    private def astForForStatement(fileName: String, forStatement: ForStatement): Ast = {
        scope.pushNewScope(blockNode(forStatement, "", ""))
        val initAst = astForStatement(fileName, forStatement.initialization.get)
        scope.popScope()
        val conditionAst = astForExpression(fileName, forStatement.condition.get)
        val updateAst = astForStatement(fileName, forStatement.post.get)
        val bodyAst = astForStatement(fileName, forStatement.body.get)

        //  TODO: Handle for loop code
        val forNode = NewControlStructure()
            .controlStructureType(ControlStructureTypes.FOR)
            .code(forStatement.code)
        forAst(forNode, Seq(), initAst, Seq(conditionAst), updateAst, bodyAst.head)
    }

    private def astForRangeStatement(fileName: String, rangeStatement: RangeStatement): Ast = {
        Ast()
    }

    private def astForBlockStatement(fileName: String, blockStatement: BlockStatement): Ast = {
        val blockNode = NewBlock()
            .code(blockStatement.code)
            .typeFullName("unit")
            .lineNumber(line(blockStatement))
            .columnNumber(column(blockStatement))
        val childrenAst = ListBuffer[Ast]()
        blockStatement.statements.foreach(statement => childrenAst.addAll(astForStatement(fileName, statement)))
        blockAst(blockNode, childrenAst.toList)
    }

    private def astForBranchStatement(fileName: String, branchStatement: BranchStatement): Ast = {
        val controlStructureType = branchStatement.token match {
            case Token.Break => ControlStructureTypes.BREAK
            case Token.Continue => ControlStructureTypes.CONTINUE
            case Token.Goto => ControlStructureTypes.GOTO
            case _ => null
        }

        //  Handle code
        val controlNode = NewControlStructure()
            .controlStructureType(controlStructureType)
            .code(branchStatement.toString)

        Ast(controlNode)
    }

    private def astForDeclarationStatement(fileName: String, declarationStatement: DeclarationStatement): Seq[Ast] = {
        declarationStatement.declaration match {
            case Some(declaration) => astForDeclaration(fileName, "", declaration)
            case None => Seq()
        }
    }

    private def astForReturnStatement(fileName: String, returnStatement: ReturnStatement): Ast = {
        val returnNode = NewReturn()
            .code(returnStatement.code)
        val arguments = returnStatement.results.map(e => astForExpression(fileName, e))
        returnAst(returnNode, arguments.toSeq)
    }

    private def astForAssignStatement(fileName: String, assignmentStatement: AssignStatement): Seq[Ast] = {
        val (isDefineAssign, operator) = assignmentStatement.token match {
            case Token.Assign => (false, Operators.assignment)
            case Token.Define => (true, Operators.assignment)
            case Token.AddAssign => (false, Operators.assignmentPlus)
            case Token.SubAssign => (false, Operators.assignmentMinus)
            case Token.MulAssign => (false, Operators.assignmentMultiplication)
            case Token.QuoAssign => (false, Operators.assignmentDivision)
            case Token.RemAssign => (false, Operators.assignmentModulo)
            case Token.AndAssign => (false, Operators.assignmentAnd)
            case Token.OrAssign => (false, Operators.assignmentOr)
            case Token.XorAssign => (false, Operators.assignmentXor)
            case Token.ShlAssign => (false, Operators.assignmentShiftLeft)
            case Token.ShrAssign => (false, Operators.assignmentArithmeticShiftRight)
            case _ => (false, Defines.Unknown)
        }
        var asts = ListBuffer[Ast]()
        if (isDefineAssign) {
            val leftExpressions = assignmentStatement.lhs
            val values = assignmentStatement.rhs

            var index: Int = 0
            for (expr <- leftExpressions) {
                expr match {
                    case identifier: Identifier =>
                        if (values.length > index) {
                            var value = values(index)
                            if (value.isInstanceOf[FunctionLiteral]) {
                                asts.addOne(
                                    astForExpression(fileName, value.asInstanceOf[FunctionLiteral])
                                )
                            } else {
                                val typeFullName = getTypeFullNameFromExpression(value)
                                val local = localNode(
                                    identifier, identifier.name.get,
                                    identifier.code,
                                    typeFullName
                                )
                                scope.addToScope(identifier.name.get, (local, typeFullName))
                                asts.addOne(Ast(local))

                                //  Treat assignment statement as a call node
                                val call = callNode(
                                    value,
                                    value.code,
                                    Operators.assignment,
                                    Operators.assignment,
                                    DispatchTypes.STATIC_DISPATCH
                                )
                                val leftAst = astForExpression(fileName, identifier)
                                val rightAst = astForExpression(fileName, value)
                                asts.addOne(callAst(
                                    call, Seq(leftAst, rightAst)
                                ))
                            }
                            index += 1
                        }
                    case _ => {}
                }
            }
        }
        else {
            val leftExprAst = assignmentStatement.lhs.map(expr => astForExpression(
                fileName, expr
            ))
            val rightExprAst = assignmentStatement.rhs.map(expr => astForExpression(
                fileName, expr
            ))
            val typeFullName = getTypeFullNameFromExpression(assignmentStatement.rhs.head)
            val call = callNode(
                assignmentStatement, assignmentStatement.code,
                operator, operator, DispatchTypes.STATIC_DISPATCH,
                None, Option(typeFullName)
            )
            asts.addOne(
                callAst(call, leftExprAst.toSeq ++: rightExprAst.toSeq)
            )
        }
        asts.toSeq
    }

}
