package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.Constant
import io.joern.gosrc2cpg.ast.Token
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.joern.gosrc2cpg.ast.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewControlStructure, NewIdentifier, NewJumpTarget, NewLocal, NewReturn}

import scala.collection.mutable.ListBuffer

trait AstForStatementCreator(implicit withSchemaValidation: ValidationMode) {
    this: AstCreator =>

    def astForStatement(fileName: String, statement: Statement): Seq[Ast] = {
        statement match {
            case deferStatement: DeferStatement =>
                //  TODO: handle defer statement for careful
                deferStatement.call match {
                case Some(call) => Seq(astForExpression(fileName, call))
                case None => Seq.empty
            } 
            case assignStatement: AssignStatement => astForAssignStatement(fileName, assignStatement)
            case declarationStatement: DeclarationStatement => astForDeclarationStatement(fileName, declarationStatement)
            case ifStatement: IfStatement => Seq(astForIfStatement(fileName, ifStatement))
            case switchStatement: SwitchStatement => Seq(astForSwitchStatement(fileName, switchStatement))
            case caseClause: CaseClause => astForCaseClause(fileName, caseClause)
            case typeSwitchStatement: TypeSwitchStatement => Seq(astForTypeSwitchStatement(fileName, typeSwitchStatement))
            case forStatement: ForStatement => Seq(astForForStatement(fileName, forStatement))
            case rangeStatement: RangeStatement => Seq(astForRangeStatement(fileName, rangeStatement))
            case blockStatement: BlockStatement => Seq(astForBlockStatement(fileName, blockStatement))
            case branchStatement: BranchStatement => Seq(astForBranchStatement(fileName, branchStatement))
            case returnStatement: ReturnStatement => Seq(astForReturnStatement(fileName, returnStatement))
            case labeledStatement: LabeledStatement => astForLabeledStatement(fileName, labeledStatement)
            case expressionStatement: ExpressionStatement => expressionStatement.expression match {
                case Some(expression) => Seq(astForExpression(fileName, expression))
                case None => Seq()
            }
            case incDecStatement: IncrementDecrementStatement => Seq(astForIncDecStatement(fileName, incDecStatement))
            case unknown =>
                logger.error(s"Unhandled statement node ${unknown.nodeType}")
                Seq()
        }
    }

    private def astForLabeledStatement(fileName: String, labeledStatement: LabeledStatement): Seq[Ast] = {
        val cpgLabel =  NewJumpTarget()
            .parserTypeName(labeledStatement.nodeType)
            .name(labeledStatement.label match {
                case Some(identifier) => identifier.name.getOrElse("")
                case None => 
                    logger.warn("Label statement does not have label name")
                    ""
            })
            .code(labeledStatement.code)
        val nestedStmts = labeledStatement.statement match {
            case Some(statement) => astForStatement(fileName, statement)
            case None => Seq.empty
        }
        Ast(cpgLabel) +: nestedStmts
    }

    private def astForCaseClause(fileName: String, caseClause: CaseClause): Seq[Ast] = {
        val conditionAsts = ListBuffer[Ast]()

        if (caseClause.list.nonEmpty) {
            caseClause.list.foreach(expr => {
                val node = jumpTargetNode(
                    caseClause, "case", expr.code
                )
                val jumpAst = Ast(node)
                val exprAst = astForExpression(fileName, expr)
                conditionAsts.addOne(jumpAst)
                conditionAsts.addOne(exprAst)
            })
        } else {
            val target = jumpTargetNode(caseClause, "default", "default")
            conditionAsts.addOne(Ast(target))
        }

        val bodyAsts = ListBuffer[Ast]()
        caseClause.body.foreach(statement => bodyAsts.addAll(astForStatement(fileName, statement)))

        conditionAsts.toList ++: bodyAsts.toList
    }

    private def astForIfStatement(fileName: String, ifStatement: IfStatement): Ast = {
        val initAst = ifStatement.initialization match {
            case Some(statement: Statement) => astForStatement(fileName, statement)
            case None => Seq[Ast]()
        }

        initAst.foreach(ast => {
            ast.nodes.foreach(node => scope.pushNewScope(node))
        })

        val conditionAst = ifStatement.condition match {
            case Some(condition) => astForExpression(fileName, condition)
            case None => Ast()
        }
        val ifNode = controlStructureNode(
            ifStatement, ControlStructureTypes.IF, ifStatement.code
        )

        val bodyAst = ifStatement.body match {
            case Some(body) => astForBlockStatement(fileName, body)
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

        if (initAst != null) {
            initAst.foreach(ast => {
                ast.nodes.foreach(node => scope.popScope())
            })
        }

        controlStructureAst(
            ifNode, Some(conditionAst), Seq(bodyAst, elseAst) ++ initAst
        )
    }

    private def astForSwitchStatement(fileName: String, switchStatement: SwitchStatement): Ast = {
        val conditionAst = switchStatement.tag match {
            case Some(tag) => astForExpression(fileName: String, tag)
            case None => Ast()
        }

        val switchNode = NewControlStructure()
            .controlStructureType(ControlStructureTypes.SWITCH)
            .code(conditionAst.toString)

        val switchBodyAst = switchStatement.body match {
            case Some(blockStatement) => astForStatement(fileName, blockStatement)
            case None => Seq.empty
        }
        controlStructureAst(
            switchNode, Option(conditionAst), switchBodyAst
        )
    }

    private def astForTypeSwitchStatement(fileName: String, typeSwitchStatement: TypeSwitchStatement): Ast = {
        val assignAsts = typeSwitchStatement.assign match {
            case Some(assignStatement) => astForStatement(fileName, assignStatement)
            case None => Seq.empty
        }

        val switchNode = NewControlStructure()
            .parserTypeName("SwitchStatement")
            .controlStructureType(ControlStructureTypes.SWITCH)
            .code(typeSwitchStatement.code)

        val switchBodyAsts = astForStatement(fileName, typeSwitchStatement.body.get)

        val identifierAsts = List[Ast]()
        assignAsts.foreach(assignAst => {
            val ident = assignAst.root match {
                case Some(node) => node match {
                    case identifier: NewIdentifier => Ast(identifierNode(
                        typeSwitchStatement.assign.get, identifier.name,
                        identifier.code, identifier.typeFullName
                    ))
                    case local: NewLocal =>
                        val identNode = identifierNode(
                            typeSwitchStatement.assign.get, local.name,
                            local.code, local.typeFullName
                        )
                        val ast = Ast(identNode).withRefEdge(identNode, local)
                        ast
                    case _ => Ast()
                }
                case None => Ast()
            }
        })

        val call = callNode(typeSwitchStatement, typeSwitchStatement.code,
            Operators.is, Operators.is, DispatchTypes.STATIC_DISPATCH
        )
        val conditionAst = callAst(call, identifierAsts)

        controlStructureAst(
            switchNode, Option(conditionAst), switchBodyAsts
        )
    }

    private def astForForStatement(fileName: String, forStatement: ForStatement): Ast = {
        scope.pushNewScope(blockNode(forStatement, "", ""))
        val initAst = forStatement.initialization match {
            case Some(statement) => astForStatement(fileName, forStatement.initialization.get)
            case None => Seq.empty
        }
        scope.popScope()
        val conditionAst = forStatement.condition match {
            case Some(condition) => astForExpression(fileName, condition)
            case None => Ast()
        }
        val updateAsts = forStatement.post match {
            case Some(statement) => astForStatement(fileName, statement)
            case None => Seq.empty
        }
        val bodyAsts = forStatement.body match {
            case Some(blockStatement) => astForStatement(fileName, forStatement.body.get)
            case None => Seq.empty
        }

        //  TODO: Handle for loop code
        val forNode = NewControlStructure()
            .controlStructureType(ControlStructureTypes.FOR)
            .code(forStatement.code)
        forAst(forNode, Seq(), initAst, Seq(conditionAst), updateAsts, bodyAsts)
    }

    private def astForRangeStatement(fileName: String, rangeStatement: RangeStatement): Ast = {
        if (rangeStatement.key.isDefined && rangeStatement.value.isDefined) {
            val keyName = rangeStatement.key match {
                case Some(key) => key match {
                    case identifier: Identifier => identifier.name.getOrElse("")
                    case other =>
                        logger.warn(s"Unhandled expression when parse range statement ${other.getClass.getTypeName}")
                        ""
                }
                case None => ""
            }
            val valueName = rangeStatement.value match {
                case Some(value) => value match {
                    case identifier: Identifier => identifier.name.getOrElse("")
                    case other =>
                        logger.warn(s"Unhandled expression when parse range statement ${other.getClass.getTypeName}")
                        ""
                }
                case None => ""
            }
            if (keyName.nonEmpty && valueName.nonEmpty) {
                val pattern = s"for${keyName},${valueName}:="
                val forNode = controlStructureNode(rangeStatement, ControlStructureTypes.FOR, rangeStatement.code)
                scope.pushNewScope(forNode)
                val declAst = ListBuffer[Ast]()
                if (rangeStatement.code.replace(" ", "").startsWith(pattern)) {
                    if (!keyName.equals("_") && rangeStatement.key.get.isInstanceOf[Identifier]) {
                        val identifier = rangeStatement.key.get.asInstanceOf[Identifier]
                        val local = newLocalNode(identifier)
                        scope.addToScope(keyName, (local, identifier.typeFullName))
                        declAst.addOne(Ast(local))
                    }
                    if (!valueName.equals("_") && rangeStatement.value.get.isInstanceOf[Identifier]) {
                        val identifier = rangeStatement.value.get.asInstanceOf[Identifier]
                        val local = newLocalNode(identifier)
                        scope.addToScope(valueName, (local, identifier.typeFullName))
                        declAst.addOne(Ast(local))
                    }
                }
                val keyIdentAst = keyName.equals("_") match {
                    case false => astForExpression(fileName, rangeStatement.key.get)
                    case true => Ast()
                }
                val valueIdentAst = valueName.equals("_") match {
                    case false => astForExpression(fileName, rangeStatement.value.get)
                    case true => Ast()
                }
                val initAst = rangeStatement.expression match {
                    case Some(expression) => astForExpression(fileName, expression)
                    case None => Ast()
                }
                val bodyAsts = rangeStatement.body match {
                    case Some(body) => astForStatement (fileName, body)
                    case None => Seq.empty
                }
                scope.popScope()
                return controlStructureAst(forNode, None, initAst +: bodyAsts)
            }
        } else {
            val initAst = rangeStatement.expression match {
                case Some(expression) => astForExpression(fileName, expression)
                case None => Ast()
            }
            val forNode = controlStructureNode(rangeStatement, ControlStructureTypes.FOR, rangeStatement.code)
            val bodyAsts = rangeStatement.body match {
                case Some(body) => astForStatement (fileName, body)
                case None => Seq.empty
            }
            return controlStructureAst(forNode, None, initAst +: bodyAsts)
        }
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
            case _ =>
                logger.warn("Unhandled branch statement token")
                null
        }

        Ast(controlStructureNode(
            branchStatement, controlStructureType, branchStatement.code
        ))
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
//                                val typeFullName = getTypeFullNameFromExpression(value)
                                usedPrimitiveTypes.add(identifier.typeFullName)
                                val local = localNode(
                                    identifier, identifier.name.get,
                                    identifier.code,
                                    identifier.typeFullName
                                )
                                scope.addToScope(identifier.name.get, (local, identifier.typeFullName))
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
            val typeFullName = assignmentStatement.lhs.headOption match {
                case Some(head) => head match {
                    case identifier: Identifier => identifier.typeFullName
                    case _ => ""
                }
                case None => ""
            }
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

    private def astForIncDecStatement(fileName: String, incDecStatement: IncrementDecrementStatement): Ast = {
        val operator = incDecStatement.token match {
            case Token.Inc => Operators.postIncrement
            case Token.Dec => Operators.postDecrement
            case _ => Defines.Unknown
        }
        val cNode = callNode(incDecStatement, incDecStatement.code, operator, operator, DispatchTypes.STATIC_DISPATCH)
        incDecStatement.expression match {
            case Some(expression) => 
                val argAst = astForExpression(fileName, expression)
                callAst(cNode, Seq(argAst))
            case None => Ast()
        }
    }

}
