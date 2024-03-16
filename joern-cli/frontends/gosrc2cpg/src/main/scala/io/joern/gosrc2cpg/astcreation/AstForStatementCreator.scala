package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.ast.Token
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.joern.gosrc2cpg.ast.nodes.*
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewControlStructure}

import scala.collection.mutable.ListBuffer

trait AstForStatementCreator(implicit withSchemaValidation: ValidationMode) {
    this: AstCreator =>

    def astForStatement(fileName: String, statement: Statement): Seq[Ast] = {
        statement match {
            case declarationStatement: DeclarationStatement => astForDeclarationStatement(fileName, declarationStatement)
            case ifStatement: IfStatement => Seq(astForIfStatement(fileName, ifStatement))
            case switchStatement: SwitchStatement => Seq(astForSwitchStatement(fileName, switchStatement))
            case typeSwitchStatement: TypeSwitchStatement => Seq(astForTypeSwitchStatement(fileName, typeSwitchStatement))
            case forStatement: ForStatement => Seq(astForForStatement(fileName, forStatement))
            case rangeStatement: RangeStatement => Seq(astForRangeStatement(fileName, rangeStatement))
            case blockStatement: BlockStatement => Seq(astForBlockStatement(fileName, blockStatement))
            case branchStatement: BranchStatement => Seq(astForBranchStatement(fileName, branchStatement))
            case _ => Seq()
        }
    }

    private def astForIfStatement(fileName: String, ifStatement: IfStatement): Ast = {
        val conditionAst = astForExpression(fileName, ifStatement.condition.get)
        val ifNode = NewControlStructure()
            .controlStructureType(ControlStructureTypes.IF)
            .code(conditionAst.toString)

        val bodyAst = astForBlockStatement(fileName, ifStatement.body.get)
        val elseAst = astForStatement(fileName, ifStatement.elseStatement.get)

        Ast(ifNode)
            .withChildren(Seq(conditionAst, bodyAst, elseAst.head))
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
        val initAst = astForStatement(fileName, forStatement.initialization.get)
        val conditionAst = astForExpression(fileName, forStatement.condition.get)
        val updateAst = astForStatement(fileName, forStatement.post.get)
        val bodyAst = astForStatement(fileName, forStatement.body.get)

        //  TODO: Handle for loop code
        val forNode = NewControlStructure()
            .controlStructureType(ControlStructureTypes.FOR)
            .code("")
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
            .columnNumber(column(blockStatement ))
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

}
