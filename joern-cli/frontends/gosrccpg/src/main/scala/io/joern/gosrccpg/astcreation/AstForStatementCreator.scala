package io.joern.gosrccpg.astcreation

import io.joern.x2cpg.ValidationMode
import io.joern.gosrccpg.ast.nodes.*
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewControlStructure

trait AstForStatementCreator(implicit withSchemaValidation: ValidationMode) { this : AstCreator =>

  def astForStatement(fileName: String, statement: Statement): Ast = {
    var ast : Ast = statement match {
      case ifStatement: IfStatement => astForIfStatement(fileName: String, ifStatement: IfStatement)
    }
    controlStructureNode()
    return ast
  }

  private def astForIfStatement(fileName: String, ifStatement: IfStatement): Ast = {
    val conditionAst = astForExpression(fileName, ifStatement.condition.get)
    val ifNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.IF)
      .code(conditionAst.toString)

    val bodyAst = astForBlockStatement(fileName, ifStatement.body.get)
    val elseAst = astForStatement(fileName, ifStatement.elseStatement.get)

    Ast(ifNode)
      .withChildren(Seq(conditionAst, bodyAst, elseAst))
      .withConditionEdge(ifNode, conditionAst.root.get)
  }

  private def astForSwitchStatement(fileName: String, switchStatement: SwitchStatement): Ast = {
    val conditionAst = astForExpression(fileName: String, switchStatement.tag.get)

    val switchNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.SWITCH)
      .code(conditionAst.toString)

    val switchBodyAst = astForStatement(fileName, switchStatement.body)
    Ast(switchNode)
      .withChildren(Seq(conditionAst, switchBodyAst))
      .withConditionEdge(switchNode, conditionAst.root.get)
  }

  private def astForTypeSwitchStatement(fileName: String, typeSwitchStatement: TypeSwitchStatement): Ast = {
    val assignAst = astForStatement(typeSwitchStatement.assign.get)

    val switchNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.SWITCH)
      .code(assignAst.toString)

    val switchBodyAst = astForStatement(fileName, typeSwitchStatement.body.get)

    Ast(switchNode)
      .withChildren(Seq(switchBodyAst))
      .withConditionEdge(switchNode, assignAst.root.get)
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
    forAst(forNode, Seq(), Seq(initAst), Seq(conditionAst), Seq(updateAst), bodyAst)
  }

  private def astForRangeStatment(fileName: String, rangeStatement: RangeStatement): Ast = {

  }

  private def astForBlockStatement(fileName: String, blockStatement: BlockStatement): Ast = {
    Ast()
  }



}
