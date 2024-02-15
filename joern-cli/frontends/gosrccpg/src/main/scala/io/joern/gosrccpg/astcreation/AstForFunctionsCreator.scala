package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.nodes.{Identifier, *}
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodReturn
import io.joern.x2cpg.Ast

import scala.collection.mutable.ListBuffer

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForFunctionDeclaration(fileName: String, functionDecl: FunctionDeclaration): Ast = {

    val functionTypeIdentifier = getFunctionReturnType(functionDecl)
    val returnNode = methodReturnNode(
      functionTypeIdentifier, functionTypeIdentifier.name.get
    )

    functionDecl.functionType.get.params

    val functionName = functionDecl.name.get.name.get

    val methodNode_ = methodNode(
      functionDecl, functionName,
      functionDecl.code, functionName, fileName
    )

    methodAst(
      methodNode_,
      List(),
      Ast(),
      NewMethodReturn(),
      List()
    )
  }

  private def getFunctionReturnType(functionDecl: FunctionDeclaration): Identifier = {
    val functionType: FunctionType = functionDecl.functionType.get;
    val fieldList: FieldList = functionType.results.get
    val fields: ListBuffer[Field] = fieldList.fields
    var typeName = fields.map(field => {
      val expression = field.typeExpression.get
      expression match {
        case identifier: Identifier => identifier.name.get
        case _ =>
          println("[getFunctionReturnType] Not handled type expression " + expression.getClass.getTypeName)
          ""
      }
    }).mkString("(", ", ", ")")
    val identifier = Identifier()
    identifier.name = Option(typeName)
    identifier
  }

}
