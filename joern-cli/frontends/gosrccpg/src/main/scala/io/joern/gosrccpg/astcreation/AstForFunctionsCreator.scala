package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.nodes.FunctionDeclaration
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodReturn
import io.joern.x2cpg.Ast

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForFunctionDeclaration(fileName: String, functionDecl: FunctionDeclaration): Ast = {
      val functionName = functionDecl.name.get.name.get
      val methodNode_ = methodNode(
        functionDecl, functionName, functionName, functionName, fileName
      )
      methodAst(
        methodNode_,
        List(),
        Ast(),
        NewMethodReturn(),
        List()
      )
  }

}
