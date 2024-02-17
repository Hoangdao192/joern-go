package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.nodes.*
import io.joern.x2cpg.{Ast, ValidationMode}

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) {
    this: AstCreator =>

    def astForFunctionDeclaration(fileName: String, functionDecl: FunctionDeclaration): Ast = {

        val (params, returnNode) = generateNodeFromFunctionType(fileName, functionDecl.functionType.get)

        val functionName = functionDecl.name.get.name.get

        val methodNode_ = methodNode(
            functionDecl, functionName,
            functionDecl.code, functionName, fileName
        )

        val bodyAst = astForStatement(fileName, functionDecl.body.get)

        methodAst(
            methodNode_,
            params.map(param => Ast(param)),
            bodyAst,
            returnNode
        )
    }

}
