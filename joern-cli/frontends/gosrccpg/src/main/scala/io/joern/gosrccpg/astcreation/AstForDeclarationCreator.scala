package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.nodes.*
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.Ast
import io.joern.gosrccpg.ast.*

import scala.collection.mutable.ListBuffer

trait AstForDeclarationCreator(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>

    def astForDeclaration(fileName: String, declaration: Declaration): Seq[Ast] = {
        declaration match {
            case functionDeclaration: FunctionDeclaration => Seq(
                astForFunctionDeclaration(fileName, functionDeclaration)
            )
            case genericDeclaration: GenericDeclaration => astForGenericDeclaration(
                fileName, genericDeclaration
            )
            case _ => {
                logger.warn(s"Unhandled declaration")
                Seq()
            }
        }
    }

    def astForFunctionDeclaration(fileName: String, functionDecl: FunctionDeclaration): Ast = {
        if (functionDecl == null) {
            return Ast()
        }

        functionDecl.functionType match {
            case Some(functionType) => {
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
            case None => {
                logger.warn(s"Unhandled function declaration")
                astForUnhandledNode(fileName, functionDecl)
            }
        }
    }
    
    private def astForGenericDeclaration(fileName: String, genericDeclaration: GenericDeclaration): Seq[Ast] = {
        val asts = ListBuffer[Ast]()
        genericDeclaration.specifications.map(specification => astForSpecification(
            fileName, specification
        )).foreach(seq => asts.addAll(seq))
        asts.toList
    }

}
