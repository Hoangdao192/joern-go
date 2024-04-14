package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.ast.nodes.*
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.Ast
import io.joern.gosrc2cpg.ast.*
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, ModifierTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewNamespaceBlock}

import scala.collection.mutable.ListBuffer

trait AstForDeclarationCreator(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>

    def astForDeclaration(fileName: String, parentFullname: String, declaration: Declaration): Seq[Ast] = {
        declaration match {
            case functionDeclaration: FunctionDeclaration => Seq(
                astForFunctionDeclaration(fileName, functionDeclaration)
            )
            case genericDeclaration: GenericDeclaration => astForGenericDeclaration(
                fileName, parentFullname, genericDeclaration
            )
            case unknown => {
                logger.warn(s"Unhandled declaration type ${unknown.nodeType}")
                Seq()
            }
        }
    }

    private def astForFunctionDeclaration(fileName: String, functionDecl: FunctionDeclaration): Ast = {
        if (functionDecl == null) {
            return Ast()
        }

        functionDecl.functionType match {
            case Some(functionType) => {
                val (params, returnNode) = generateNodeFromFunctionType(fileName, functionDecl.functionType.get)

                val functionName = functionDecl.name.get.name.get

                val methodNode_ = methodNode(
                    functionDecl, functionName,
                    functionDecl.fullName, functionDecl.signature, fileName
                )

                namespaceStack.push(methodNode_)
                scope.pushNewScope(methodNode_)

                val bodyAst = astForStatement(fileName, functionDecl.body.get)
                
                scope.popScope()
                namespaceStack.pop()

                val modifier = if (functionName.headOption.exists(_.isUpper)) {
                    newModifierNode(ModifierTypes.PUBLIC)
                } else {
                    newModifierNode(ModifierTypes.PRIVATE)
                }

                methodAst(
                    methodNode_,
                    params.map(param => Ast(param)),
                    bodyAst.head,
                    returnNode,
                    Seq(modifier)
                )
            }
            case None => {
                logger.warn(s"Unhandled function declaration at $fileName")
                astForUnhandledNode(fileName, functionDecl)
            }
        }
    }

    private def astForReceiver(field: Field): Ast = {
        val fieldName = if (field.names != null && field.names.nonEmpty) {
            field.names.head.name.get
        } else {
            ""
        }
        val ast = NodeBuilders.newThisParameterNode(
            fieldName,
            field.code,
            field.typeFullName,
            evaluationStrategy = EvaluationStrategies.BY_REFERENCE
        )
        scope.addToScope(fieldName, (ast, field.typeFullName))
        Ast(ast)
    }

    private def astForGenericDeclaration(fileName: String, parentFullname: String, genericDeclaration: GenericDeclaration): Seq[Ast] = {
        val asts = ListBuffer[Ast]()
        genericDeclaration.specifications.map(specification => astForSpecification(
            fileName, parentFullname, specification
        )).foreach(seq => asts.addAll(seq))
        asts.toList
    }

}
