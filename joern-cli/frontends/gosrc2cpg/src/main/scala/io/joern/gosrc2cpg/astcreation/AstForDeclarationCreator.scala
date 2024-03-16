package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.ast.nodes.*
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.Ast
import io.joern.gosrc2cpg.ast.*
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
            case _ => {
                logger.warn(s"Unhandled declaration")
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
                val methodFullName = namespaceStack.peek() match {
                    case namespaceBlock: NewNamespaceBlock => namespaceBlock.fullName
                    case methodNode: NewMethod => methodNode.fullName
                    case _ => functionName
                }

                val methodNode_ = methodNode(
                    functionDecl, functionName,
                    functionDecl.code, methodFullName, None, fileName
                )

                namespaceStack.push(methodNode_)

                val bodyAst = astForStatement(fileName, functionDecl.body.get)

                namespaceStack.pop()

                methodAst(
                    methodNode_,
                    params.map(param => Ast(param)),
                    bodyAst.head,
                    returnNode
                )
            }
            case None => {
                logger.warn(s"Unhandled function declaration at $fileName")
                astForUnhandledNode(fileName, functionDecl)
            }
        }
    }
    
    private def astForGenericDeclaration(fileName: String, parentFullname: String, genericDeclaration: GenericDeclaration): Seq[Ast] = {
        val asts = ListBuffer[Ast]()
        genericDeclaration.specifications.map(specification => astForSpecification(
            fileName, parentFullname, specification
        )).foreach(seq => asts.addAll(seq))
        asts.toList
    }

}
