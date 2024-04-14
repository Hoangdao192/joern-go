package io.joern.gosrc2cpg.astcreation

import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.joern.gosrc2cpg.ast.nodes.*
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, ModifierTypes, Operators}

import scala.collection.mutable.ListBuffer

trait AstForSpecificationCreator(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>

    def astForSpecification(fileName: String, parentFullName: String, specification: Specification): Seq[Ast] = {
        specification match {
            case importSpecification: ImportSpecification => Seq(astForImportSpecification(
                fileName, importSpecification
            ))
            case valueSpecification: ValueSpecification => astForValueSpecification(fileName, valueSpecification)
            case typeSpecification: TypeSpecification => astForTypeSpecification(fileName, parentFullName, typeSpecification)
            case unknown =>
                logger.error(s"Unhandled expression node ${unknown.nodeType}")
                Seq()
        }
    }

    private def astForImportSpecification(fileName: String, importSpecification: ImportSpecification): Ast = {
        val importPath = importSpecification.path.get.value
        val importAs = importSpecification.name match {
            case Some(identifier) => identifier.name.get
            case None => ""
        }

        val importNode = newImportNode(
            importSpecification.code,
            importPath,
            importAs, importSpecification.path.get
        )
        Ast(importNode)
    }

    private def astForValueSpecification(fileName: String, valueSpecification: ValueSpecification): Seq[Ast] = {
        var asts = ListBuffer[Ast]()

        val identifiers = valueSpecification.names.toArray
        val values = valueSpecification.values.toArray

        var index: Int = 0
        for (identifier <- identifiers) {
            if (values.length > index) {
                var value = values(index)
                if (value.isInstanceOf[FunctionLiteral]) {
                    asts.addOne(
                        astForExpression(fileName, value.asInstanceOf[FunctionLiteral])
                    )
                } else {
                    val typeFullName = valueSpecification.typeExpression match {
                        case Some(typeExpression) => getTypeFullNameFromExpression(typeExpression)
                        case None => Defines.Unknown
                    }
                    //  TODO: Handle type
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
            } else {
                val typeFullName = valueSpecification.typeExpression match {
                    case Some(typeExpression) => getTypeFullNameFromExpression(typeExpression)
                    case None => Defines.Unknown
                }
                //  TODO: Handle type
                val local = localNode(
                    identifier, identifier.name.get,
                    identifier.code,
                    typeFullName
                )
                scope.addToScope(identifier.name.get, (local, typeFullName))
                asts.addOne(Ast(local))
            }
        }

        asts.toList
    }

    private def astForTypeSpecification(
                                           filename: String,
                                           parentFullName: String,
                                           typeSpecification: TypeSpecification): Seq[Ast] = {
        typeSpecification.typeExpression match {
            case Some(typeExpression) => typeExpression match {
                case structType: StructType => Seq(astForStructType(
                    filename, parentFullName, typeSpecification, structType
                ))
                case _ =>
                    logger.warn("Unhandled type expression when parse type spec")
                    Seq.empty
            }
            case None => Seq.empty
        }
    }
    
    private def astForStructType(filename: String,
                                 parentFullName: String,
                                 typeSpecification: TypeSpecification, 
                                 structType: StructType): Ast = {
        val name = typeSpecification.name match {
            case Some(identifier) => identifier.name match {
                case Some(name) => name
                case None => "<unknown>"
            }
            case None => "<unknown>"
        }
        val fullname = s"$parentFullName.$name"
        val fieldAsts = structType.fields match {
            case Some(fields) => astForFieldListNode(filename, fields)
            case None => Seq.empty
        }

        val typeDecl = typeDeclNode(typeSpecification, name, s"$parentFullName.$name",
            filename, typeSpecification.code
        )
        val modifierAst = if (name.headOption.exists(_.isUpper)) {
            Ast(newModifierNode(ModifierTypes.PUBLIC))
        } else {
            Ast(newModifierNode(ModifierTypes.PRIVATE))
        }

        val constructor = createStructConstructor(
            filename, typeSpecification, structType
        )

        Ast(typeDecl).withChild(modifierAst)
            .withChild(constructor)
            .withChildren(
            fieldAsts.map(fieldAst => {
                val fieldModifier = if (name.headOption.exists(_.isUpper)) {
                    Ast(newModifierNode(ModifierTypes.PUBLIC))
                } else {
                    Ast(newModifierNode(ModifierTypes.PRIVATE))
                }
                fieldAst.withChild(fieldModifier)
            })
        )
    }

    private def createStructConstructor(
                                           filename: String,
                                           typeSpecification: TypeSpecification,
                                           structType: StructType)
    : Ast = {
        val methodNode_ = methodNode(
            structType, typeSpecification.name.get.name.get + ".<init>",
            typeSpecification.fullName + ".<init>",
            typeSpecification.fullName + "()",
            filename
        )

        val modifier = if (typeSpecification.name.get.name.get.headOption.exists(_.isUpper)) {
            newModifierNode(ModifierTypes.PUBLIC)
        } else {
            newModifierNode(ModifierTypes.PRIVATE)
        }
        
        methodAst(
            methodNode_,
            Seq(),
            Ast(),
            methodReturnNode(
                structType, typeSpecification.fullName
            ),
            Seq(modifier)
        )
    }
    
}
