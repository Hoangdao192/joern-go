package io.joern.gosrc2cpg.astcreation

import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.joern.gosrc2cpg.ast.nodes.*
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
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
                logger.error(s"Unhandled specification node ${unknown.nodeType}")
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
        val asts = ListBuffer[Ast]()

        val identifiers = valueSpecification.names.toArray
        val values = valueSpecification.values.toArray

        var index: Int = 0

        for (identifier <- identifiers) {
            if (values.length > index) {
                val value = values(index)
                value match
                    case literal: FunctionLiteral =>
                        asts.addOne(
                            astForExpression(fileName, literal)
                        )
                    case _ =>
                        usedPrimitiveTypes.add(identifier.typeFullName)
                        val local: NewLocal = newLocalNode(identifier)
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
                index += 1
            } else {
                val local: NewLocal = newLocalNode(identifier)
                scope.addToScope(identifier.name.get, (local, identifier.typeFullName))
                asts.addOne(Ast(local))
                asts.addOne(astForExpression(fileName, identifier))
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
                case identifier: Identifier =>
                    usedPrimitiveTypes.add(typeSpecification.name.get.typeFullName)
                    val typeDecl = typeDeclNode(
                        typeSpecification,
                        typeSpecification.name.get.name.get,
                        typeSpecification.name.get.typeFullName,
                        filename,
                        typeSpecification.code
                    )
                    val modifier = typeSpecification.name.get.name.get.exists(_.isUpper) match {
                        case true => ModifierTypes.PUBLIC
                        case false => ModifierTypes.PRIVATE
                    }
                    Seq(
                        Ast(typeDecl)
                        .withChild(Ast(newModifierNode(modifier)))
                    )
                case interfaceType: InterfaceType =>
                    usedPrimitiveTypes.add(typeSpecification.name.get.typeFullName)
                    val typeDecl = typeDeclNode(
                        typeSpecification,
                        typeSpecification.name.get.name.get,
                        typeSpecification.name.get.typeFullName,
                        filename,
                        typeSpecification.code
                    )
                    val modifier = typeSpecification.name.get.name.get.exists(_.isUpper) match {
                        case true => ModifierTypes.PUBLIC
                        case false => ModifierTypes.PRIVATE
                    }
                    Seq(
                        Ast(typeDecl)
                            .withChild(Ast(newModifierNode(modifier)))
                    )
                case functionType: FunctionType =>
                    usedPrimitiveTypes.add(typeSpecification.name.get.typeFullName)
                    val typeDecl = typeDeclNode(
                        typeSpecification,
                        typeSpecification.name.get.name.get,
                        typeSpecification.name.get.typeFullName,
                        filename,
                        typeSpecification.code
                    )
                    val modifier = typeSpecification.name.get.name.get.exists(_.isUpper) match {
                        case true => ModifierTypes.PUBLIC
                        case false => ModifierTypes.PRIVATE
                    }
                    Seq(
                        Ast(typeDecl)
                            .withChild(Ast(newModifierNode(modifier)))
                    )
                case selectorExpression: SelectorExpression =>
                    usedPrimitiveTypes.add(typeSpecification.name.get.typeFullName)
                    val typeDecl = typeDeclNode(
                        typeSpecification,
                        typeSpecification.name.get.name.get,
                        typeSpecification.name.get.typeFullName,
                        filename,
                        typeSpecification.code
                    )
                    val modifier = typeSpecification.name.get.name.get.exists(_.isUpper) match {
                        case true => ModifierTypes.PUBLIC
                        case false => ModifierTypes.PRIVATE
                    }
                    Seq(
                        Ast(typeDecl)
                            .withChild(Ast(newModifierNode(modifier)))
                    )
                case _ =>
                    logger.warn(s"Unhandled type expression when parse type spec ${typeExpression.nodeType}")
                    logger.warn(typeExpression.code)
                    Seq.empty
            }
            case None => Seq.empty
        }
    }
    
    private def astForStructType(filename: String,
                                 parentFullName: String,
                                 typeSpecification: TypeSpecification, 
                                 structType: StructType): Ast = {
        val (name, fullname) = typeSpecification.name match {
            case Some(identifier) => identifier.name match {
                case Some(name) => (name, identifier.typeFullName)
                case None => ("<unknown>", "<unknown>")
            }
            case None => ("<unknown>", "<unknown>")
        }
        val fieldAsts = structType.fields match {
            case Some(fields) => astForFieldListNode(filename, fields)
            case None => Seq.empty
        }

        if (fullname.nonEmpty) {
            usedPrimitiveTypes.add(fullname)
        }

        val typeDecl = typeDeclNode(
            typeSpecification, name,
            fullname,
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

        val blockNode_ = blockNode(
            typeSpecification.name.get,
            typeSpecification.name.get.code,
            "void"
        )

        methodAst(
            methodNode_,
            Seq(),
            Ast(blockNode_),
            methodReturnNode(
                structType, typeSpecification.fullName
            ),
            Seq(modifier)
        )
    }
    
}
