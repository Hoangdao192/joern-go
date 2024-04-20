package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.ast.{GoModule, Token}
import io.joern.gosrc2cpg.ast.nodes.*
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.joern.gosrc2cpg.Constant
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.{NewNamespaceBlock, NewMethod, NewMethodParameterIn, NewMethodReturn, NewUnknown}

import java.nio.file.Paths
import scala.collection.mutable.ListBuffer

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) {
    this: AstCreator =>

    def getCurrentScopeFullName(): String = {
        namespaceStack.peek() match {
            case namespaceBlock: NewNamespaceBlock => namespaceBlock.fullName
            case methodNode: NewMethod => methodNode.fullName
            case other =>
                logger.warn(s"Unknown scope type ${other.getClass}")
                ""
        }
    }

    def getPackageFullname(goModule: GoModule, packageFilePath: String, packageIdentifier: Identifier): String = {
        val relativePath = Paths.get(goModule.modulePath).relativize(
            Paths.get(packageFilePath).getParent
        ).toString
        val packageName = packageIdentifier.name match {
            case Some(_name) => _name
            case None =>
                logger.warn(s"Not found package name in file $packageFilePath")
                ""
        }
        var fullname = s"${goModule.moduleName}"
        if (relativePath.trim.nonEmpty) {
            fullname = s"$fullname.${relativePath.replace("/", ".")}"
        }
        if (packageName.trim.nonEmpty) {
            fullname = s"$fullname.$packageName"
        }
        fullname
    }

    def generateNodeFromFunctionType(fileName: String, functionType: FunctionType): (Seq[NewMethodParameterIn], NewMethodReturn) = {
        val functionTypeIdentifier = getFunctionReturnType(functionType)
        val returnNode = methodReturnNode(
            functionTypeIdentifier, functionTypeIdentifier.typeFullName
        )

        val params = ListBuffer[NewMethodParameterIn]()

        functionType.params match {
            case Some(paramFieldList) => {
                var index = 1
                for (field <- paramFieldList.fields) {
//                    val parameterType = astForExpression(fileName, field.typeExpression.get)
                    for (fieldIdentifier <- field.names) {
                        val parameterName = fieldIdentifier.name.get
                        val code = s"$parameterName ${fieldIdentifier.typeFullName}"
                        val parameterNodeIn = parameterInNode(
                            field, fieldIdentifier.name.get, code,
                            index, true, EvaluationStrategies.BY_VALUE,
                            fieldIdentifier.typeFullName
                        )
                        index += 1
                        usedPrimitiveTypes.add(fieldIdentifier.typeFullName)
                        scope.addToScope(parameterName, (parameterNodeIn, fieldIdentifier.typeFullName))
                        params.addOne(parameterNodeIn)
                    }
                }
            }
            case None => {}
        }


        (params.toList, returnNode)
    }

    def astForUnhandledNode(fileName: String, node: Node): Ast = {
        Ast(
            NewUnknown()
                .code(node.code)
        )
    }
    
    private def getFunctionReturnType(functionType: FunctionType): Identifier = {
        functionType.results match {
            case Some(results) => {
                val fieldList: FieldList = functionType.results.get
                val fields: ListBuffer[Field] = fieldList.fields
                val typeName = fields.map(field => {
                    val expression = field.typeExpression.get
                    expression match {
                        case identifier: Identifier => identifier.typeFullName
                        case starExpression: StarExpression => starExpression.expression match {
                            case Some(expr) => expr match {
                                case identifier: Identifier => identifier.typeFullName
                                case _ => logger.warn("[getFunctionReturnType] Unhandled type expression " + expr.getClass.getTypeName)
                            }
                            case None => ""
                        }
                        case interfaceType: InterfaceType => "interface{}"
                        case _ =>
                            println(functionType.code)
                            println("[getFunctionReturnType] Not handled type expression " + expression.getClass.getTypeName)
                            ""
                    }
                }).mkString("(", ", ", ")")
                val identifier = Identifier()
                identifier.name = Option(typeName)
                identifier
            }
            case None => {
                val identifier = Identifier()
                identifier.name = Option("unit")
                identifier
            }
        }
    }
    
    private def getFieldNodeTypeName(field: Field): Option[String] = {
        if (field.typeExpression.isDefined) {
            val typeExpression = field.typeExpression.get;
            val typeName = typeExpression match {
                case identifier: Identifier =>
                    identifier.name
                case _ => None
            }
            return typeName
        }
        None
    }
    
    private def getFunctionTypeSignature(functionType: FunctionType): String = {
        val returnTypeIdentifier = getFunctionReturnType(functionType)
        val parameters = functionType.params match {
            case Some(fieldList) =>
                fieldList.fields.map(field => {
                    val typeNameOption = getFieldNodeTypeName(field)
                    val name = typeNameOption match {
                        case Some(typeName) => typeName
                        case None => ""
                    }
                    name
                }).mkString(", ")
            case None => ""
        }
        s"($parameters):${returnTypeIdentifier.name.get}"
    }
    
    def generateNodeSignature(node: Node): String = {
        node match {
            case functionDeclaration: FunctionDeclaration => 
                val functionName = functionDeclaration.name match {
                    case Some(name) => name.name match { 
                        case Some(value) => value 
                        case None => ""
                    }
                    case None => ""
                }
                val functionType = functionDeclaration.functionType match {
                    case Some(functionType) => getFunctionTypeSignature(functionType)
                    case None => ""
                }
                s"$functionName$functionType"
            
        }
    }

    private def resolveType(expression: Expression): (String, Boolean) = {
        expression match {
            case identifier: Identifier => identifier.name match {
                case Some(name) =>
                    val isPrimitive = Constant.PRIMITIVE_TYPES.contains(name)
                    if (isPrimitive) {
                        (name, isPrimitive)
                    } else {
                        usedPrimitiveTypes.add(getCurrentScopeFullName() + "." + name)
                        (getCurrentScopeFullName() + "." + name, isPrimitive)
                    }
                case None => (Defines.Unknown, false)
            }
            case basicLiteralExpression: BasicLiteralExpression =>
                val typeName = basicLiteralExpression.kind match {
                    case Token.Int => "int"
                    case Token.Float => "float32"
                    case Token.Imag => "imag"
                    case Token.Char => "char"
                    case Token.String => "string"
                    case _ => Defines.Unknown
                }
                (typeName, Constant.PRIMITIVE_TYPES.contains(typeName))
            case arrayType: ArrayType =>
                val (fullname, primitive) = arrayType.element match {
                    case Some(elementIdentifier) => resolveType(elementIdentifier)
                    case None => (Defines.Unknown, false)
                }
                (s"[]$fullname", primitive)
            case starExpression: StarExpression =>
                val (fullname, primitive) = starExpression.expression match {
                    case Some(innerExpression) => resolveType(innerExpression)
                    case None => (Defines.Unknown, false)
                }
                (s"*$fullname", primitive)
            case _ =>
                logger.warn(s"Unhandled type expression ${expression.getClass.toString}")
                logger.warn(s"Code: ${expression.code}")
                (Defines.Unknown, false)
        }
    }

    def getTypeFullNameFromExpression(expression: Expression): String = {
        val (typeFullname, isPrimitiveType) = resolveType(expression)
        if (isPrimitiveType) {
            usedPrimitiveTypes.add(typeFullname)
        }
        typeFullname
    }

}
