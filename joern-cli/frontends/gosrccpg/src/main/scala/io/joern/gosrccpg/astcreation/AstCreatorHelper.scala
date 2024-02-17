package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.nodes.*
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethodParameterIn, NewMethodReturn}

import scala.collection.mutable.ListBuffer

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) {
    this: AstCreator =>

    def generateNodeFromFunctionType(fileName: String, functionType: FunctionType): (Seq[NewMethodParameterIn], NewMethodReturn) = {
        val functionTypeIdentifier = getFunctionReturnType(functionType)
        val returnNode = methodReturnNode(
            functionTypeIdentifier, functionTypeIdentifier.name.get
        )

        val paramFieldList = functionType.params.get

        val params = ListBuffer[NewMethodParameterIn]()
        var index = 1
        for (field <- paramFieldList.fields) {
            val parameterType = astForExpression(fileName, field.typeExpression.get)
            for (fieldIdentifier <- field.names) {
                val parameterName = fieldIdentifier.name.get
                val parameterTypeName = field.typeExpression.get match {
                    case identifier: Identifier => identifier.name.get
                    case _ => ""
                }
                val code = s"$parameterName $parameterTypeName"
                val parameterNodeIn = parameterInNode(
                    field, fieldIdentifier.name.get, code,
                    index, true, EvaluationStrategies.BY_VALUE,
                    parameterTypeName
                )
                index += 1
                params.addOne(parameterNodeIn)
            }
        }

        (params.toList, returnNode)
    }

    private def getFunctionReturnType(functionType: FunctionType): Identifier = {
        val fieldList: FieldList = functionType.results.get
        val fields: ListBuffer[Field] = fieldList.fields
        val typeName = fields.map(field => {
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
