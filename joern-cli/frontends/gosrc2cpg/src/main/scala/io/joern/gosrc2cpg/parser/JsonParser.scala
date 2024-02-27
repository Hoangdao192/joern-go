package io.joern.gosrc2cpg.parser

import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import io.joern.gosrc2cpg.ast.nodes.*
import com.google.gson.{Gson, JsonDeserializer, JsonElement, JsonParseException, JsonSerializer}
import io.shiftleft.utils.IOUtils

import java.nio.file.Paths
import scala.collection.mutable.ListBuffer

/**
 * Load go ast from json to Node
 */
class JsonParser {

    private val gson = new Gson()
    private val objectMapper = new ObjectMapper()
    objectMapper.registerModule(DefaultScalaModule)
    objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)

    def parse(filepath: String): FileNode = {
        val fileContent: String = IOUtils.readEntireFile(Paths.get(filepath))
        //    val fileNode: FileNode = gson.fromJson(fileContent, classOf[FileNode])
        val fileNode: FileNode = objectMapper.readValue(fileContent, classOf[FileNode])
        mapParent(Option(fileNode), None)
        fileNode
    }

    private def mapParent(node: Node, parent: Node): Unit = {
        mapParent(Option(node), Option(parent))
    }

    private def mapParent(node: Option[Node], parent: Node): Unit = {
        mapParent(node, Option(parent))
    }

    private def mapParent(node: Option[Node], parent: Option[Node]): Unit = {

        if (node.isDefined) {
            if (parent.isDefined) {
                node.get.setParent(parent.get)
            }

            node.get match {
                case functionDeclaration: FunctionDeclaration =>
                    mapParent(functionDeclaration.documentation, Option(functionDeclaration))
                    mapParent(functionDeclaration.receiver, Option(functionDeclaration))
                    mapParent(functionDeclaration.name, Option(functionDeclaration))
                    mapParent(functionDeclaration.functionType, Option(functionDeclaration))
                    mapParent(functionDeclaration.body, Option(functionDeclaration))

                case genericDeclaration: GenericDeclaration =>
                    mapParent(genericDeclaration.documentation, Option(genericDeclaration))
                    genericDeclaration.specifications.foreach(mapParent(_, genericDeclaration))

                // Expression
                case callExpression: CallExpression =>
                    mapParent(callExpression.function, Option(callExpression))
                    callExpression.args.foreach(mapParent(_, callExpression))

                case binaryExpression: BinaryExpression =>
                    mapParent(binaryExpression.leftExpression, Option(binaryExpression))
                    mapParent(binaryExpression.rightExpression, Option(binaryExpression))

                case ellipsisExpression: EllipsisExpression =>
                    mapParent(ellipsisExpression.element, Option(ellipsisExpression))

                case indexExpression: IndexExpression =>
                    mapParent(indexExpression.expression, Option(indexExpression))
                    mapParent(indexExpression.index, Option(indexExpression))

                case indexListExpression: IndexListExpression =>
                    mapParent(indexListExpression.expression, Option(indexListExpression))
                    indexListExpression.indicates.foreach(mapParent(_, indexListExpression))

                case keyValueExpression: KeyValueExpression =>
                    mapParent(keyValueExpression.key, keyValueExpression)
                    mapParent(keyValueExpression.value, keyValueExpression)

                case parenthesizedExpression: ParenthesizedExpression =>
                    mapParent(parenthesizedExpression.expression, parenthesizedExpression)

                case selectorExpression: SelectorExpression =>
                    mapParent(selectorExpression.expression, selectorExpression)
                    mapParent(selectorExpression.selector, selectorExpression)

                case sliceExpression: SliceExpression =>
                    mapParent(sliceExpression.expression, sliceExpression)
                    mapParent(sliceExpression.low, sliceExpression)
                    mapParent(sliceExpression.max, sliceExpression)

                case starExpression: StarExpression =>
                    mapParent(starExpression.expression, starExpression)

                case typeAssertExpression: TypeAssertExpression =>
                    mapParent(typeAssertExpression.expression, typeAssertExpression)
                    mapParent(typeAssertExpression.typeExpression, typeAssertExpression)

                case unaryExpression: UnaryExpression =>
                    mapParent(unaryExpression.expression, unaryExpression)

                case functionLiteral: FunctionLiteral =>
                    mapParent(functionLiteral.functionType, functionLiteral)
                    mapParent(functionLiteral.body, functionLiteral)

                case compositeLiteral: CompositeLiteral =>
                    mapParent(compositeLiteral.typeExpression, compositeLiteral)
                    compositeLiteral.elements.foreach(mapParent(_, compositeLiteral))

                //  Field
                case field: Field =>
                    mapParent(field.documentation, field)
                    field.names.foreach(mapParent(_, field))
                    mapParent(field.typeExpression, field)
                    mapParent(field.tag, field)
                    mapParent(field.comment, field)

                case fieldList: FieldList =>
                    fieldList.fields.foreach(mapParent(_, fieldList))

                // File
                case file: FileNode =>
                    mapParent(file.documentation, file)
                    mapParent(file.name, file)
                    file.declarations.foreach(mapParent(_, file))
                    file.imports.foreach(mapParent(_, file))
                    file.unresolved.foreach(mapParent(_, file))
                    file.comments.foreach(mapParent(_, file))

                //  Specification
                case importSpecification: ImportSpecification =>
                    mapParent(importSpecification.documentation, importSpecification)
                    mapParent(importSpecification.name, importSpecification)
                    mapParent(importSpecification.path, importSpecification)
                    mapParent(importSpecification.comment, importSpecification)

                case typeSpecification: TypeSpecification =>
                    mapParent(typeSpecification.documentation, typeSpecification)
                    mapParent(typeSpecification.name, typeSpecification)
                    mapParent(typeSpecification.typeParams, typeSpecification)
                    mapParent(typeSpecification.typeExpression, typeSpecification)
                    mapParent(typeSpecification.comment, typeSpecification)

                case valueSpecification: ValueSpecification =>
                    mapParent(valueSpecification.documentation, valueSpecification)
                    valueSpecification.names.foreach(mapParent(_, valueSpecification))
                    mapParent(valueSpecification.typeExpression, valueSpecification)
                    valueSpecification.values.foreach(mapParent(_, valueSpecification))
                    mapParent(valueSpecification.comment, valueSpecification)

                //  Statement
                case assignStatement: AssignStatement =>
                    assignStatement.lhs.foreach(mapParent(_, assignStatement))
                    assignStatement.rhs.foreach(mapParent(_, assignStatement))

                case blockStatement: BlockStatement =>
                    blockStatement.statements.foreach(mapParent(_, blockStatement))

                case branchStatement: BranchStatement =>
                    mapParent(branchStatement.label, branchStatement)

                case declarationStatement: DeclarationStatement =>
                    mapParent(declarationStatement.declaration, declarationStatement)

                case deferStatement: DeferStatement =>
                    mapParent(deferStatement.call, deferStatement)

                case expressionStatement: ExpressionStatement =>
                    mapParent(expressionStatement.expression, expressionStatement)

                case forStatement: ForStatement =>
                    mapParent(forStatement.initialization, forStatement)
                    mapParent(forStatement.condition, forStatement)
                    mapParent(forStatement.post, forStatement)
                    mapParent(forStatement.body, forStatement)

                case goStatement: GoStatement =>
                    mapParent(goStatement.call, goStatement)

                case ifStatement: IfStatement =>
                    mapParent(ifStatement.initialization, ifStatement)
                    mapParent(ifStatement.condition, ifStatement)
                    mapParent(ifStatement.body, ifStatement)
                    mapParent(ifStatement.elseStatement, ifStatement)

                case incDecStatement: IncrementDecrementStatement =>
                    mapParent(incDecStatement.expression, incDecStatement)

                case labeledStatement: LabeledStatement =>
                    mapParent(labeledStatement.statement, labeledStatement)
                    mapParent(labeledStatement.label, labeledStatement)

                case rangeStatement: RangeStatement =>
                    mapParent(rangeStatement.key, rangeStatement)
                    mapParent(rangeStatement.value, rangeStatement)
                    mapParent(rangeStatement.expression, rangeStatement)
                    mapParent(rangeStatement.body, rangeStatement)

                case returnStatement: ReturnStatement =>
                    returnStatement.results.foreach(mapParent(_, returnStatement))

                case selectStatement: SelectStatement =>
                    mapParent(selectStatement.body, selectStatement)

                case sendStatement: SendStatement =>
                    mapParent(sendStatement.chanel, sendStatement)
                    mapParent(sendStatement.value, sendStatement)

                case switchStatement: SwitchStatement =>
                    mapParent(switchStatement.initialization, switchStatement)
                    mapParent(switchStatement.tag, switchStatement)
                    mapParent(switchStatement.body, switchStatement)

                case typeSwitchStatement: TypeSwitchStatement =>
                    mapParent(typeSwitchStatement.initialization, typeSwitchStatement)
                    mapParent(typeSwitchStatement.assign, typeSwitchStatement)
                    mapParent(typeSwitchStatement.body, typeSwitchStatement)

                //  Type
                case functionType: FunctionType =>
                    mapParent(functionType.typeParams, functionType)
                    mapParent(functionType.params, functionType)
                    mapParent(functionType.results, functionType)

                case arrayType: ArrayType =>
                    mapParent(arrayType.length, arrayType)
                    mapParent(arrayType.element, arrayType)

                case chanelType: ChanelType =>
                    mapParent(chanelType.value, chanelType)

                case interfaceType: InterfaceType =>
                    mapParent(interfaceType.methods, interfaceType)

                case mapType: MapType =>
                    mapParent(mapType.key, mapType)
                    mapParent(mapType.value, mapType)

                case structType: StructType =>
                    mapParent(structType.fields, structType)

                case _ =>
            }
        }

    }

    private def mapParent(node: Node): Unit = {
        node match {
            case functionDeclaration: FunctionDeclaration =>
                if (functionDeclaration.documentation.isDefined) {
                    functionDeclaration.documentation.get.setParent(functionDeclaration)
                }
                if (functionDeclaration.receiver.isDefined) {
                    functionDeclaration.receiver.get.setParent(functionDeclaration)
                }
                if (functionDeclaration.name.isDefined) {
                    functionDeclaration.name.get.setParent(functionDeclaration)
                }
                if (functionDeclaration.functionType.isDefined) {
                    functionDeclaration.functionType.get.setParent(functionDeclaration)
                }
                if (functionDeclaration.body.isDefined) {
                    functionDeclaration.body.get.setParent(functionDeclaration)
                }
            case genericDeclaration: GenericDeclaration =>
                if (genericDeclaration.documentation.isDefined) {
                    genericDeclaration.documentation.get.setParent(genericDeclaration)
                }
                for (specification <- genericDeclaration.specifications) {
                    specification.setParent(genericDeclaration)
                }
            // Expression
            case callExpression: CallExpression =>
                if (callExpression.function.isDefined) {
                    callExpression.function.get.setParent(callExpression)
                }
                for (arg <- callExpression.args) {
                    arg.setParent(callExpression)
                }
            case binaryExpression: BinaryExpression =>
                if (binaryExpression.leftExpression.isDefined) {
                    binaryExpression.leftExpression.get.setParent(binaryExpression)
                }
                if (binaryExpression.rightExpression.isDefined) {
                    binaryExpression.rightExpression.get.setParent(binaryExpression)
                }
            case ellipsisExpression: EllipsisExpression =>
                if (ellipsisExpression.element.isDefined) {
                    ellipsisExpression.element.get.setParent(ellipsisExpression)
                }
            case indexExpression: IndexExpression =>
                if (indexExpression.expression.isDefined) {
                    indexExpression.expression.get.setParent(indexExpression)
                }
                if (indexExpression.index.isDefined) {
                    indexExpression.index.get.setParent(indexExpression)
                }
            case indexListExpression: IndexListExpression =>
                if (indexListExpression.expression.isDefined) {
                    indexListExpression.expression.get.setParent(indexListExpression)
                }
                for (indicate <- indexListExpression.indicates) {
                    indicate.setParent(indexListExpression)
                }
            case keyValueExpression: KeyValueExpression =>
                if (keyValueExpression.key.isDefined) {
                    keyValueExpression.key.get.setParent(keyValueExpression)
                }
                if (keyValueExpression.value.isDefined) {
                    keyValueExpression.value.get.setParent(keyValueExpression)
                }
            case parenthesizedExpression: ParenthesizedExpression =>
                if (parenthesizedExpression.expression.isDefined) {
                    parenthesizedExpression.expression.get.setParent(parenthesizedExpression)
                }
            case selectorExpression: SelectorExpression =>
                if (selectorExpression.expression.isDefined) {
                    selectorExpression.expression.get.setParent(selectorExpression)
                }
                if (selectorExpression.selector.isDefined) {
                    setParent(selectorExpression.selector, selectorExpression)
                }
            case sliceExpression: SliceExpression =>
                setParent(sliceExpression.expression, sliceExpression)
                setParent(sliceExpression.low, sliceExpression)
                setParent(sliceExpression.max, sliceExpression)
            case starExpression: StarExpression =>
                setParent(starExpression.expression, starExpression)
            case typeAssertExpression: TypeAssertExpression =>
                setParent(typeAssertExpression.expression, typeAssertExpression)
                setParent(typeAssertExpression.typeExpression, typeAssertExpression)
            case unaryExpression: UnaryExpression =>
                setParent(unaryExpression.expression, unaryExpression)
            case functionLiteral: FunctionLiteral =>
                setParent(functionLiteral.functionType, functionLiteral)
                setParent(functionLiteral.body, functionLiteral)
            case compositeLiteral: CompositeLiteral =>
                setParent(compositeLiteral.typeExpression, compositeLiteral)
                for (element <- compositeLiteral.elements) {
                    element.setParent(compositeLiteral)
                }
            //  Field
            case field: Field =>
                setParent(field.documentation, field)
                for (name <- field.names) {
                    name.setParent(field)
                }
                setParent(field.typeExpression, field)
                setParent(field.tag, field)
                setParent(field.comment, field)
            case fieldList: FieldList =>
                for (field <- fieldList.fields) {
                    field.setParent(fieldList)
                }
            // File
            case file: FileNode =>
                setParent(file.documentation, file)
                setParent(file.name, file)
                for (declaration <- file.declarations) {
                    declaration.setParent(file)
                }
                for (importNode <- file.imports) {
                    importNode.setParent(file)
                }
                for (unresolvedNode <- file.unresolved) {
                    unresolvedNode.setParent(file)
                }
                for (comment <- file.comments) {
                    comment.setParent(file)
                }

            //  Specification
            case importSpecification: ImportSpecification =>
                setParent(importSpecification.documentation, importSpecification)
                setParent(importSpecification.name, importSpecification)
                setParent(importSpecification.path, importSpecification)
                setParent(importSpecification.comment, importSpecification)
            case typeSpecification: TypeSpecification =>
                setParent(typeSpecification.documentation, typeSpecification)
                setParent(typeSpecification.name, typeSpecification)
                setParent(typeSpecification.typeParams, typeSpecification)
                setParent(typeSpecification.typeExpression, typeSpecification)
                setParent(typeSpecification.comment, typeSpecification)
            case valueSpecification: ValueSpecification =>
                setParent(valueSpecification.documentation, valueSpecification)
                for (name <- valueSpecification.names) {
                    name.setParent(valueSpecification)
                }
                setParent(valueSpecification.typeExpression, valueSpecification)
                for (value <- valueSpecification.values) {
                    value.setParent(valueSpecification)
                }
                setParent(valueSpecification.comment, valueSpecification)

            //  Statement
            case assignStatement: AssignStatement =>
                for (expression <- assignStatement.lhs) {
                    expression.setParent(assignStatement)
                }
                for (expression <- assignStatement.rhs) {
                    expression.setParent(assignStatement)
                }
            case blockStatement: BlockStatement =>
                for (statement <- blockStatement.statements) {
                    statement.setParent(blockStatement)
                }
            case branchStatement: BranchStatement =>
                setParent(branchStatement.label, branchStatement)
            case declarationStatement: DeclarationStatement =>
                setParent(declarationStatement.declaration, declarationStatement)
            case deferStatement: DeferStatement =>
                setParent(deferStatement.call, deferStatement)
            case expressionStatement: ExpressionStatement =>
                setParent(expressionStatement.expression, expressionStatement)
            case forStatement: ForStatement =>
                setParent(forStatement.initialization, forStatement)
                setParent(forStatement.condition, forStatement)
                setParent(forStatement.post, forStatement)
                setParent(forStatement.body, forStatement)
            case goStatement: GoStatement =>
                setParent(goStatement.call, goStatement)
            case ifStatement: IfStatement =>
                setParent(ifStatement.initialization, ifStatement)
                setParent(ifStatement.condition, ifStatement)
                setParent(ifStatement.body, ifStatement)
                setParent(ifStatement.elseStatement, ifStatement)
            case incDecStatement: IncrementDecrementStatement =>
                setParent(incDecStatement.expression, incDecStatement)
            case labeledStatement: LabeledStatement =>
                setParent(labeledStatement.statement, labeledStatement)
                setParent(labeledStatement.label, labeledStatement)
            case rangeStatement: RangeStatement =>
                setParent(rangeStatement.key, rangeStatement)
                setParent(rangeStatement.value, rangeStatement)
                setParent(rangeStatement.expression, rangeStatement)
                setParent(rangeStatement.body, rangeStatement)
            case returnStatement: ReturnStatement =>
                for (result <- returnStatement.results) {
                    result.setParent(returnStatement)
                }
            case selectStatement: SelectStatement =>
                setParent(selectStatement.body, selectStatement)
            case sendStatement: SendStatement =>
                setParent(sendStatement.chanel, sendStatement)
                setParent(sendStatement.value, sendStatement)
            case switchStatement: SwitchStatement =>
                setParent(switchStatement.initialization, switchStatement)
                setParent(switchStatement.tag, switchStatement)
                setParent(switchStatement.body, switchStatement)
            case typeSwitchStatement: TypeSwitchStatement =>
                setParent(typeSwitchStatement.initialization, typeSwitchStatement)
                setParent(typeSwitchStatement.assign, typeSwitchStatement)
                setParent(typeSwitchStatement.body, typeSwitchStatement)
            //  Type
            case functionType: FunctionType =>
                setParent(functionType.typeParams, functionType)
                setParent(functionType.params, functionType)
                setParent(functionType.results, functionType)
            case arrayType: ArrayType =>
                setParent(arrayType.length, arrayType)
                setParent(arrayType.element, arrayType)
            case chanelType: ChanelType =>
                setParent(chanelType.value, chanelType)
            case interfaceType: InterfaceType =>
                setParent(interfaceType.methods, interfaceType)
            case mapType: MapType =>
                setParent(mapType.key, mapType)
                setParent(mapType.value, mapType)
            case structType: StructType =>
                setParent(structType.fields, structType)
        }
    }

    private def setParent(child: Option[Node], parent: Node): Unit = {
        if (child.isDefined) {
            child.get.setParent(parent)
        }
    }

}
