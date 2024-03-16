package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.ast.nodes.*
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewNamespaceBlock, NewNode, NewType}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import com.fasterxml.jackson.databind.ObjectMapper
import io.joern.gosrc2cpg.ast.GoModule

import java.nio.file.Paths
import java.util
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class AstCreator(rootNode: FileNode, filename: String, goModule: GoModule, protected var usedPrimitiveTypes: util.Set[String])(implicit val validationMode: ValidationMode)
    extends AstCreatorBase(filename)
        with AstCreatorHelper
        with AstForExpressionCreator
        with AstForStatementCreator
        with AstForDeclarationCreator
        with AstForSpecificationCreator
        with AstNodeBuilder[Node, AstCreator] {

    protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])
    protected val objectMapper: ObjectMapper = ObjectMapper()
    protected val namespaceStack: util.Stack[NewNode] = new util.Stack()
    protected val namespaceMap: util.Map[String, NewNode] = new util.HashMap()
    protected val typeSet: util.Set[String] = new util.HashSet[String]()
    protected var globalAst: Option[Ast] = None

    def createAst(): DiffGraphBuilder = {
        val ast = astForTranslationUnit(rootNode)
//        val asts = astForGoAstNode(rootNode)
//        for (ast <- asts) {
//            Ast.storeInDiffGraph(ast, diffGraph)
//        }
        Ast.storeInDiffGraph(ast, diffGraph)
        globalAst = Option(ast)
        diffGraph
    }

    def createAstCustom(): (DiffGraphBuilder, Ast) = {
        val ast = astForTranslationUnit(rootNode)
        Ast.storeInDiffGraph(ast, diffGraph)
        (diffGraph, ast)
    }

    private def generatePrimitiveType(): Seq[Ast] = {
        val typeAsts = new ListBuffer[Ast]()
        usedPrimitiveTypes.forEach(primitiveType => {
            val ast = Ast(typeDeclNode(
                new PrimitiveType(Option(primitiveType)),
                primitiveType,
                primitiveType,
                Defines.Unknown,
                primitiveType
            ))
            typeAsts.addOne(ast)
        })
        typeAsts.toSeq
    }

    private def astForTranslationUnit(root: FileNode): Ast = {
        val (fullname, namespaceAst) = root.name match {
            case Some(packageIdent) => astForPackageNode(root.filePath, root.name)
            case None =>
                (goModule.moduleName, Ast(NewNamespaceBlock().name("\\").fullName(goModule.moduleName).filename(root.filePath)))
        }

        namespaceStack.push(namespaceAst.nodes.head)
        
        val childrenAst = ListBuffer[Ast]()
        val parentPath = Paths.get(goModule.modulePath);
        val childPath = Paths.get(root.filePath)
        val filePath = parentPath.relativize(childPath).toString
        for (decl <- root.declarations) {
            childrenAst.addAll(astForGoAstNode(decl, fullname, filePath))
        }
        
        namespaceStack.pop()
        namespaceAst.withChildren(childrenAst)
    }

    private def astForGoAstNode(node: Node, parentFullname: String, filePath: String): Seq[Ast] = {
        val asts: Seq[Ast] = node match {
            case declaration: Declaration => astForDeclaration(filePath, parentFullname, declaration)
            case statement: Statement => astForStatement(filePath, statement)
            case expression: Expression => Seq(astForExpression(filePath, expression))
            case specification: Specification => astForSpecification(filePath,parentFullname, specification)
            case unknown => {
                logger.warn(s"Unknown node type")
                Seq()
            }
        }
        asts
    }

    private def astForPackageNode(filename: String,
                                  packageIdentifier: Option[Identifier]): (String, Ast) = {
        val (name, fullname) = packageIdentifier match {
            case Some(packageDecl) =>
                (packageDecl.name.get, getPackageFullname(goModule, filename, packageDecl))
            case None =>
                ("\\", goModule.moduleName)
        }
        
        var namespaceBlock = if (namespaceMap.containsKey(fullname)) {
            namespaceMap.get(fullname)
        } else {
            val parentPath = Paths.get(goModule.modulePath);
            val childPath = Paths.get(filename)
            NewNamespaceBlock()
                .name(name)
                .fullName(fullname)
                .filename(parentPath.relativize(childPath).toString)
        }
        namespaceMap.put(
            fullname, namespaceBlock
        )
        (fullname, Ast(namespaceBlock))
    }

    def astForFieldListNode(filename: String, fieldList: FieldList): Seq[Ast] = {
        val fieldAsts = ListBuffer[Ast]()
        fieldList.fields.foreach(field => fieldAsts.addAll(astForFieldNode(
            filename, field
        )))
        fieldAsts.toArray
    }
    
    def astForFieldNode(filename: String, field: Field): Seq[Ast] = {
        field.names.map(identifier => {
            val name = identifier.name match {
                case Some(identifierName) => identifierName
                case None => Defines.Unknown
            }
            val typeName = field.typeExpression match {
                case Some(typeExpression) => getTypeFullNameFromExpression(typeExpression)
                case None => Defines.Unknown
            }
            val member = memberNode(
                field, name, field.code, typeName
            )
            Ast(member)
        }).toSeq
    }
    
    //  TODO: Need implements correctly
    protected override def code(node: io.joern.gosrc2cpg.ast.nodes.Node): String = "Ignore"

    protected def column(node: io.joern.gosrc2cpg.ast.nodes.Node): Option[Integer] = Option(0)

    protected def columnEnd(node: Node): Option[Integer] = Option(0)

    protected def line(node: io.joern.gosrc2cpg.ast.nodes.Node): Option[Integer] = Option(0)

    protected def lineEnd(node: io.joern.gosrc2cpg.ast.nodes.Node): Option[Integer] = Option(0)


}
