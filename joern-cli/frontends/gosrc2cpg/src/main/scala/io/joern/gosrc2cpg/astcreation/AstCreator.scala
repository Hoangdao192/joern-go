package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.ast.nodes.*
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewNamespaceBlock, NewNode}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import com.fasterxml.jackson.databind.ObjectMapper
import io.joern.gosrc2cpg.ast.GoModule

import java.util
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class AstCreator(rootNode: FileNode, filename: String, goModule: GoModule)(implicit val validationMode: ValidationMode)
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

    def createAst(): DiffGraphBuilder = {
        val ast = astForTranslationUnit(rootNode)
//        val asts = astForGoAstNode(rootNode)
//        for (ast <- asts) {
//            Ast.storeInDiffGraph(ast, diffGraph)
//        }
        Ast.storeInDiffGraph(ast, diffGraph)
        diffGraph
    }

    private def astForTranslationUnit(root: FileNode): Ast = {
        val namespaceAst = root.name match {
            case Some(packageIdent) => astForPackageNode (root.filePath, root.name)
            case None => Ast(NewNamespaceBlock().name("\\").fullName(goModule.moduleName).filename(root.filePath))
        }

        namespaceStack.push(namespaceAst.nodes.head)
        
        val childrenAst = ListBuffer[Ast]()
        for (decl <- root.declarations) {
            childrenAst.addAll(astForGoAstNode(decl))
        }
        
        namespaceStack.pop()
        namespaceAst.withChildren(childrenAst)
    }

    private def astForGoAstNode(node: Node): Seq[Ast] = {
        val asts: Seq[Ast] = node match {
            case declaration: Declaration => astForDeclaration(filename, declaration)
            case statement: Statement => Seq(astForStatement(filename, statement))
            case expression: Expression => Seq(astForExpression(filename, expression))
            case specification: Specification => astForSpecification(filename, specification)
            case unknown => {
                logger.warn(s"Unknown node type")
                Seq()
            }
        }
        asts
    }

    private def astForPackageNode(filename: String,
                                  packageIdentifier: Option[Identifier]): Ast = {
        val (name, fullname) = packageIdentifier match {
            case Some(packageDecl) =>
                (packageDecl.name.get, getPackageFullname(goModule, filename, packageDecl))
            case None =>
                ("\\", goModule.moduleName)
        }
        
        var namespaceBlock = if (namespaceMap.containsKey(fullname)) {
            namespaceMap.get(fullname)
        } else {
            NewNamespaceBlock()
                .name(name)
                .fullName(fullname)
                .filename(filename)
        }
        namespaceMap.put(
            fullname, namespaceBlock
        )
        Ast(namespaceBlock)
    }

    //  TODO: Need implements correctly
    protected override def code(node: io.joern.gosrc2cpg.ast.nodes.Node): String = "Ignore"

    protected def column(node: io.joern.gosrc2cpg.ast.nodes.Node): Option[Integer] = Option(0)

    protected def columnEnd(node: Node): Option[Integer] = Option(0)

    protected def line(node: io.joern.gosrc2cpg.ast.nodes.Node): Option[Integer] = Option(0)

    protected def lineEnd(node: io.joern.gosrc2cpg.ast.nodes.Node): Option[Integer] = Option(0)


}
