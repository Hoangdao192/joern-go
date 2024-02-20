package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.nodes.*
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import com.fasterxml.jackson.databind.ObjectMapper

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class AstCreator(rootNode: FileNode, filename: String)(implicit val validationMode: ValidationMode)
    extends AstCreatorBase(filename)
        with AstCreatorHelper
        with AstForExpressionCreator
        with AstForStatementCreator
        with AstForDeclarationCreator
        with AstForSpecificationCreator
        with AstNodeBuilder[Node, AstCreator] {

    protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])
    protected val objectMapper: ObjectMapper = ObjectMapper()

    def createAst(): DiffGraphBuilder = {
        val asts = astForGoAstNode(rootNode)
        for (ast <- asts) {
            Ast.storeInDiffGraph(ast, diffGraph)
        }
        diffGraph
    }

    private def astForGoAstNode(node: Node): Seq[Ast] = {
        val asts: Seq[Ast] = node match {
            case fileNode: FileNode =>
                val childrenAst = ListBuffer[Ast]()
                for (decl <- fileNode.declarations) {
                    childrenAst.addAll(astForGoAstNode(decl))
                }
                if (fileNode.name.isDefined) {
                    val packageIdentifier = fileNode.name
                    Seq(astForPackageNode(filename, packageIdentifier, childrenAst.toList))
                } else {
                    Seq()
                }
            case declaration: Declaration => astForDeclaration(filename, declaration)
            case statement: Statement => Seq(astForStatement(filename, statement))
            case expression: Expression => Seq(astForExpression(filename, expression))
            case unknown => {
                logger.warn(s"Unknown node type")
                Seq()
            }
        }
        asts
    }

    private def astForPackageNode(filename: String,
                                  packageIdentifier: Option[Identifier],
                                  children: List[Ast] = List()): Ast = {
        val namespaceBlock = packageIdentifier match {
            case Some(packageDecl) =>
                NewNamespaceBlock()
                    .name(packageDecl.name.get)
                    .fullName(packageDecl.name.get)
                    .filename(filename)
            case None =>
                NewNamespaceBlock()
                    .name("Global")
                    .fullName("Global")
                    .filename(filename)
        }

        Ast(namespaceBlock).withChildren(children)
    }

    //  TODO: Need implements correctly
    protected override def code(node: io.joern.gosrccpg.ast.nodes.Node): String = "Ignore"

    protected def column(node: io.joern.gosrccpg.ast.nodes.Node): Option[Integer] = Option(0)

    protected def columnEnd(node: Node): Option[Integer] = Option(0)

    protected def line(node: io.joern.gosrccpg.ast.nodes.Node): Option[Integer] = Option(0)

    protected def lineEnd(node: io.joern.gosrccpg.ast.nodes.Node): Option[Integer] = Option(0)


}
