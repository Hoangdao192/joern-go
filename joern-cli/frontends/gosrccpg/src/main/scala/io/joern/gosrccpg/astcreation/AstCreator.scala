package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.nodes.*
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class AstCreator(rootNode: FileNode, filename: String)(implicit val validationMode: ValidationMode)
    extends AstCreatorBase(filename)
        with AstCreatorHelper
        with AstForFunctionsCreator
        with AstForExpressionCreator
        with AstForStatementCreator
        with AstForDeclarationCreator
        with AstForSpecificationCreator
        with AstNodeBuilder[Node, AstCreator] {

    protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

    def createAst(): DiffGraphBuilder = {
        val ast = astForGoAstNode(rootNode)
        Ast.storeInDiffGraph(ast, diffGraph)
        diffGraph
    }

    private def astForGoAstNode(node: Node): Ast = {
        val ast: Ast = node match {
            case fileNode: FileNode =>
                val childrenAst = ListBuffer[Ast]()
                for (decl <- fileNode.declarations) {
                    childrenAst.addOne(astForGoAstNode(decl))
                }
                if (fileNode.name.isDefined) {
                    val packageIdentifier = fileNode.name
                    astForPackageNode(filename, packageIdentifier, childrenAst.toList)
                } else {
                    Ast()
                }
            case functionDecl: FunctionDeclaration =>
                astForFunctionDeclaration(filename, functionDecl)
            case statement: Statement => astForStatement(filename, statement)
            case expression: Expression => astForExpression(filename, expression)
            case unknown => {
                logger.warn(s"Unknown node type")
                Ast()
            }
        }
        Ast.storeInDiffGraph(ast, diffGraph)
        ast
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
