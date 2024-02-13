package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.nodes.{FileNode, FunctionDeclaration, Identifier, Node}
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class AstCreator(rootNode: FileNode, filename: String) (implicit val validationMode: ValidationMode)
  extends AstCreatorBase(filename)
  with AstForFunctionsCreator 
    with AstForExpressionCreator 
    with AstForStatementCreator
  with AstNodeBuilder[Node, AstCreator] {

  def createAst(): DiffGraphBuilder = {
    val ast = astForGoAstNode(rootNode)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def astForGoAstNode(node: Node): Ast = {
    var ast: Ast = node match {
      case fileNode: FileNode =>
        var childrenAst = ListBuffer[Ast]()
        for (decl <- fileNode.declarations) {
          childrenAst.addOne(astForGoAstNode(decl))
        }
        if (fileNode.name.isDefined) {
          var packageIdentifier = fileNode.name
          astForPackageNode(filename, packageIdentifier, childrenAst.toList)
        } else {
          Ast()
        }
      case functionDecl: FunctionDeclaration =>
        astForFunctionDeclaration(filename, functionDecl)
      case _ =>
        Ast()
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
