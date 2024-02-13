package io.joern.gosrccpg.astcreation

import io.joern.x2cpg.ValidationMode
import io.joern.gosrccpg.ast.nodes.Expression
import io.joern.x2cpg.Ast

trait AstForExpressionCreator(implicit validationMode: ValidationMode) { this: AstCreator =>
  
  def astForExpression(fileName: String, expression: Expression): Ast = {
    Ast()
  }

}
