package io.joern.gosrccpg.astcreation

import io.joern.gosrccpg.ast.nodes.GenericDeclaration
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.Ast
import io.joern.gosrccpg.ast.*

import scala.collection.mutable.ListBuffer

trait AstForDeclarationCreator(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>

    private def astForGenericDeclaration(fileName: String, genericDeclaration: GenericDeclaration): Seq[Ast] = {
        var asts = ListBuffer[Ast]()
        genericDeclaration.specifications.map(specification => astForSpecification(
            fileName, specification
        )).foreach(seq => asts.addAll(seq))
        asts.toList
    }

}
