package io.joern.gosrccpg.astcreation

import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.Ast
import io.joern.gosrccpg.ast.nodes.*

import scala.collection.mutable.ListBuffer

trait AstForSpecificationCreator(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>

    def astForSpecification(fileName: String, specification: Specification): Seq[Ast] = {
        specification match {
            case importSpecification: ImportSpecification => Seq(astForImportSpecification(
                fileName, importSpecification
            ))
            case valueSpecification: ValueSpecification => astForValueSpecification(fileName, valueSpecification)
            case _ => Seq()
        }
    }

    private def astForImportSpecification(fileName: String, importSpecification: ImportSpecification): Ast = {
        val importPath = importSpecification.path.get.value
        val importAs = importSpecification.name match {
            case Some(identifier) => identifier.name.get
            case None => ""
        }

        val importNode = newImportNode(
            importSpecification.code,
            importPath,
            importAs, importSpecification.path.get
        )
        Ast(importNode)
    }

    private def astForValueSpecification(fileName: String, valueSpecification: ValueSpecification): Seq[Ast] = {
        var asts = ListBuffer[Ast]()

        val identifiers = valueSpecification.names.toArray
        val values = valueSpecification.values.toArray

        var index: Int = 0
        for (identifier <- identifiers) {
            if (values.length > index) {
                var value = values(index)
                if (value.isInstanceOf[FunctionLiteral]) {
                    asts.addOne(
                        astForExpression(fileName, value.asInstanceOf[FunctionLiteral])
                    )
                } else {
                    //  TODO: Handle type
                    val local = localNode(
                        identifier, identifier.name.get,
                        identifier.code,
                        ""
                    )
                    asts.addOne(Ast(local))
                }
                index += 1
            }
        }

        asts.toList
    }


}
