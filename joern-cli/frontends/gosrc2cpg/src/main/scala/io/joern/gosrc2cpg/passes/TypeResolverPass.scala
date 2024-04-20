package io.joern.gosrc2cpg.passes

import io.shiftleft.passes.CpgPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewType
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.language.*

class TypeResolverPass(cpg: Cpg, usedTypes: Array[String]) extends CpgPass(cpg) {
    
    override def run(diffGraph: DiffGraphBuilder): Unit = {
        usedTypes.foreach { typeName =>
            var shortName = typeName
            if (shortName.contains(".")) {
                val segments: Array[String] = shortName.split("\\.")
                shortName = segments.last
            }
            val node = NewType()
                .name(shortName)
                .fullName(typeName)
                .typeDeclFullName(typeName)
            diffGraph.addNode(node)
        }
    }
    
}
