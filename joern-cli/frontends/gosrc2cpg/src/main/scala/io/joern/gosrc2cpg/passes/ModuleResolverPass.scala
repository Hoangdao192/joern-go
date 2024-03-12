package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.ast.GoModule
import io.joern.x2cpg.Defines
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import overflowdb.BatchedUpdate

class ModuleResolverPass(cpg: Cpg, goModule: GoModule) extends CpgPass(cpg) {

    override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
        val moduleNamespace = NewNamespaceBlock()
            .name(goModule.moduleName)
            .filename("\\")
            .fullName(goModule.moduleName)
        builder.addNode(moduleNamespace)
        cpg.namespaceBlock.foreach(namespaceBlock => {
            if (!namespaceBlock.name.equals("<global>")) {
                builder.addEdge(moduleNamespace, namespaceBlock, EdgeTypes.AST)
            }
        })
    }
}
