package io.joern.scanners.golang

import io.joern.console.{Query, QueryBundle, q}
import io.joern.macros.QueryMacros.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.scanners.{Crew, QueryTags}
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*
import io.shiftleft.codepropertygraph.generated.Operators

object NullPointer extends QueryBundle {
    implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
    implicit val resolver: ICallResolver = NoResolve

    @q
    def shellExec()(implicit context: EngineContext): Query =
        Query.make(
            name = "test-go",
            author = Crew.niko,
            title = "This is a test query for go",
            description = """
                            |An attacker controlled parameter is used in an insecure `shell-exec` call.
                            |
                            |If the parameter is not validated and sanitized, this is a remote code execution.
                            |""".stripMargin,
            score = 5,
            withStrRep(cpg => {
                println("Query")
                cpg.method
            }),
            tags = List(QueryTags.remoteCodeExecution, QueryTags.default)
        )

}
