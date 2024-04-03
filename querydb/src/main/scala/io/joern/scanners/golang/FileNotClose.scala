package io.joern.scanners.golang

import io.joern.console.{Query, QueryBundle, q}
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.macros.QueryMacros.*
import io.joern.scanners.{Crew, QueryTags}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable.ListBuffer

object FileNotClose extends QueryBundle {
    implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
    implicit val resolver: ICallResolver = NoResolve

    @q
    def fileNotClose()(implicit context: EngineContext): Query =
        Query.make(
            name = "file-not-close",
            author = Crew.niko,
            title = "File not close after use",
            description =
                """
                  |An attacker controlled parameter is used in an insecure `shell-exec` call.
                  |
                  |If the parameter is not validated and sanitized, this is a remote code execution.
                  |""".stripMargin,
            score = 6,
            withStrRep(cpg => {
                println("File not close check")
                val result = ListBuffer[Call]()
                try {
                    val assignmentCalls = cpg.call.name("<operator>.assignment")
                        .filter(call => {
                            val result = call.argument.exists(arg => {
                                arg match {
                                    case c: Call => c.methodFullName.equals("os.Open") || c.methodFullName.equals("os.OpenFile")
                                    case _ => false
                                }
                            })
                            if (result) {
                                println(call.code)
                            }
                            result
                        })
                        .filter(call => {
                            call.argument(1) match {
                                case identifier: Identifier =>
                                    val fileArgument = identifier.name
                                    val parentBlocks = call.parentBlock.l
                                    var isClose = false;
                                    for (parent <- parentBlocks) {
                                        val calls = parent.astChildren
                                            .filter(item => item.isInstanceOf[Call])
                                            .map(item => item.asInstanceOf[Call])
                                            .methodFullName("^os\\.Open.*\\.Close()$")
                                            .filter(item => item.argument
                                                .filter(i => i.isInstanceOf[Identifier])
                                                .map(i => i.asInstanceOf[Identifier])
                                                .exists(i => i.name.equals(fileArgument))
                                            )
                                            .l
                                        if (calls.nonEmpty) {
                                            isClose = true
                                        }
                                    }
                                    !isClose
                                case _ => true
                            }
                        })
                        .l

                    println(s"Assignment ${assignmentCalls.size}")
                    assignmentCalls.foreach(call => {
                        println("Add call")
                        result.addOne(call)
                    })
                } catch {
                    case nullPointer: NullPointerException => nullPointer.printStackTrace()
                    case e: Exception => e.printStackTrace()
                }
                println(s"Assignment ${result.size}")
                result.toList.iter
            }),
            tags = List(QueryTags.default)
        )

}
