package io.joern.scanners.golang

import io.joern.scanners.*
import io.joern.console.*
import io.joern.macros.QueryMacros.*
import io.shiftleft.codepropertygraph.generated.nodes.{Call, ControlStructure, Identifier, Method}
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable.ListBuffer

object NullPointer extends QueryBundle {

    implicit val resolver: ICallResolver = NoResolve

    @q
    def checkNullPointer(): Query =
        Query.make(
            name = "null-pointer-golang",
            author = Crew.niko,
            title = "Null pointer exception",
            description =
                """
                Null pointer check
                """.stripMargin,
            score = 4,
            withStrRep({ cpg =>
                println("Null Pointer check")
                val uncheckedCall = ListBuffer[Call]()
                //  Methods that use pointer as parameter
                val methods = cpg.method.filter(method => method.parameter.typeFullName("^\\*.*").nonEmpty)
                for (method <- methods) {
                    val pointerParameters = method.parameter.typeFullName("^\\*.*").l
                    //  All field access that using pointer parameter as argument
                    val calls = method.call("<operator>.fieldAccess")
                        .filter(c => {
                            val args = c.argument.argumentIndex(1).filter(arg => 
                                arg match {
                                    case identifier: Identifier => pointerParameters.name.contains(identifier.name)
                                    case _ => false
                                }
                            )
                            args.nonEmpty
                        })
                    calls.foreach(call => {
                        val callArguments = call.argument.argumentIndex(1)
                            .filter(e => e.isInstanceOf[Identifier])
                            .map(e => e.asInstanceOf[Identifier].name)
                        var isCheckNull = false
                        try {
                            var astParent = call.astParent
                            while (!astParent.isInstanceOf[Method] && !isCheckNull) {
                                astParent match {
                                    case controlStructure: ControlStructure =>
                                        controlStructure.condition.foreach {
                                            case call: Call =>
                                                if (call.methodFullName == "<operator>.notEquals"
                                                    && call.argument.length == 2) {
                                                    var containPointer = false
                                                    var containNil = false
                                                    call.argument.foreach {
                                                        case identifier: Identifier =>
                                                            if (identifier.name.equals("nil")) {
                                                                containNil = true
                                                            } else if (callArguments.contains(identifier.name)) {
                                                                containPointer = true
                                                            }
                                                    }
                                                    if (containPointer && containNil) {
                                                        isCheckNull = true
                                                    }
                                                }
                                        }
                                }
                                if (!isCheckNull) {
                                    astParent = astParent.astParent
                                }
                            }
                        } catch {
                            case e: NoSuchElementException => //ignored
                        }
                    })
                }
                uncheckedCall.toList.iter
            }),
            tags = List(QueryTags.default)
        )

}
