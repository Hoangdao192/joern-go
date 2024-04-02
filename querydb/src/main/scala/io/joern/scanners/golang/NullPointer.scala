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
import io.shiftleft.codepropertygraph.generated.nodes.*
import scala.collection.mutable.ListBuffer

object NullPointer extends QueryBundle {
    implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
    implicit val resolver: ICallResolver = NoResolve

    @q
    def shellExec()(implicit context: EngineContext): Query =
        Query.make(
            name = "test-go",
            author = Crew.niko,
                title = "Field access may produce null pointer",
            description = """
                            |An attacker controlled parameter is used in an insecure `shell-exec` call.
                            |
                            |If the parameter is not validated and sanitized, this is a remote code execution.
                            |""".stripMargin,
            score = 5,
            withStrRep(cpg => {
                println("Null Pointer check")
                val uncheckedCall = ListBuffer[Call]()
                //  Methods that use pointer as parameter
                val methods = cpg.method.filter(method => method.parameter.typeFullName("^\\*.*").nonEmpty).l
                println(methods.length)
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
                        }).l
                    if (calls.nonEmpty) {
                        println(s"Method ${method.name} ${method.filename}")
                    }
                    calls.foreach(call => {
                        val callArguments = call.argument.argumentIndex(1)
                            .filter(e => e.isInstanceOf[Identifier])
                            .map(e => e.asInstanceOf[Identifier].name)
                            .l
                        callArguments.foreach(item => println(item))
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
                                            case _ => {}
                                        }
                                    case _ => {}
                                }
                                if (!isCheckNull) {
                                    astParent = astParent.astParent
                                }
                            }
                        } catch {
                            case e: NoSuchElementException => e.printStackTrace()
                            case nullPointer: NullPointerException => nullPointer.printStackTrace()
                            case other => other.printStackTrace()
                        }
                        if (!isCheckNull) {
                            uncheckedCall.addOne(call)
                        }
                    })
                }
                println(uncheckedCall.size)
                uncheckedCall.toList.iter
            }),
            tags = List(QueryTags.remoteCodeExecution, QueryTags.default)
        )

}
