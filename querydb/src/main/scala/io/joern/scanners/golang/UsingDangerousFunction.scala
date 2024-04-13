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
import overflowdb.traversal.TraversalSugarExt

import java.io.{PrintWriter, StringWriter}
import scala.collection.mutable.ListBuffer

object UsingDangerousFunction extends QueryBundle {
    implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
    implicit val resolver: ICallResolver = NoResolve



    @q
    def go_2024_2610()(implicit context: EngineContext): Query =
        Query.make(
            name = "Vune",
            author = Crew.niko,
            title = "Errors returned from JSON marshaling may break template escaping in html/template",
            description = """
                            |If errors returned from MarshalJSON methods contain user controlled data,
                            |they may be used to break the contextual auto-escaping behavior of the html/template package,
                            |allowing for subsequent actions to inject unexpected content into templates.
                            |Found in: html/template@go1.21.5
                            |Fixed in: html/template@go1.21.8
                            |""".stripMargin,
            score = 5,
            withStrRep(cpg => {
                println("Go 2024 2610")
                var result = ListBuffer[Call]()
                try {
                    val calls = cpg.call.methodFullName("html/template.Template.Execute").l
                    println(s"${calls.length}")
                    calls.foreach(call =>
                        result.addOne(call)
                    )
                    println(s"${result.length}")
                } catch {
                    case e: Exception =>
                        println("Exception when invoke go_2024_2687")
                        println(e.getMessage)
                        var sw = StringWriter()
                        var pw = PrintWriter(sw)
                        e.printStackTrace(pw)
                        println(sw.toString)
                }
                result.toList.iter
            }),
            tags = List(QueryTags.remoteCodeExecution, QueryTags.default)
        )

//    @q
//    def go_2024_2687()(implicit context: EngineContext): Query =
//        Query.make(
//            name = "Vulnerability Report: GO-2024-2687",
//            author = Crew.niko,
//            title = "HTTP/2 CONTINUATION flood in net/http",
//            description = """
//                            |An attacker may cause an HTTP/2 endpoint to read arbitrary amounts of header data by sending an excessive number of CONTINUATION frames.
//                            |Maintaining HPACK state requires parsing and processing all HEADERS and CONTINUATION frames on a connection.
//                            |When a request's headers exceed MaxHeaderBytes, no memory is allocated to store the excess headers, but they are still parsed.
//                            |This permits an attacker to cause an HTTP/2 endpoint to read arbitrary amounts of header data, all associated with a request which is going to be rejected.
//                            |These headers can include Huffman-encoded data which is significantly more expensive for the receiver to decode than for an attacker to send.
//                            |The fix sets a limit on the amount of excess header frames we will process before closing a connection.
//                            |""".stripMargin,
//            score = 5,
//            withStrRep(cpg => {
//                println("Go 2024 2610")
//                var result = ListBuffer[Call]()
//                try {
//                    val calls = cpg.call.methodFullName("golang.org/x/net/http2.StreamError.Error").l
//                    println(s"${calls.length}")
//                    calls.foreach(call => result.addOne(call))
//
//                    //                        cpg.call.methodFullName("net/http.HandlerFunc.ServeHTTP") ++
//                    //                        cpg.call.methodFullName("net/http.Dir.Open") ++
//                    //                        cpg.call.methodFullName("net/http.Header.Del") ++
//                    //                        cpg.call.methodFullName("net/http.Header.Get") ++
//                    //                        cpg.call.methodFullName("net/http.Header.Set") ++
//                    //                        cpg.call.methodFullName("net/http.ListenAndServe") ++
//                    //                        cpg.call.methodFullName("net/http.ListenAndServeTLS") ++
//                    //                        cpg.call.methodFullName("net/http.Redirect") ++
//                    //                        cpg.call.methodFullName("net/http.Request.FormFile") ++
//                    //                        cpg.call.methodFullName("net/http.Request.ParseForm")
//                    println(s"${result.length}")
//                } catch {
//                    case e: Exception =>
//                        println("Exception when invoke go_2024_2687")
//                        println(e.getMessage)
//                        var sw = StringWriter()
//                        var pw = PrintWriter(sw)
//                        e.printStackTrace(pw)
//                        println(sw.toString)
//
//                }
//                result.toList.iter
//            }),
//            tags = List(QueryTags.remoteCodeExecution, QueryTags.default)
//        )


}
