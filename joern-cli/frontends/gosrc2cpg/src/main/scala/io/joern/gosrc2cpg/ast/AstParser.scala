package io.joern.gosrc2cpg.ast

import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

class AstParser {

  val command = "./goparser"
  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  def runAstParserCLI(inputDirectory: String, outputDirectory: String): Unit = {
    val output = ExternalCommand.run(s"$command $inputDirectory $outputDirectory", ".")
    output.foreach(strs => {
      strs.foreach(str => logger.info("[GoParser] {}", str))
    })
  }

}
