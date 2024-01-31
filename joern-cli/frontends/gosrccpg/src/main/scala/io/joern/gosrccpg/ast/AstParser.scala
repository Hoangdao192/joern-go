package io.joern.gosrccpg.ast

import io.joern.x2cpg.utils.ExternalCommand

class AstParser {

  val command = "./goparser"

  def runAstParserCLI(inputDirectory: String, outputDirectory: String): Unit = {
    ExternalCommand.run(s"$command $inputDirectory $outputDirectory", ".")
  }

}
