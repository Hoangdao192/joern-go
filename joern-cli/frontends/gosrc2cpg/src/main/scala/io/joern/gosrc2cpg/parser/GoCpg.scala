package io.joern.gosrc2cpg.parser

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.Frontend.defaultConfig
import io.joern.gosrc2cpg.passes.AstCreationPass
import io.joern.x2cpg.utils.Report
import io.joern.x2cpg.{X2Cpg, X2CpgFrontend}
import io.shiftleft.codepropertygraph.Cpg

import java.io.File
import scala.util.Try

class GoCpg extends X2CpgFrontend[Config] {

  private val report: Report = new Report()

  override def createCpg(config: Config): Try[Cpg] = {
    val inputFile = new File(config.inputPath)
    if (!inputFile.isDirectory && !inputFile.isFile) {
      throw new IllegalArgumentException(s"$inputFile is not a valid directory or file.")
    }

    X2Cpg.withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      better.files.File.usingTemporaryDirectory("gosrccpg_tmp") { tempWorkingDir =>
        val astCreationPass = new AstCreationPass(cpg, config, tempWorkingDir.pathAsString, report)
        astCreationPass.createAndApply()
      }
    }
  }

}
