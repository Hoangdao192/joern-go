package io.joern.gosrc2cpg

import io.joern.gosrc2cpg.parser.GoCpg
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import scopt.OParser

final case class Config() extends X2CpgConfig[Config]

private object Frontend {
  
    implicit val defaultConfig: Config = Config()
    
    val cmdLineParser: OParser[Unit, Config] = {
      val builder = OParser.builder[Config]
      import builder.programName
      OParser.sequence(programName("gosrc2cpg"))
    }
    
}

object Main extends X2CpgMain(Frontend.cmdLineParser, new GoCpg())(Frontend.defaultConfig) {
  
  def run(config: Config, gocpg: GoCpg): Unit = {
    gocpg.run(config)
  }
    
}
