name := "benchmarks"

dependsOn(Projects.dataflowengineoss)
dependsOn(Projects.semanticcpg)
dependsOn(Projects.console)
dependsOn(Projects.x2cpg)
dependsOn(Projects.joerncli)

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % Versions.scalatest % Test)

Compile / doc / sources ~= (_ filter (_ => false))

trapExit    := false
Test / fork := true
