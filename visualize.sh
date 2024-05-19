#!/bin/bash

inputProject=$1
database=$2
tool=$3

echo "Input cpg: $inputProject"
echo "Database: $database"
echo "Tool: $tool"

outputDir=/tmp/visualize_$(date +%s)

mkdir $outputDir

echo "$outputDir"

echo "Parsing"
if [ "$tool" = "joern" ]; then
  /opt/joern/joern-cli/joern-parse -o "$outputDir/cpg.bin" "$inputProject"
else
  /home/hoangdao/Workspace/Scala/Joern/joern-cli/target/universal/stage/joern-parse -o "$outputDir/cpg.bin" "$inputProject"
fi

# Generate import file
#echo "Exporting"
joern-export --repr=all --format=neo4jcsv --out="$outputDir/csv" "$outputDir/cpg.bin"

 # Remove old import file
  docker exec neo4j bash -c "rm -rf /var/lib/neo4j/import/*"
 # Remove old graph
  docker exec neo4j bash -c "cypher-shell -u neo4j -p 12345678 -d $database \"MATCH (n) DETACH DELETE n\""

  docker cp "$outputDir"/csv/. neo4j:/var/lib/neo4j/import

  rm -rf "$outputDir"

  docker exec neo4j bash -c "find /var/lib/neo4j/import/ -name 'nodes_*_cypher.csv' -exec cypher-shell -u neo4j -p 12345678 -d $database --file {} \;"
  docker exec neo4j bash -c "find /var/lib/neo4j/import/ -name 'edges_*_cypher.csv' -exec cypher-shell -u neo4j -p 12345678 -d $database --file {} \;"



