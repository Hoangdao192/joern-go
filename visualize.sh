#!/bin/bash

inputCpg=$0
outputDir=$1

# Generate import file
joern-export --repr=all --format=neo4jcsv --out=outputDir inputCpg

# Remove old import file
docker exec neo4j bash -c "rm -rf /var/lib/neo4j/import/*"
# Remove old graph
docker exec neo4j bash -c "cypher-shell -u neo4j -p 12345678 -d neo4j \"MATCH (n) DETACH DELETE n\""

docker cp outputDir/. neo4j:/var/lib/neo4j/import

find /var/lib/neo4j/import/ -name 'nodes_*_cypher.csv' -exec cypher-shell -u neo4j -p 12345678 -d neo4j --file {} \;
find /var/lib/neo4j/import/ -name 'edges_*_cypher.csv' -exec cypher-shell -u neo4j -p 12345678 -d neo4j --file {} \;



