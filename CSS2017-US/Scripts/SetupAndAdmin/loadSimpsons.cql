//simpsons.csv must be loaded from within the /import folder of the Neo4j database location.
//simpsons.csv NOT used in the workshop: createSimpsons.cql was used to create the graph.
// This script provided as an example of LOAD CSV

LOAD CSV WITH HEADERS FROM 'file:///simpsons.csv' AS row
    WITH row, SPLIT(row.daughter, "|") AS daughters
    UNWIND daughters AS daughterName
    //RETURN row.firstName, daughterName;
    //RETURN row;
   
MERGE (person:Person {
    persID: row.id,
    firstName: row.firstName,
    lastName:  row.lastName,
    gender:    row.gender
    })

MERGE (person)-[:hasDaughter]->(daugtherName)
MERGE (person)-[:hasSon]->(son);
