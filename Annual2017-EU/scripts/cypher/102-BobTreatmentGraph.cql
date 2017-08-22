// 102-BobTreatmentGraph.cql
MATCH treatGraph = (PERSON)-[:treatment]->(TREAT)
WHERE PERSON.firstname='Bob'
RETURN treatGraph
