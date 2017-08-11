// 102-BobTreatmentGraph.cql
MATCH a = (person)-[:treatment]->(treat)
WHERE person.firstName='Bob'
RETURN a
