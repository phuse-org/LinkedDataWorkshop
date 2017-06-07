// 102-BobTreatmentGraph.cql
MATCH a = (person)-[:hasTreatment]->(treat)
WHERE person.firstName='Bob'
RETURN a
