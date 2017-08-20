// 102-BobTreatmentGraph.cql
MATCH a = (PERSON)-[:treatment]->(TREAT)
WHERE PERSON.firstname='Bob'
RETURN a
