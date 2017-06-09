// 103-BobTreatmentValue.cql
MATCH a = (person)-[:hasTreatment]->(treat)
WHERE person.firstName='Bob'
RETURN person.firstName AS Name, treat.label as Treatment, treat.description AS Description