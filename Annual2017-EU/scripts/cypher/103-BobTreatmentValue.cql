// 103-BobTreatmentValue.cql
MATCH a = (PERSON)-[:treatment]->(TREAT)
WHERE PERSON.firstname='Bob'
RETURN PERSON.firstname AS Name, TREAT.label as Treatment, TREAT.description AS Description