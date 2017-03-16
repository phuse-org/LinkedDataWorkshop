//102-MargeSon.cql
MATCH (pers1)-[:hasSon]-(pers2)
WHERE pers1.firstName='Marge'
RETURN pers2.firstName