//106-CreateRel-HomerSon.cql
MATCH (person1), (person2)
WHERE person1.firstName="Homer" AND
     person2.firstName="Bart"
CREATE (person1)-[:hasSon]->(person2);