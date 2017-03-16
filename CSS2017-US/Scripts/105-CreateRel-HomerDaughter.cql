//105-CreateRel-HomerDaughter.cql
MATCH (person1), (person2)
WHERE person1.firstName="Homer" AND
    (person2.firstName="Lisa" OR
     person2.firstName="Maggie")
CREATE (person1)-[:hasDaughter]->(person2);
