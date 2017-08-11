// 101-Study1Participants.cql
MATCH a = (person)-[:enrolledIn]->(study)
WHERE study.name='Study1'
RETURN person.firstName AS Study1Participants