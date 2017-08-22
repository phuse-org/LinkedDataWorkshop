// 101-Study1Participants.cql
MATCH enrollGraph = (PERSON)-[:enrolledin]->(STUDY)
WHERE STUDY.name='STUDY1'
RETURN PERSON.firstname AS STUDY1Participants