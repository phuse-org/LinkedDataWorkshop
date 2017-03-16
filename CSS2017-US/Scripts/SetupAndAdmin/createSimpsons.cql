//Create Simpsons Graph
// Inefficient but easily explained. 
CREATE (p1:Person{
           firstName:"Marge",
           lastName:"Simpson",
           gender: "Female"})
-[:hasDaughter{DeliveredBy:"Dr. Hibbert"}]->
(p2:Person{firstName:"Maggie",
           lastName:"Simpson",
           gender:"Female"})

WITH count(*) as dummy        

CREATE (p3:Person{firstName:"Lisa",
           lastName:"Simpson",
           gender: "Female"})

WITH count(*) as dummy   

MATCH (a), (b)
WHERE a.firstName="Marge" AND b.firstName="Lisa"
CREATE (a)-[:hasDaughter]->(b)

WITH count(*) as dummy   
CREATE (p3:Person{firstName:"Bart",
           lastName:"Simpson",
           gender: "Male"})

WITH count(*) as dummy   

MATCH (a), (b)
WHERE a.firstName="Marge" AND b.firstName="Bart"
CREATE (a)-[:hasSon]->(b)

