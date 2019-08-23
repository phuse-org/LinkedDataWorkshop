# xxx.rq Information about your study at XXXX
PREFIX  ct:     <http://bio2rdf.org/clinicaltrials:>
PREFIX  ct-voc:  <http://bio2rdf.org/clinicaltrials_vocabulary:>
PREFIX  dcterms: <http://purl.org/dc/terms/>
SELECT ?study ?nctid ?title ?phase
WHERE{
  ?study eg:studyId ?nctid .

  SERVICE <http://lod.openlinksw.com/sparql>
  {
    ?nctid dcterms:title ?title   ;
           ct-voc:phase ?phaseIRI .
    ?phaseIRI dcterms:title ?phase .
  }
}

PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX ct:     <http://bio2rdf.org/clinicaltrials:>
PREFIX ct-voc: <http://bio2rdf.org/clinicaltrials_vocabulary:>
SELECT *
WHERE
{
  # SERVICE <http://clinicaltrials.bio2rdf.org/sparql>
  SERVICE <http://lod.openlinksw.com/sparql>
  {
  #   <http://bio2rdf.org/clinicaltrials:NCT00175890> <http://bio2rdf.org/clinicaltrials_vocabulary:phase> ?phaseIRI .
    ct:NCT01248468


    dcterms:title ?title ;
                    ct-voc:phase ?phaseIRI .
    ?phaseIRI dcterms:title ?phaseName .

}
}

PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX ct:     <http://bio2rdf.org/clinicaltrials:>
PREFIX ct-voc: <http://bio2rdf.org/clinicaltrials_vocabulary:>
SELECT ?title ?phaseName
WHERE
{
  SERVICE <http://clinicaltrials.bio2rdf.org/sparql>
  {
    # <http://bio2rdf.org/clinicaltrials:NCT00175890> <http://bio2rdf.org/clinicaltrials_vocabulary:phase> ?phaseIRI .
    ct:NCT02301286  dcterms:title ?title ;
                    ct-voc:phase ?phaseIRI .
    ?phaseIRI dcterms:title ?phaseName .
  } 
}

PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX ct:     <http://bio2rdf.org/clinicaltrials:>
PREFIX ct-voc: <http://bio2rdf.org/clinicaltrials_vocabulary:>
SELECT ?nctID ?intervention
WHERE
{
  # SERVICE <http://clinicaltrials.bio2rdf.org/sparql>
  SERVICE <http://lod.openlinksw.com/sparql>
  {
  ?nctID  ct-voc:intervention ?interventionIRI .
   ?interventionIRI rdf:type ct-voc:Drug .
   ?interventionIRI ct-voc:intervention-name ?intervention
   FILTER regex(?intervention, "Acetylsalicylic acid|Aspirin", "i" ) 
  } 
} LIMIT 500