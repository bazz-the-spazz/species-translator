# species-translator
R function to translate species names from Latin to German or English with the help of Wikipedia. Also get taxonomic information from GBIF or Wikipedia.

Example: `species.translator(latin="Capsella bursa-pastoris", language = "en", taxonomy = T)`

## Known issues:

- If the species is not on Wikipedia, there will be no translation.
- Sometimes, the names are not correctly translated (examples: "Silene latifolia", "Briza media") ;-(

