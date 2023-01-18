# species-translator
Translate species names from Latin to German (maybe English) with the help of Wikipedia. Also get taxonomic information.

Example: `species.translator(latin="Capsella bursa-pastoris", language = "en", taxonomy = T)`

## Known issues:

- If the species is not on Wikipedia, there will be no translation.
- If the species is not on the English Wikipedia there will be no taxonomic information.
- Sometimes (often), the english name is not correctly extracted ;-(

