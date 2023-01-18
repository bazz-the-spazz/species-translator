# Query wikipedia


species.translator <- function(latin, language="de", taxonomy=F ){
	require(WikipediR)
	if(language=="de"){
	  ac <-page_content(language = "de", project = "wikipedia", page_name = latin, as_wikitext = F, clean_response = F)
	  text <- ac$parse$text
	  sp<- strsplit(as.character(text), split='\\"')
	  r <- unlist(sp)[which(unlist(sp) == " title=")+1]
	}
	if(language=="en"){
		ac <-page_content(language = "en", project = "wikipedia", page_name = latin, as_wikitext = F, clean_response = F)
		text <- as.character(ac$parse$text)
    sp <- unlist(strsplit(text, split='(180,250,180)'))[2]
		sp<- substr(sp, 4, unlist(gregexpr('<', sp))[1]-2)
		r <- sp
	}

	if(taxonomy){
		ac <-page_content(language = "en", project = "wikipedia", page_name = latin, as_wikitext = F, clean_response = F)
		text <- unlist(strsplit(unlist(strsplit(as.character(ac$parse$text), "<td>Kingdom:"))[2], "Species:"))[1]
		text <- gsub('<i>',"", text)
		text <- gsub('</i>',"", text)
		text <- gsub('\n</td></tr>\n<tr>\n<td>',"", text)
		text <- unlist(strsplit(text, "</a>"))
		text <- text[-length(text)]

		l <- list(Species=r)
		l <- c(l, Latin=latin)
		for(i in 1:length(text)){
			l[[i+2]] <- substr(text[i], tail(unlist(gregexpr('>', text[i])), n=1)+1, nchar(text[i]))
			if(i ==1) names(l)[3] <- "Kingdom" else names(l)[2+i] <- substr(text[i], 1, unlist(gregexpr(':', text[i]))[1]-1)
		}

		r <- l
		}

	return(r)
}

species.translator(latin="Capsella bursa-pastoris")
species.translator(latin="Capsella bursa-pastoris", language = "en")

species.translator(latin="Capsella bursa-pastoris", taxonomy = T)
species.translator(latin="Capsella bursa-pastoris", language = "en", taxonomy = T)

species.translator(latin="Abies alba", taxonomy = T)
species.translator(latin="Abies alba", language = "en", taxonomy = T)

species.translator(latin="Leontodon autumnalis")
species.translator(latin="Leontodon autumnalis", language = "en")
