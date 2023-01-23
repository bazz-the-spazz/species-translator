# Query wikipedia


species.translator <- function(latin, language="de", taxonomy=F ){
	require(WikipediR)
	if("de" %in% language){
	  ac <-page_content(language = "de", project = "wikipedia", page_name = latin, as_wikitext = F, clean_response = F)
	  text <- ac$parse$text
	  sp<- strsplit(as.character(text), split='\\"')
	  rd <- unlist(sp)[which(unlist(sp) == " title=")+1]
	}
	if("en" %in% language){
		ac <-page_content(language = "en", project = "wikipedia", page_name = latin, as_wikitext = F, clean_response = F)
		text <- as.character(ac$parse$text)
    sp <- unlist(strsplit(text, split='(180,250,180)'))[2]
		sp<- substr(sp, 4, unlist(gregexpr('<', sp))[1]-2)
		re <- sp

		if((re==""|is.na(re))){
			ac <-page_content(language = "en", project = "wikipedia", page_name = latin, as_wikitext = F, clean_response = F)
			text <- as.character(ac$parse$text)
			sp <- unlist(strsplit(text, split='</tbody></table>'))[2]
			sp <- unlist(strsplit(sp, split='<b>'))[3]
			sp <- unlist(strsplit(sp, split='</b>'))[1]
			sp<- paste(toupper(substr(sp,1,1)),substr(sp,2,nchar(sp)), sep="")
			re <- sp
		}
	}


	if(length(language)==2) {
		r <- list(rd,re)
		names(r) <- c("de", "en")
		}
	else{
		if(language=="de") r <- rd
		if(language=="en") r <- re
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

# # Examples
# species.translator(latin="Capsella bursa-pastoris")
# species.translator(latin="Capsella bursa-pastoris", language = c("en","de"), taxonomy = T)
#
# species.translator(latin="Capsella bursa-pastoris",  language = c("en","de"), taxonomy = T)
#
# species.translator(latin="Abies alba", language = c("en","de"), taxonomy = T)
#
# species.translator(latin="Leontodon autumnalis", language = c("en","de"), taxonomy = T)
