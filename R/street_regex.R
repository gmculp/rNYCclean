####helper function for performing string replacement cleaning of a data frame of NYC addresses###

street_regex <- function(addr_vector,this_pattern){
	
	ADDR_vector_regex_clean <- addr_vector
	
	for (i in 1:nrow(this_pattern)) {
		if (this_pattern$grep_pat[i]!=""){
			ADDR_vector_regex_clean <- ifelse(grepl(this_pattern$grep_pat[i],ADDR_vector_regex_clean), 
			gsub(this_pattern$original[i],this_pattern$replace[i],ADDR_vector_regex_clean,ignore.case=T,perl=TRUE),ADDR_vector_regex_clean)
			
		} else {
			ADDR_vector_regex_clean <- gsub(this_pattern$original[i],this_pattern$replace[i],ADDR_vector_regex_clean,ignore.case=T,perl=TRUE)
		}
	}
	
	ADDR_vector_regex_clean <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", ADDR_vector_regex_clean, perl=TRUE)
	
	return(ADDR_vector_regex_clean)
}	