####helper function for spell checking a vector of addresses using a vector of either zip codes or borough codes###

vector_splchk <- function(addr_col,third_col,third_col_type){

	###determine third column type for look-up table###
	if(grepl("zip",third_col_type,ignore.case=TRUE)){
		sorted_words <- split(unname(zipcode_words_vector),names(zipcode_words_vector))
	} else{
		sorted_words <- split(unname(borocode_words_vector),names(borocode_words_vector))
	}
	
	
	correct_vector <- sapply(1:length(addr_col),
		function(i) {
			sorted_wordz <- sorted_words[[as.character(third_col[i])]]
			return(sapply(list(sapply(unlist(strsplit(as.character(addr_col[i]), " ")),function(z) splchk(toupper(z), sorted_wordz))), paste, collapse = " "))
	})
	
	return(correct_vector)
}