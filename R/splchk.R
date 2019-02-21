##SPELL CHECKER BASED ON http://www.sumsar.net/blog/2014/12/peter-norvigs-spell-checker-in-two-lines-of-r/

splchk <- function(word, sorted_words) {
  
  if(!grepl("[[:digit:]]",word) & nchar(word) > 3){
	  # Calculate the edit distance between the word and all other words in sorted_words.
	  edit_dist <- adist(word, sorted_words)
	  
	  # Calculate the minimum edit distance to find a word that exists in address with a limit of two edits.
	  min_edit_dist <- min(edit_dist, 2)
	  
	  # Generate a vector with all words with this minimum edit distance.
	  # Since sorted_words is ordered from most common to least common, the resulting
	  # vector will have the most common / probable match first.
	  proposals_by_prob <- c(sorted_words[ edit_dist <= min(edit_dist, 2)])
	  
	  # In case proposals_by_prob would be empty we append the word to be corrected...
	  proposals_by_prob <- c(proposals_by_prob, word)
	  
	  # ... and return the first / most probable word in the vector.
	  return(proposals_by_prob[1])
  } else {
		return(word)
  }
  
}