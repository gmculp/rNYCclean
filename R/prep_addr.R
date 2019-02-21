#' The \code{prep_addr} function removes potentially illegal characters from a vector or column of addresses leaving only letters, numbers, hyphens, and spaces remaining.
#'
#' @title Remove potentially illegal characters from a vector or column of addresses.
#' @name prep_addr
#' @export prep_addr
#' @param addr_vector vector or data frame column of input addresses.  Required.
#' @return A vector or column of addresses stripped of potentially illegal characters leaving only letters, numbers, hyphens, and spaces remaining.
#' @examples # create a data frame of addresses
#' FAC_NAME <- c("DOHMH","DOHMH","DOHMH","LEHMAN COLLEGE","BAM","DOHMH")
#' ADDR1 <- c("80 CENTER' ST","125 WORTH_","42-09 28 ST.",
#'     "250 BEDFORD P@RK BLVD W","#30 LAFAYETTE AVE","125~")
#' ADDR2 <- c("","UNIT #329","1$ FLR","212","ROOM 3","WORTH STREET")
#' BORO_CODE <- rep(1,length(ADDR1))
#' u_id <- 1:length(ADDR1)
#' df = data.frame(u_id, FAC_NAME, ADDR1, ADDR2, BORO_CODE)
#'
#' #remove illegal characters from address column
#' df$ADDR1.prep <- prep_addr(df$ADDR1)
#' df$ADDR2.prep <- prep_addr(df$ADDR2)
#'
#' #preview records
#' head(df)

prep_addr <- function(addr_vector){
	
	pat_1 <- "[^/A-Za-z0-9 \\-]"
	
	pat_2 <- "(?<=[\\s])\\s*|^\\s+|\\s+$"

	return(gsub(pat_2, "", toupper(as.character(gsub(pat_1,"",addr_vector))), perl=TRUE))
}