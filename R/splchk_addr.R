#' The \code{splchk_addr} function performs a spell check on a data frame of NYC addresses with a street name dictionary built from NYC Department of City Planning's (DCP) PAD (Property Address Directory) and SND (Street Name Dictionary) files.    
#'
#' @title Spell check a data frame of NYC addresses.
#' @name splchk_addr
#' @import data.table
#' @export splchk_addr
#' @param in_df a data frame containing NYC addresses.  Required.
#' @param addr_col_name the name of the input addresses column as string.  Required.
#' @param third_col_name the name of either the borough code or zip code column as string.  Required.
#' @param new_addr_col_name new_addr_col_name the name of output addresses column as string.  Required.
#' @param third_col_type either \code{"boro_code"} or \code{"zip_code"} as string.  Required.
#' @return A data frame containing the input data frame plus the spell checked address column.
#' @usage splchk_addr(in_df, new_addr_col_name, addr_col_name,
#'     third_col_name, third_col_type)
#' @examples # create a data frame of addresses
#' ADDR <- c("1212 AMESTERDAM AVEN","253 BROADWY",
#'     "250 BREDFORD PORK BLVD W","30 LAFAYET AVE")
#' CITY <- c("NEW YORK","NEW YORK","BRONX","BROOKLYN")
#' STATE <- rep("NY",length(ADDR))
#' ZIP_CODE <- c("10027","10007","10468","11217")
#' u_id <- 1:length(ADDR)
#' df = data.frame(u_id, ADDR, CITY, STATE, ZIP_CODE)
#'
#' #spell check address column using zip code
#' df1 <- splchk_addr(df,"ADDR.splchk","ADDR","ZIP_CODE","zip_code")
#' 
#' #preview records
#' head(df1)

splchk_addr <- function(in_df, new_addr_col_name, addr_col_name, third_col_name, third_col_type){

	###detect if data.table or data.frame###
	is.DT <- "data.table" %in% class(in_df)
	
	###if data.frame, convert to data.table###
	if(!is.DT) in_df <- as.data.table(in_df)

	###more efficient if we remove duplicates###
	temp_df <- unique(in_df[,c(addr_col_name,third_col_name),with=FALSE])
	
	temp_df[,temp.name := vector_splchk(get(addr_col_name),get(third_col_name),third_col_type)]
	
	setnames(temp_df,c("temp.name"),c(new_addr_col_name))
	
	out_df <- merge(x=in_df, y=temp_df, by=c(addr_col_name,third_col_name))
	
	invisible(gc())
	
	#if input was a data.frame, return to that state
	if(!is.DT) out_df <- as.data.frame(out_df)

	return(out_df)
	
}