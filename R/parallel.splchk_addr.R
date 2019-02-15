#' The \code{parallel.splchk_addr} function is a more efficient way to perform a spell check on a large data frame of NYC addresses (+10,000 records) with a street name dictionary built from NYC Department of City Planning's (DCP) PAD (Property Address Directory) and SND (Street Name Dictionary) using parallel processing.
#'
#' @title Spell check an extremely large data frame of NYC addresses.
#' @name parallel.splchk_addr
#' @import parallel
#' @import data.table
#' @export parallel.splchk_addr
#' @param in_clus the number of clusters available to the function as integer.  Required.
#' @param in_df a data frame containing NYC addresses.  Required.
#' @param addr_col_name the name of the input addresses column as string.  Required.
#' @param third_col_name the name of either the borough code or zip code column as string.  Required.
#' @param new_addr_col_name the name of output addresses column as string.  Required.
#' @param third_col_type either \code{"boro_code"} or \code{"zip_code"} as string.  Required.
#' @return A data frame containing the input data frame plus the spell checked address column.
#' @examples # create a data frame of addresses
#' ADDR <- c(paste(1:5000,"BRODWAY"),paste(1:2400,"1 AVNUE"),
#'     paste(1:3400,"ATLANTAC AVE"),paste(1:3400,"FULTAIN ST"))
#' BORO_CODE <- ifelse(grepl("ATLANT|FULT",ADDR),3,1)
#' u_id <- 1:length(ADDR)
#' df = data.frame(u_id, ADDR, BORO_CODE)
#'
#' #spell check address column using borough code
#' df1 <- parallel.splchk_addr(in_clus = 10, in_df = df,   
#'     new_addr_col_name="ADDR.splchk", addr_col_name="ADDR", 
#'     third_col_name="BORO_CODE", third_col_type="boro_code")



parallel.splchk_addr <- function(in_clus, in_df, new_addr_col_name, addr_col_name, third_col_name, third_col_type){

	
	###algorithm to limit clusters###
	in_clus <- min(parallel::detectCores()-1,20,in_clus,as.integer(nrow(in_df)/1000))

	###only use parallel processing if the data frame is large enough... >1000 records/core###
	if (in_clus <= 1) {
	
		out_df <- splchk_addr(in_df, new_addr_col_name, addr_col_name, third_col_name, third_col_type)
		
	} else {
	
		###detect if data.table or data.frame###
		is.DT <- "data.table" %in% class(in_df)
		
		###if data.frame, convert to data.table###
		if(!is.DT) in_df <- as.data.table(in_df)
	
		n.r <- nrow(in_df)	

		in_df[,pc := rep(1:in_clus,each=(ceiling(n.r/in_clus)))[1:n.r]]

		if(.Platform$OS.type != "windows"){
			
			out_df <- data.table::rbindlist(parallel::mclapply(1:in_clus, function(zz) 
			splchk_addr(in_df[pc==zz,], new_addr_col_name, addr_col_name, third_col_name, third_col_type),mc.cores=in_clus))
		
		} else {
			clus <- parallel::makeCluster(in_clus)

			#load this package to access other functions and data
			parallel::clusterCall(clus, function() suppressMessages(library("rNYCclean", character.only = TRUE)))	

			parallel::clusterExport(clus, c("in_clus", "in_df", "addr_col_name","third_col_name","new_addr_col_name","third_col_type"), envir=environment())

			out_df <- data.table::rbindlist(parallel::parLapply(clus, 1:in_clus, function(zz) 
			splchk_addr(in_df[pc==zz,], new_addr_col_name, addr_col_name, third_col_name, third_col_type)))
			
			#terminate cluster
			parallel::stopCluster(clus)
		}
		
		out_df[,pc := NULL]
		
		if(!is.DT) out_df <- as.data.frame(out_df)
	}
	
	invisible(gc())
	return(out_df)
	
}	