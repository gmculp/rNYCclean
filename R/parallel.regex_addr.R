#' The \code{parallel.regex_addr} function is a more efficient way to perform string replacement cleaning on a large data frame of NYC addresses (+10,000 records) with a look-up dataset of locations using parallel processing.  The locations dataset was constructed from NYC Department of City Planning's (DCP) PAD (Property Address Directory) and SND (Street Name Dictionary).  In addition, the function attempts to reconcile addresses containing post office box information or indicators of missing addresses (e.g., "UNKNOWN", "HOMELESS").  
#'
#' @title String replacement cleaning of a large data frame of NYC addresses.
#' @name parallel.regex_addr
#' @import parallel
#' @import data.table
#' @export parallel.regex_addr
#' @param in_clus the number of clusters available to the function as integer.  Required.
#' @param in_df a data frame containing NYC addresses.  Required.
#' @param new_addr_col_name the name of output addresses column as string.  Required.
#' @param addr1_col_name the name of the input address line one column as string.  Required.
#' @param addr2_col_name the name of the input address line two column as string.  Optional.
#' @return A data frame containing the input data frame plus the cleaned address column.
#' @usage parallel.regex_addr(in_clus, in_df, new_addr_col_name,
#'     addr2_col_name = NULL)
#' @examples # create a data frame of addresses
#' ADDR1 <- c(paste(1:5000,"BROADWAY"),paste(1:2400,"1"),
#'     paste(1:3400,"ATLANTIC A"), paste(1:3400,"FULTON S"), paste(1:4000,"NOSTRA"))
#' ADDR2 <- ifelse(grepl(" 1$",ADDR1),"AVE","ROOM 123")
#' BORO_CODE <- ifelse(grepl("ATLANT|FULTON|NOSTRA",ADDR1),3,1)
#' u_id <- 1:length(ADDR1)
#' df = data.frame(u_id, ADDR1, ADDR2, BORO_CODE)
#' 
#' #get version of DCP PAD used to build package data
#' rNYCclean::pad_version
#'
#' #get number of records
#' nrow(df)
#'
#' #one address input column
#' system.time({df1 <- parallel.regex_addr(in_clus=2, in_df = df, 
#'     new_addr_col_name = "ADDR.regex", addr1_col_name = "ADDR1")})
#'
#' #preview records
#' head(df1)
#' 
#' #two address input column
#' system.time({df2 <- parallel.regex_addr(in_clus=2, in_df = df, 
#'     new_addr_col_name = "ADDR.regex", addr1_col_name = "ADDR1", 
#'     addr2_col_name = "ADDR2")})
#'
#' #preview records
#' head(df2)

parallel.regex_addr <- function(in_clus, in_df, new_addr_col_name, addr1_col_name, addr2_col_name=NULL){

	if(!(missing(addr1_col_name))){
		###algorithm to limit clusters###
		in_clus <- min(parallel::detectCores()-1,in_clus,as.integer(nrow(in_df)/1000))

		###only use parallel processing if the data frame is large enough... >1000 records/core###
		if (in_clus <= 1) {
		
			out_df <- regex_addr(in_df, new_addr_col_name, addr1_col_name, addr2_col_name)
			
		} else {
		
			###detect if data.table or data.frame###
			is.DT <- "data.table" %in% class(in_df)
		
			###if data.frame, convert to data.table###
			if(!is.DT) in_df <- as.data.table(in_df)
		
			n.r <- nrow(in_df)	
			
			in_df[,pc := rep(1:in_clus,each=(ceiling(n.r/in_clus)))[1:n.r]]		

			if(.Platform$OS.type != "windows"){
				
				out_df <- data.table::rbindlist(parallel::mclapply(1:in_clus, function(zz) 
				regex_addr(in_df[pc==zz,], new_addr_col_name, addr1_col_name, addr2_col_name),mc.cores=in_clus))
			
			} else {
			
				clus <- parallel::makeCluster(in_clus)

				#load desired version/release of rGBAT package
				parallel::clusterCall(clus, function() suppressMessages(library("rNYCclean", character.only = TRUE)))	

				parallel::clusterExport(clus, c("in_clus","in_df","new_addr_col_name",'addr1_col_name','addr2_col_name'), envir=environment())
				
				out_df <- data.table::rbindlist(parallel::parLapply(clus, 1:in_clus, function(zz) 
				regex_addr(in_df[pc==zz,], new_addr_col_name, addr1_col_name, addr2_col_name)))

				#terminate cluster
				parallel::stopCluster(clus)
			}
			
			out_df[,pc := NULL]
			
			if(!is.DT) out_df <- as.data.frame(out_df)
		}
		
		invisible(gc())
		return(out_df)
	} else{
		stop("Please check you column names.")
	}
}	