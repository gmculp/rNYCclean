#' The \code{parallel.seqsplt_addr} function is a more efficient way to split address strings in a large data frame (+10,000 records) into sequential combinations of words using parallel processing.
#'
#' @title Split address strings in a large data frame into sequential combinations of words.
#' @name parallel.seqsplt_addr
#' @import parallel
#' @import data.table
#' @export parallel.seqsplt_addr
#' @param in_clus the number of clusters available to the function as integer.  Required.
#' @param in_df a data frame containing addresses.  Required.
#' @param id_col_name the name of the unique identifier column as string.  Required.
#' @param addr_col_name the name of the input addresses column as string.  Required.
#' @param third_col_name the name of either the borough code or zip code column as string.  Required. 
#' @param new_addr_col_name the name of output addresses column as string.  Required.
#' @param remove_orig option to exclude original address from output as binary.  Optional.
#' @return A data frame containing \code{id_col_name}, \code{third_col_name}, and a column of address strings split into sequential combinations of words.
#' @examples # create a data frame of addresses
#' ADDR <- c("ROOM 326 125 WORTH STREET","253 BROADWAY FLR 3",
#'     "C/O DOHMH 42-09 28 STREET")
#' BORO_CODE <- c(1,1,4)
#' u_id <- 1:length(ADDR)
#' df = data.frame(u_id, ADDR, BORO_CODE)
#'
#' #split address column into sequential combinations 
#' df1 <- parallel.seqsplt_addr(in_clus = 1,in_df = df, 
#'     new_addr_col_name = "ADDR.seqsplt", id_col_name = "u_id", 
#'     addr_col_name = "ADDR", third_col_name = "BORO_CODE")
#'
#' #preview records
#' head(df1)


parallel.seqsplt_addr <- function(in_clus, in_df, new_addr_col_name, id_col_name, addr_col_name, third_col_name, remove_orig=TRUE){

	###algorithm to limit clusters###
	in_clus <- min(parallel::detectCores(),20,in_clus,as.integer(nrow(in_df)/1000))

	###only use parallel processing if the data frame is large enough... >1000 records/core###
	if (in_clus <= 1) {
	
		out_df <- seqsplt_addr(in_df, new_addr_col_name, id_col_name, addr_col_name, third_col_name, remove_orig)
		
	} else {
	
		###detect if data.table or data.frame###
		is.DT <- "data.table" %in% class(in_df)
		
		###if data.frame, convert to data.table###
		if(!is.DT) in_df <- as.data.table(in_df)
	
		n.r <- nrow(in_df)	
			
		in_df[,pc := rep(1:in_clus,each=(ceiling(n.r/in_clus)))[1:n.r]]	

		if(.Platform$OS.type != "windows"){
			
			out_df <- data.table::rbindlist(parallel::mclapply(1:in_clus, function(zz) 
				seqsplt_addr(in_df[pc==zz,], new_addr_col_name, id_col_name, addr_col_name, third_col_name, remove_orig), 
				mc.cores=in_clus))
		
		} else {
			clus <- parallel::makeCluster(in_clus)

			#load desired version/release of rGBAT package
			parallel::clusterCall(clus, function() suppressMessages(library("rNYCclean", character.only = TRUE)))	

			parallel::clusterExport(clus, c("in_clus","in_df","id_col_name",'addr_col_name','third_col_name','new_addr_col_name','remove_orig'), envir=environment())
			
			out_df <- data.table::rbindlist(parallel::parLapply(clus, 1:in_clus, function(zz) 
				seqsplt_addr(in_df[pc==zz,], new_addr_col_name, id_col_name, addr_col_name, third_col_name, remove_orig)
			))

			#terminate cluster
			parallel::stopCluster(clus)
		}
		
		out_df[,pc := NULL]
		
		if(!is.DT) out_df <- as.data.frame(out_df)
	}
	
	invisible(gc())
	return(out_df)
}	