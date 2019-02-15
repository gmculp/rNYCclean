#' The \code{seqsplt_addr} function splits address strings in a data frame into sequential combinations of words.
#'
#' @title Split address strings in a data frame into sequential combinations of words.
#' @name seqsplt_addr
#' @import stringi
#' @import stringr
#' @export seqsplt_addr
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
#' df1 <- seqsplt_addr(in_df = df, new_addr_col_name = "ADDR.seqsplt",
#'     id_col_name = "u_id", addr_col_name = "ADDR", 
#'     third_col_name = "BORO_CODE")

seqsplt_addr <- function(in_df,new_addr_col_name,id_col_name,addr_col_name,third_col_name,remove_orig=TRUE){
	
	###detect if data.table or data.frame###
	is.DT <- "data.table" %in% class(in_df)
	
	###if data.frame, convert to data.table###
	if(!is.DT) in_df <- as.data.table(in_df)
	
	in_df[,temp.name := as.character(get(addr_col_name))]
	
	yy <- strsplit(in_df$temp.name, "\\s", perl = TRUE)
	
	names(yy) <- paste0(in_df$temp.name,"_")
	
	combos.yy <- lapply(yy, 
		function(z) { 
			if(length(z)>1){ 
				apply(t(combn(1:length(z), 2)), 1, function(x) paste0(z[x[1]:x[2]],collapse=" "))
			} else {
				z[1]
			}
		}
	)
	
	out_df <- data.table(ID.part=names(unlist(combos.yy)),ADDR_split=unlist(combos.yy))
	
	setnames(out_df,c("ADDR_split"),c(new_addr_col_name))
	
	out_df[,temp.name := gsub("^(.*)_.*$","\\1",ID.part)]
	
	out_df[,ID.part := NULL]

	out_df <- unique(out_df[,c("temp.name",new_addr_col_name),with=FALSE])
	
	out_df <- merge(out_df,in_df[,c(id_col_name,"temp.name",third_col_name),with=FALSE],by="temp.name",allow.cartesian=TRUE)
	
	if (remove_orig){
		out_df <- out_df[temp.name != get(new_addr_col_name)]
	}
	
	out_df[,temp.order := nchar(get(new_addr_col_name))]
	
	setorderv(out_df, c(id_col_name,"temp.order"), c(1, -1))
	
	out_df[,c('temp.name','temp.order') := NULL]
	
	if(!is.DT) out_df <- as.data.frame(out_df)
	
	return(out_df)
	
}