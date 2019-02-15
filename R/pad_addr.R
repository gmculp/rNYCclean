#' The \code{pad_addr} function performs a substring match between a data frame of NYC addresses and NYC Department of City Planning's (DCP) PAD (Property Address Directory) and returns the PAD address, if available.  
#'
#' @title NYC DCP PAD address match on a data frame of NYC addresses.
#' @name pad_addr
#' @aliases pad_addr
#' @import data.table
#' @export pad_addr
#' @param in_df a data frame containing NYC addresses.  Required.
#' @param addr_col_name the name of the input addresses column as string.  Required.
#' @param third_col_name the name of either the borough code or zip code column as string.  Required.
#' @param new_addr_col_name the name of the output addresses column as string.  Required.
#' @param third_col_type either \code{"boro_code"} or \code{"zip_code"} as string.  Required.
#' @param return_type option to exclude address which failed to match from output as string.  Optional.
#' @return A data frame containing the input data frame plus the PAD address column.
#' @usage pad_addr(in_df, new_addr_col_name, addr_col_name, third_col_name, 
#'     third_col_type, return_type = "all")
#' @examples # create a data frame of addresses
#' ADDR <- c("80 CENTRE","125 WORTH S","42 09 28 S","253 BROADW",
#'     "620 ATLANT","125 WOR","1 FRANKLIN","1 FRANKLIN",
#'     "1 1 1 AVE","1 1 1 AVE")
#' BORO_CODE <- c(1,1,4,1,3,1,3,3,1,1)
#' ZIP_CODE <- c('10013','10013','11101','10007','11217','10013',
#'     '11222','11249','10003','10014')
#' u_id <- 1:length(ADDR)
#' df = data.frame(u_id, ADDR, BORO_CODE, ZIP_CODE)
#'
#' #get PAD address using borough code
#' df1 <- pad_addr(df,"ADDR.pad","ADDR","BORO_CODE","boro_code")
#'
#' #get PAD address using zip code
#' df2 <- pad_addr(df,"ADDR.pad","ADDR","ZIP_CODE","zip_code")


pad_addr <- function(in_df, new_addr_col_name, addr_col_name, third_col_name, third_col_type, return_type = "all"){

	###detect if data.table or data.frame###
	is.DT <- "data.table" %in% class(in_df)
	
	if(!is.DT) in_df <- as.data.table(in_df)
	
	o_cols <- names(in_df)
	
	###provide message if package's data are loading###
	if(!(exists("NYC_address_bank"))) cat("Package data loading...\n")
	
	if((addr_col_name %in% o_cols) & (third_col_name %in% o_cols) & !(new_addr_col_name %in% o_cols)) {
	
		###determine third column name for look-up table###
		this.col <- ifelse(grepl("zip",third_col_type,ignore.case=TRUE),'NYC_ab.zipcode','NYC_ab.boro')
	
		###remove spaces and hyphens###
		in_df[,ADDR1s := gsub("-","",toupper(gsub("\\s","",as.character(get(addr_col_name)))))]
		
		###assign third column from input data.frame as arbitrary column###
		in_df[,temp_col_123 := as.character(get(third_col_name))]
		
		###grab unique addresses###
		temp_df <- unique(in_df[,c("ADDR1s","temp_col_123")])
		
		###assign temporary unique id###
		temp_df[,temp_id_123 := .I]
		
		###produce sorted unique vector of string lengths###
		l_v <- sort(unique(nchar(temp_df$ADDR1s)))
		
		###substring merge###
		out_df <- data.table::rbindlist(lapply(l_v, function(i){
		
			###attempt to limit number of records###
			dt.x <- temp_df[nchar(temp_df$ADDR1s)==i,]
			
			###make one string column by pasting address substring and third column together###
			dt.x[,ADDR1s123 := paste(ADDR1s,temp_col_123,sep="_")]
			
			dt.y <- unique(NYC_address_bank[get(this.col) %in% dt.x$temp_col_123, c("NYC_ab.ADDR1","NYC_ab.ADDR1s",this.col), with=FALSE])
			dt.y[,NYC_ab.ADDR1s2 := substr(NYC_ab.ADDR1s, 1, i)]
			dt.y[,NYC_ab.ADDR1s := NULL]
			dt.y <- unique(dt.y)
			dt.y[,n.rows := .N, by=list(NYC_ab.ADDR1s2,get(this.col))] 
			dt.y <- dt.y[n.rows == 1]
			dt.y[,NYC_ab.ADDR1s123 := paste(NYC_ab.ADDR1s2,get(this.col),sep="_")]
			dt.x[, NYC_ab.ADDR1:= dt.y[chmatch(ADDR1s123,dt.y$NYC_ab.ADDR1s123),]$NYC_ab.ADDR1]
			return(dt.x[,c("ADDR1s","temp_col_123","NYC_ab.ADDR1"), with=FALSE])
			
		}))
		
		
		data.table::setnames(out_df, c("NYC_ab.ADDR1"), c(new_addr_col_name))
		
		all_x <- ifelse(return_type == "all",TRUE,FALSE)
		
		###merge back to original table###
		out_df <- merge(in_df,out_df,by=c("ADDR1s","temp_col_123"),all.x=all_x)
		
		###flush memory###
		invisible(gc())
		
		out_df <- out_df[,c(o_cols,new_addr_col_name),with=FALSE]
		
		if(!is.DT) out_df <- as.data.frame(out_df)
		
		return(out_df)
		
	} else {
		stop("Please check your column names.")
	}

}