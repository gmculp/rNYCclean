#' The \code{regex_addr} function performs string replacement cleaning on a data frame of NYC addresses with a look-up dataset of locations.  The locations dataset was constructed from NYC Department of City Planning's (DCP) PAD (Property Address Directory) and SND (Street Name Dictionary).  In addition, the function attempts to reconcile addresses containing post office box information or indicators of missing addresses (e.g., "UNKNOWN", "HOMELESS"). 
#'
#' @title String replacement cleaning of a data frame of NYC addresses.
#' @name regex_addr
#' @import data.table
#' @import stringi
#' @import stringr
#' @export regex_addr
#' @param in_df a data frame containing NYC addresses.  Required.
#' @param new_addr_col_name the name of output addresses column as string.  Required.
#' @param addr1_col_name the name of the input address line one column as string.  Required.
#' @param addr2_col_name the name of the input address line two column as string.  Optional.
#' @return A data frame containing the input data frame plus the cleaned address column.
#' @usage regex_addr(in_df, new_addr_col_name, addr1_col_name,
#'     addr2_col_name = NULL) 
#' @examples # create a data frame of addresses
#' ADDR1 <- c("80 CENTRE S","125 WORTH S","42-09 28 ST",
#'     "250 BEDFORD PARK BLV","30 LAFAYETTE A","125","1545 ATLANTIC")
#' ADDR2 <- c("","UNIT 329","1st FLR","SUITE 212B","ROOM 3","WORTH STREET","")
#' BORO_CODE <- c(rep(1,length(ADDR1)-1),3)
#' u_id <- 1:length(ADDR1)
#' df = data.frame(u_id, ADDR1, ADDR2, BORO_CODE)
#' 
#' #get version of DCP PAD used to build package data
#' rNYCclean::pad_version
#'
#' #one address input column
#' df1 <- regex_addr(in_df = df, new_addr_col_name = "regex.ADDR", 
#'     addr1_col_name = "ADDR1")
#'
#' #preview records
#' head(df1)
#' 
#' #two address input column
#' df2 <- regex_addr(in_df = df, new_addr_col_name = "regex.ADDR", 
#'     addr1_col_name = "ADDR1", addr2_col_name = "ADDR2")
#'
#' #preview records
#' head(df2)

regex_addr <- function(in_df, new_addr_col_name, addr1_col_name, addr2_col_name=NULL){
	
	###check for missing required arguments###
	if(missing(addr1_col_name)) stop("The argument addr1_col_name is required.")
	
	###detect if data.table or data.frame###
	is.DT <- "data.table" %in% class(in_df)
		
	###if data.frame, convert to data.table###
	if(!is.DT) in_df <- as.data.table(in_df)
	
	###remove illegal characters###
	in_df[,temp_addr_col := ifelse(is.na(get(addr1_col_name)),"", toupper(prep_addr(get(addr1_col_name))))]
		
	###indicate PO BOX###
	po_pat <- "(P(OST)?[[:space:]]?(-|/|\\.)?[[:space:]]?O(FFICE)?[[:space:]]?(-|/|\\.)?[[:space:]]?BO?X?|GEN.*DEL)"
	
	###indicate homeless###
	hl_pat <- "(UNDOM|HOMEL(ES)?(S)?|DOMIC|DOMOC|MICIL|H([[:space:]]|-)?O([[:space:]]|-)?M([[:space:]]|-)?E([[:space:]]|-)?L([[:space:]]|-)?E([[:space:]]|-)?S([[:space:]]|-)?S|HOMLESS|TRANSIENT)"
	
	###indicate unknown###
	na_pat <- "DO NOT|(^| )UNK(N)?(OWN)?|UNABLE|(^| )N A($| )|ADDRESS|REFUSE|NONE|UTO"
	
	###apply patterns###
	in_df[,temp_addr_col := gsub(po_pat,"#PO BOX",temp_addr_col)]
	
	###initial clean-up of address field(s)###
	if(!is.null(addr2_col_name)) {
	
		temp_ADDR2 <- as.character(in_df[,get(addr2_col_name)])

		temp_ADDR2 <- ifelse(is.na(temp_ADDR2),"",toupper(prep_addr(temp_ADDR2)))
		
		###indicate apt unit###
		temp_ADDR2 <- ifelse(grepl("^([[:digit:]]{1,}[[:alpha:]]{1,2}|[[:alpha:]]{1,2}[[:digit:]]{1,})$",temp_ADDR2),paste0("#",temp_ADDR2),temp_ADDR2)
		
		###indicate floor unit###
		temp_ADDR2 <- ifelse(grepl("(^| )FLO?O?R?( |$|[[:digit:]]{1,})",temp_ADDR2),paste0("#",temp_ADDR2),temp_ADDR2)
		
		###indicate care of###
		temp_ADDR2 <- gsub("^C(/| |)O ","#C/O ",temp_ADDR2)
		
		###indicate homeless records###
		temp_ADDR2 <- gsub(hl_pat,"#HOMELESS",temp_ADDR2)
		
		###indicate PO BOX###
		temp_ADDR2 <- gsub(po_pat,"#PO BOX",temp_ADDR2)
		
		###indicate unknown address###
		temp_ADDR2 <- gsub(na_pat,"#UNKNOWN",temp_ADDR2)
		
		###combine with primary address column###
		in_df[,temp_addr_col := paste(temp_addr_col,temp_ADDR2,sep=" ")]
		
	}
	
	###replace hyphens between letters with spaces###
	in_df[,temp_addr_col := gsub("([[:alpha:]]{1,})-([[:alpha:]]{1,})","\\1 \\2",temp_addr_col)]
	
	
	###more efficient if we remove duplicates###
	u_addr <- unique(in_df$temp_addr_col)
	
	
	###run through RegEx patterns###
	u_st_name_regex <- street_regex(u_addr,str_pat)
	u_st_name_regex <- street_regex(u_st_name_regex,str_pat2)
	
	temp_df <- data.table(a=u_addr, b=u_st_name_regex)
	
	out_df <- merge(x=in_df, y=temp_df, by.x="temp_addr_col", by.y="a")
	
	out_df[,num1 := stringi::stri_extract_first_regex(b,"[1-9]")]
	
	out_df[,num1 := ifelse(is.na(num1),"",num1)]
	
	###return first position of string text that follows###
	out_df[,new_addr_col_name := ifelse(num1=="","",stringr::str_replace(b, paste0(".*?(",num1,")"), "\\1"))]
	out_df[,new_addr_col_name := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", new_addr_col_name, perl=TRUE)]
	
	###deal with homeless records###
	out_df[,new_addr_col_name := ifelse((new_addr_col_name=="" | !grepl(" ",new_addr_col_name)| grepl(paste0("^.*",hl_pat,".*$"),new_addr_col_name)) & grepl(paste0("^.*",hl_pat,".*$"),temp_addr_col),"HOMELESS",new_addr_col_name)]
	
	###deal with PO BOXES###
	out_df[,new_addr_col_name := ifelse(!grepl(" ", new_addr_col_name) & grepl(po_pat,temp_addr_col),"PO BOX", new_addr_col_name)]
	
	###deal with unknown addresses###
	out_df[,new_addr_col_name := ifelse((new_addr_col_name=="" | !grepl(" ",new_addr_col_name)) & grepl(na_pat,temp_addr_col),"UNKNOWN",new_addr_col_name)]	
	
	out_df[,new_addr_col_name := ifelse(new_addr_col_name=="" | grepl("^[[:digit:]]{1,}$",new_addr_col_name), "UNKNOWN", new_addr_col_name)]	
	
	###deal with units###
	out_df[,new_addr_col_name := gsub("#.*$", "", new_addr_col_name)]
	out_df[,new_addr_col_name := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", new_addr_col_name, perl=TRUE)]
	
	setnames(out_df,c('new_addr_col_name'),c(new_addr_col_name))
	
	out_df[,c("b","temp_addr_col","num1") := NULL]
	
	if(!is.DT) out_df <- as.data.frame(out_df)
	
	invisible(gc())

	return(out_df)
}