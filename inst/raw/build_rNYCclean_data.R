####################################################################
###code to generate look-up tables for address cleaning functions###
####################################################################

###DISABLE SCIENTIFIC NOTATION###
options(scipen = 999)
	
###special character that may cause problems###
s.c <- c("\\","^","$",".","|","?","*","+","(",")","[","]")
s.c.p <- paste0("(",paste0("\\",s.c,collapse="|"),")")

###vector of letters###
myLetters <- toupper(letters[1:26])

###check if packages are installed###
###if not installed, install them###
packages <- c("stringi","stringr","data.table","parallel","httr")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

###load packages###
lapply(packages, require, character.only = TRUE)

###load USPS street abbreviations look-up table###
USPS_path <- system.file("raw", "USPS_abbrev.csv", package = "rNYCclean")
USPS_abbrev <- read.csv(USPS_path,stringsAsFactors=FALSE)
USPS_abbrev$original <- trimws(as.character(USPS_abbrev$original))
USPS_abbrev$replace <- trimws(as.character(USPS_abbrev$replace))
USPS_abbrev <- USPS_abbrev[USPS_abbrev$original != USPS_abbrev$replace,]

#####################
###start functions###
#####################

###this function converts release (e.g., "18d") to version (e.g., 18.4)###
r2v <- function(x){
	return(as.numeric(paste(gsub("[[:alpha:]]","",x),match(toupper(gsub("[^[:alpha:]]","",x)),myLetters),sep=".")))
}

###this function converts version (e.g., 18.4) to release (e.g., "18d")###
v2r <- function(x){	
	return(stringr::str_pad(paste0(gsub("^([[:digit:]]{1,2})\\.[[:digit:]]{1}$","\\1", as.character(x)), tolower(myLetters[as.numeric(gsub("^[[:digit:]]{1,2}\\.([[:digit:]]{1})$", "\\1", as.character(x)))])), 3, pad = "0"))
}


###this function breaks house numbers into pre hyphen, post hyphen, and letter components###
process_hnum <- function(in_dt,low_hnum_colname,high_hnum_colname){

	###main low house number part###
	in_dt[,lhnd1 := as.numeric(gsub("(-|[[:alpha:]]| ).*$","",get(low_hnum_colname)))]
	
	###hyphenated low house number part###
	in_dt[,lhnd2 := ifelse(grepl("-",get(low_hnum_colname)),as.numeric(gsub("( |[[:alpha:]]).*$","",gsub("^.*-","",get(low_hnum_colname)))),NA)]
	
	###letter low house number part###
	in_dt[,lhnd3a := gsub("[^[:alpha:]]","",get(low_hnum_colname))]
	
	###fractional low house number part###
	in_dt[,lhnd3b := ifelse(grepl(" ",get(low_hnum_colname)),gsub("[[:alpha:]]","",gsub("^.* ","",get(low_hnum_colname))),"")]
	
	###representative low house number###
	in_dt[,lhnd0 := gsub("[[:alpha:]]","",gsub(" .*$","",get(low_hnum_colname)))]
	
	###main high house number part###
	in_dt[,hhnd1 := as.numeric(gsub("(-|[[:alpha:]]| ).*$","",get(high_hnum_colname)))]
	
	###hyphenated high house number part###
	in_dt[,hhnd2 := ifelse(grepl("-",get(high_hnum_colname)),as.numeric(gsub("( |[[:alpha:]]).*$","",gsub("^.*-","",get(high_hnum_colname)))),NA)]
	
	###letter high house number part###
	in_dt[,hhnd3a := gsub("[^[:alpha:]]","",get(high_hnum_colname))]
	
	###fractional high house number part###
	in_dt[,hhnd3b := ifelse(grepl(" ",get(high_hnum_colname)),gsub("[[:alpha:]]","",gsub("^.* ","",get(high_hnum_colname))),"")]

	###representative low house number###
	in_dt[,hhnd0 := gsub("[[:alpha:]]","",gsub(" .*$","",get(high_hnum_colname)))]
	
	return(in_dt)
}

##################################################################
###this is the main function that performs the following steps:###
###1- load the required data (i.e., USPS_abbrev, PAD, and SND files)###
###2- generate the package data (i.e., street dictionaries, address look-up table, regular expression patterns)###
###3- save the data as either a lazyload database or separate RDA files###
##################################################################

build_rNYCclean_data <- function(pad_version,dest_dir,num_cores,as_rdb=TRUE) {
	
	###load street formatting regular expressions###
	str_pat_path <- system.file("raw", "street_clean_regex_patterns.csv", package = "rNYCclean")
	str_pat <- read.csv(str_pat_path,stringsAsFactors=FALSE)
	str_pat$grep_pat <- as.character(str_pat$grep_pat)
	str_pat$grep_pat <- ifelse(is.na(str_pat$grep_pat),'',str_pat$grep_pat)

	###limits to available cores###
	num_cores <- min(detectCores()-1,num_cores)

	###create temp file##
	temp <- tempfile()
	
	###clean up PAD version###
	pad_version <- as.character(pad_version)
	if(grepl("^[[:digit:]]{2}[a-d]$",tolower(pad_version),ignore.case=TRUE)) {
		pad_version <- tolower(pad_version)
	} else if (grepl("^[[:digit:]]{2}\\.[1-4]$",pad_version)) {
		pad_version <- paste0(substr(pad_version,1,2),tolower(myLetters[as.numeric(substr(pad_version,4,4))]))
	} else {
		stop("No such version of PAD exists.")
	}
	
	###get corresponding SND version###
	###this is necessary because some SND versions are absent###
	pad2snd <- get_file_versions(pad_version)
	snd_version <- pad2snd$snd.v
	
	###get correct PAD version###
	###this is necessary in case file naming convention is off (e.g., "16B" instead of "16b")###
	pad_version <- pad2snd$pad.v
	
	#############################
	###download version of PAD###
	#############################
	URL_path <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/"
	
	URL_path1 <- 'https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/'
	
	###if error when archive URL used, use current URL###
	tryCatch(download.file(paste0(URL_path,"pad",pad_version,".zip"),temp), error = function(e) download.file(paste0(URL_path1,"pad",pad_version,".zip"),temp))
	
	df_bobaadr <- fread(unzip(temp, "bobaadr.txt"), colClasses = "character")
	
	
	#############################
	###download version of SND###
	#############################
	
	#download.file(paste0(URL_path,"snd",tolower(snd_version),".zip"),temp)
	
	tryCatch(download.file(paste0(URL_path,"snd",tolower(snd_version),".zip"),temp), error = function(e) download.file(paste0(URL_path1,"snd",tolower(snd_version),".zip"),temp))
	
	df_snd <- as.data.table(read.delim(unz(temp, paste0("snd",toupper(snd_version),"cow.txt")),header=FALSE))
	
	###disconnect temp file##
	unlink(temp)
	
	#################
	###process SND###
	#################
	
	###remove header row###
	df_snd <- df_snd[2:nrow(df_snd),]
	
	df_snd[, GFT := substr(V1,51,51)] #both types... indicates type
	df_snd[, boro := substr(V1,2,2)] #both types
	df_snd[, stname := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", substr(V1,3,34), perl=TRUE)] #both types
	df_snd[, numeric_ind := substr(V1,50,50)] #both types
	df_snd[, len_full_name := substr(V1,52,53)] #both types
	
	###remove truncated street names###
	df_snd <- df_snd[GFT != "S",]
	df_snd[, primary_flag := substr(V1,35,35)]
	df_snd[, principal_flag := substr(V1,36,36)]
	df_snd[, boro2 := substr(V1,37,37)]
	df_snd[, sc5 := substr(V1,38,42)]
	df_snd[, lgc := substr(V1,43,44)]
	df_snd[, spv := substr(V1,45,47)]
	df_snd[, full_stname := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", substr(V1,54,85), perl=TRUE)] 
	df_snd[, min_SNL := substr(V1,86,87)]
	df_snd[, stn20 := substr(V1,88,107)]
	df_snd[, ht_name_type_code := substr(V1,108,108)]
	df_snd[, B7SC := substr(V1,37,44)]
	df_snd[, B10SC := substr(V1,37,47)]
	df_snd[, V1 := NULL]
	
	##################
	###process RPAD###
	##################
	
	df_bobaadr[, lhnd := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", lhnd, perl=TRUE)]
	df_bobaadr[, hhnd := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", hhnd, perl=TRUE)]
	df_bobaadr[, stname := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", stname, perl=TRUE)]
	df_bobaadr[, addrtype := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", addrtype, perl=TRUE)]
	df_bobaadr[, b7sc := substr(b10sc,1,8)]
	df_bobaadr[, lcontpar := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", lcontpar, perl=TRUE)]
	df_bobaadr[, hcontpar := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", hcontpar, perl=TRUE)]
	df_bobaadr[, bin := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", bin, perl=TRUE)]
	df_bobaadr[, bbl := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", paste0(boro,block,lot), perl=TRUE)]
	
	
	###call function to generate address bank and street name dictionaries###
	objs <- build_NYC_address_bank_and_street_dictionaries(df_bobaadr,df_snd)
	
	
	###call function to generate RegEx patterns for street names###
	str_pat2 <- build_regex_bank(df_bobaadr,df_snd,num_cores)
	
	
	
	###based on function's as_rdb argument, save as either lazy load db or individual rda files###
	###NOTE: the resulting files are huge, thus either compressing as lazy load db or splitting into multiple rda files works best###
	if(as_rdb) {
		objs[["str_pat"]] <- str_pat
		objs[["str_pat2"]] <- str_pat2
		objs[["pad_version"]] <-  pad_version
		objs[["snd_version"]] <-  snd_version
		pathz <- file.path(dest_dir, "Rdata")
		tools:::makeLazyLoadDB(objs, pathz, compress = TRUE)
		filez <- paste(pathz,c("rdb","rdx"),sep=".")
		
	} else {
		pathz1 <- file.path(dest_dir, "str_pat.rda")
		save(str_pat, file = pathz1)
		
		pathz2 <- file.path(dest_dir, "str_pat2.rda")
		save(str_pat2, file = pathz2)
		
		NYC_address_bank <- objs[["NYC_address_bank"]]
		pathz3 <- file.path(dest_dir, "NYC_address_bank.rda")
		save(NYC_address_bank, file = pathz3)
		
		borocode_words_vector <- objs[["borocode_words_vector"]]
		pathz4 <- file.path(dest_dir, "borocode_words_vector.rda")
		save(borocode_words_vector, file = pathz4)
		
		zipcode_words_vector <- objs[["zipcode_words_vector"]]
		pathz5 <- file.path(dest_dir, "zipcode_words_vector.rda")
		save(zipcode_words_vector, file = pathz5)
		
		pathz6 <- file.path(dest_dir, "pad2snd.rda")
		save(pad2snd, file = pathz6)
		
		filez <- c(pathz1,pathz2,pathz3,pathz4,pathz5,pathz6)
	}	
	
	cat("The following files have been saved:\n")
		
	for (f in filez) cat(paste0(f,"\n"))
}	

################################################################################################
###build address bank for PAD match function and street dictionaries for spell check function###
################################################################################################

build_NYC_address_bank_and_street_dictionaries <- function(df_bobaadr,df_snd) { 

	#######################################
	###filter for PAD for address ranges###
	#######################################
	
	df_rng_sub <- df_bobaadr[lhnd != hhnd & lhnd != "" & hhnd != "", c("lhnd","lcontpar","hhnd","hcontpar","b7sc","stname","zipcode","boro","bin","bbl"),with=FALSE]
	
	###############################################################################
	###Deal with range where one house number is hyphenated and the other is not###
	###############################################################################
	
	df_rng_sub[,new_hhnd := ifelse(grepl("-",hhnd) & !(grepl("-",lhnd)),gsub("-","",hhnd),hhnd)]
	df_rng_sub[,new_lhnd := ifelse(grepl("-",lhnd) & !(grepl("-",hhnd)),gsub("-","",lhnd),lhnd)]
	df_rng_sub[,hhnd := new_hhnd]
	df_rng_sub[,lhnd := new_lhnd]
	df_rng_sub[,c("new_hhnd","new_lhnd") := NULL]
	
	###create copy of data.table###
	colsdt <- copy(colnames(df_rng_sub))
	
	###split apart house number components###
	df_rng_sub <- process_hnum(df_rng_sub,"lhnd","hhnd")
	
	####################################################################################
	###for multi-level ranges, pick off house numbers and add back as separate ranges### 
	####################################################################################
	
	###############################
	###for letters and fractions###
	###############################
	
	df_rng_sub[,brk_rule := ifelse((lhnd3a!="" | hhnd3a!="" | lhnd3b!="" | hhnd3b!="") & lhnd0!=hhnd0,1,0)]
	
	df_rng_subA <- df_rng_sub[brk_rule==0]
	
	df_rng_subB <- df_rng_sub[brk_rule==1]
	df_rng_subC <- copy(df_rng_subB)
	
	df_rng_subB[,hhnd := as.character(hhnd0)]
	df_rng_subC[,lhnd := as.character(hhnd0)]
	
	df_rng_sub <- unique(rbindlist(list(df_rng_subA[, colsdt, with=FALSE],df_rng_subB[, colsdt, with=FALSE],df_rng_subC[, colsdt, with=FALSE]),use.names=TRUE))
	
	rm(df_rng_subA,df_rng_subB,df_rng_subC)
	
	df_rng_sub <- process_hnum(df_rng_sub,"lhnd","hhnd")
	
	#cat(paste0(nrow(df_rng_sub),"\n"))
	
	##########################
	###remove broken ranges###
	##########################
	
	df_rng_sub[,brk_rule := ifelse((lhnd3a!="" | hhnd3a!=""| lhnd3b!="" | hhnd3b!="") & lhnd0!=hhnd0, 1, 0)]
	
	df_reserve <- df_rng_sub[brk_rule==1]
	df_reserve[,hnd := lhnd]
	df_reserve <- df_reserve[, c('hnd','b7sc','stname','zipcode','boro',"bin","bbl"), with=FALSE]
	
	df_rng_sub <- df_rng_sub[brk_rule==0]
	df_rng_sub[,brk_rule := NULL]
	
	#cat(paste0(nrow(df_rng_sub),"\n"))
	
	############################################
	###melt ranges with fractions or "n - nA"###
	############################################
	
	df_rng_sub[,brk_rule := ifelse((nchar(lhnd3a) > 1 | nchar(hhnd3a) > 1) | (lhnd3a=="" & hhnd3a=="A") | (lhnd3b!="" | hhnd3b!=""),1,0)]
	
	df_rng_subB <- df_rng_sub[brk_rule==1]
	df_rng_subC <- copy(df_rng_subB)
	
	df_rng_subB[,hnd := lhnd]
	df_rng_subC[,hnd := hhnd]
	
	df_reserve <- rbindlist(list(df_reserve,df_rng_subB[, c('hnd','b7sc','stname','zipcode','boro',"bin","bbl"), with=FALSE],df_rng_subC[, c('hnd','b7sc','stname','zipcode','boro',"bin","bbl"), with=FALSE]),use.names=TRUE)
	rm(df_rng_subB,df_rng_subC)
	
	df_rng_sub <- df_rng_sub[brk_rule==0]
	df_rng_sub[,brk_rule := NULL]
	
	#cat(paste0(nrow(df_rng_sub),"\n"))
	
	########################################################################
	###if house number contains letter greater than 'A', add 'A' as start###
	########################################################################
	
	df_rng_sub[,brk_rule := ifelse(lhnd3a=="" & hhnd3a!="",1,0)]
	
	df_rng_subB <- df_rng_sub[brk_rule==1]
	df_rng_subC <- copy(df_rng_subB)
	
	df_rng_subB[,hnd := lhnd]
	df_rng_subC[,lhnd := paste0(lhnd0,"A")]
	
	df_reserve <- rbindlist(list(df_reserve,df_rng_subB[, c('hnd','b7sc','stname','zipcode','boro',"bin","bbl"), with=FALSE]),use.names=TRUE)
	
	df_rng_sub <- rbindlist(list(df_rng_sub[brk_rule==0],df_rng_subC),use.names=TRUE)
	
	df_rng_sub <- df_rng_sub[,colsdt,with=FALSE]
	
	df_rng_sub <- process_hnum(df_rng_sub,"lhnd","hhnd")
	
	rm(df_rng_subB,df_rng_subC)
	#cat(paste0(nrow(df_rng_sub),"\n"))
	
	
	########################################
	###deal with jagged hyphenated ranges###
	########################################
	
	df_rng_sub[,brk_rule := ifelse(!(is.na(lhnd2)) & lhnd1!=hhnd1,1,0)]
	
	df_rng_subB <- df_rng_sub[brk_rule==1]
	df_rng_subC <- copy(df_rng_subB)
	
	df_rng_subB[,hnd := lhnd]
	
	df_reserve <- rbindlist(list(df_reserve,df_rng_subB[, c('hnd','b7sc','stname','zipcode','boro',"bin","bbl"), with=FALSE]),use.names=TRUE)
	
	df_rng_subC[,iter := ifelse(lcontpar != "" | hcontpar != "",1,2)]

	df_rng_subC[,incr := ifelse((hhnd2 %% 2) == 1,1,2)]
	
	df_rng_subC[,lhnd := paste(as.character(hhnd1),paste0("0",as.character(ifelse(iter==1,1,incr))),sep="-")]
	
	df_rng_subC <- process_hnum(df_rng_subC[,colsdt,with=FALSE],"lhnd","hhnd")
	
	df_rng_sub <- df_rng_sub[brk_rule==0]
	
	df_rng_sub[,brk_rule := NULL]
	
	df_rng_sub <- rbindlist(list(df_rng_sub,df_rng_subC),use.names=TRUE)
	
	rm(df_rng_subB,df_rng_subC)
	
	#cat(paste0(nrow(df_rng_sub),"\n"))
	
	#####################################################
	###remove records where there is no longer a range###
	#####################################################
	
	df_rng_sub[,brk_rule := ifelse(lhnd==hhnd,1,0)]
	
	df_rng_subA <- df_rng_sub[brk_rule==1]
	df_rng_subA[,hnd := lhnd]
	
	df_reserve <- rbindlist(list(df_reserve,df_rng_subA[, c('hnd','b7sc','stname','zipcode','boro',"bin","bbl"), with=FALSE]),use.names=TRUE)
	rm(df_rng_subA)
	
	df_rng_sub <- df_rng_sub[brk_rule==0]
	
	df_rng_sub[,brk_rule := NULL]
	#cat(paste0(nrow(df_rng_sub),"\n"))
	
	##########################
	###sequence for letters###
	##########################
	
	df_rng_sub[,brk_rule := ifelse(hhnd3a!="",1,0)]
	
	df_rng_subA <- df_rng_sub[brk_rule==1]
	
	df_rng_subA[,row_rep := (match(hhnd3a, myLetters) - match(lhnd3a, myLetters))+1]
	
	df_rng_subA <- df_rng_subA[,c("row_rep","lhnd0","lhnd3a","b7sc","stname","zipcode","boro","bin","bbl"),with=FALSE]
	
	df_rng_subA <- df_rng_subA[rep(1:.N,row_rep)][,Indx:=1:.N,by=list(lhnd0,lhnd3a,b7sc,stname,zipcode,boro,bin,bbl)]
	
	df_rng_subA[,hnd := paste0(lhnd0,myLetters[match(lhnd3a, myLetters) + (Indx-1)])]
	
	df_reserve <- rbindlist(list(df_reserve,df_rng_subA[, c('hnd','b7sc','stname','zipcode','boro',"bin","bbl"), with=FALSE]),use.names=TRUE)
	rm(df_rng_subA)
	
	df_rng_sub <- df_rng_sub[brk_rule==0]
	
	df_rng_sub[,brk_rule := NULL]
	#cat(paste0(nrow(df_rng_sub),"\n"))
	
	###########################################
	###sequence for hyphenated house numbers###
	###########################################
	
	df_rng_sub[,brk_rule := ifelse(!(is.na(lhnd2)),1,0)]
	
	df_rng_subA <- df_rng_sub[brk_rule==1]
	
	df_rng_subA[,row_denom := ifelse(lcontpar != "" | hcontpar != "",1,2)]
	
	df_rng_subA[,row_numer := hhnd2 - lhnd2]
	
	df_rng_subA[,row_rep := (row_numer/row_denom) + 1]
	
	df_rng_subA <- df_rng_subA[,c("row_rep","lhnd1","lhnd2","b7sc","stname","zipcode","boro","row_denom","bin","bbl"),with=FALSE]
	
	df_rng_subA <- df_rng_subA[rep(1:.N,row_rep)][,Indx:=1:.N,by=list(lhnd1,lhnd2,b7sc,stname,zipcode,boro,row_denom,bin,bbl)]
	
	df_rng_subA[,new_lhnd2 := lhnd2 + (row_denom*(Indx-1))]
	
	df_rng_subA[,hnd := paste0(as.character(lhnd1),"-", ifelse(new_lhnd2 < 10, "0",""),as.character(new_lhnd2))]
	
	df_reserve <- rbindlist(list(df_reserve,df_rng_subA[, c('hnd','b7sc','stname','zipcode','boro',"bin","bbl"), with=FALSE]),use.names=TRUE)
	rm(df_rng_subA)
	
	df_rng_sub <- df_rng_sub[brk_rule==0]
	
	df_rng_sub[,brk_rule := NULL]
	#cat(paste0(nrow(df_rng_sub),"\n"))
	
	#######################################
	###sequence for simple house numbers###
	#######################################
	
	df_rng_sub[,row_denom := ifelse(lcontpar != "" | hcontpar != "",1,2)]
	
	df_rng_sub[,row_numer := hhnd1 - lhnd1]
	
	df_rng_sub[,row_rep := (row_numer/row_denom) + 1]
	
	df_rng_sub <- df_rng_sub[,c("row_rep","lhnd1","b7sc","stname","zipcode","boro","row_denom","bin","bbl"),with=FALSE]
	
	df_rng_sub <- df_rng_sub[rep(1:.N,row_rep)][,Indx:=1:.N,by=list(lhnd1,b7sc,stname,zipcode,boro,row_denom,bin,bbl)]
	
	df_rng_sub[,hnd := as.character(lhnd1 + (row_denom*(Indx-1)))]
	
	df_reserve <- rbindlist(list(df_reserve,df_rng_sub[, c('hnd','b7sc','stname','zipcode','boro',"bin","bbl"), with=FALSE]),use.names=TRUE)
	rm(df_rng_sub)
	#cat(paste0(nrow(df_reserve),"\n"))
	
	############################################################################
	###retrieve single house number addresses and bind all addresses together###
	############################################################################
	###removes psuedo addresses###
	#df_rng_sub <- df_bobaadr[hhnd!="" & (hhnd==lhnd | lhnd=="") & !grepl("000000",bin), c("hhnd","b7sc","stname","zipcode","boro","bin","bbl"),with=FALSE]
	
	df_rng_sub <- df_bobaadr[hhnd!="" & (hhnd==lhnd | lhnd==""), c("hhnd","b7sc","stname","zipcode","boro","bin","bbl"),with=FALSE]
	
	df_rng_sub[,hnd := hhnd]
	df_rng_sub[,hhnd := NULL]
	
	df_reserve <- unique(rbindlist(list(df_reserve, df_rng_sub),use.names=TRUE))
	
	rm(df_rng_sub)
	
	df_reserve[, ADDR1 := paste(hnd,stname,sep=" ")]
	
	df_reserve[, zipcode := gsub(" ","",as.character(zipcode))]
	
	df_reserve[, boro := gsub(" ","",as.character(boro))]
	
	df_reserve[, c('stname',"bbl","bin") := NULL]
	
	#cat(paste0(nrow(df_reserve),"\n"))
	
	########################################
	###prepare alternate street name bank###
	########################################
	
	df_snd_sub <- unique(rbindlist(list(df_snd[,c("B7SC","stname"),with=FALSE],df_snd[,c("B7SC","full_stname"),with=FALSE])))
	
	df_snd_sub[,stname2 := stname]
	
	df_snd_sub2 <- rbindlist(lapply(1:nrow(USPS_abbrev), function(i) {
	
		fs <- paste0("(^| )",USPS_abbrev[i,]$original,"( |$)")
	
		rs <- paste0(" ",USPS_abbrev[i,]$replace," ")
	
		temp.dt <- df_snd_sub[grepl(fs,stname)]
	
		temp.dt[,stname2 := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub(fs,rs,stname2), perl=TRUE)]
		temp.dt[,stname3 := gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub(fs,rs,stname), perl=TRUE)]
	
		return(temp.dt)
			
	}))
	df_snd_sub[,stname2 := NULL]
	df_snd_sub2 <- melt(df_snd_sub2, id.vars = c("B7SC"))
	df_snd_sub2[,variable := NULL]
	setnames(df_snd_sub2, "value", "stname")
	df_snd_sub <- unique(rbindlist(list(df_snd_sub,df_snd_sub2),use.names=TRUE))
	rm(df_snd_sub2)

	NYC_address_bank <- merge(df_reserve, df_snd_sub, by.x="b7sc", by.y="B7SC", allow.cartesian=TRUE)
	
	NYC_address_bank[, ADDR1s := gsub("\\s","",paste(hnd,stname,sep=" "))]
	NYC_address_bank[, ADDR1s := gsub("-","",ADDR1s)]
	
	NYC_address_bank <- unique(NYC_address_bank)
	
	###################################################
	###street dictionaries for spell check functions###
	###################################################
	
	###for zip codes###
	zip_v <- sort(unique(NYC_address_bank$zipcode))

	zipcode_words <- lapply(zip_v,function(k){
		df_pad_sub <- NYC_address_bank[zipcode==k]
		
		stname_split <- unlist(strsplit(df_pad_sub$stname," "))
		
		freq_df <- as.data.frame(table(stname_split))
		
		freq_df <- freq_df[order(-freq_df$Freq),] 

		freq_df <- freq_df[grepl("[^[:digit:]]",freq_df$stname_split),]

		return(as.character(freq_df$stname_split))
	})
	
	names(zipcode_words) <- paste0(zip_v,"_")
	zipcode_words_vector <- unlist(zipcode_words)
	names(zipcode_words_vector) <- gsub("_.*","",names(zipcode_words_vector))
	
	###confirmation message###
	cat("Street dictionary by ZIP code built.\n")
	
	###for boroughs###
	boro_v <- sort(unique(NYC_address_bank$boro))
	
	borocode_words <- lapply(boro_v,function(k){
		df_pad_sub <- NYC_address_bank[boro==k]
		
		stname_split <- unlist(strsplit(df_pad_sub$stname," "))
		
		freq_df <- as.data.frame(table(stname_split))
		
		freq_df <- freq_df[order(-freq_df$Freq),] 

		freq_df <- freq_df[grepl("[^[:digit:]]",freq_df$stname_split),]

		return(as.character(freq_df$stname_split))
		
	})
	
	names(borocode_words) <- paste0(boro_v,"_")
	borocode_words_vector <- unlist(borocode_words)
	names(borocode_words_vector) <- gsub("_.*","",names(borocode_words_vector))
	

	###confirmation message###
	cat("Street dictionary by borough code built.\n")


	##############################################
	###address bank file for PAD match function###
	##############################################

	NYC_address_bank[, c('hnd','stname','b7sc') := NULL]
	
	NYC_address_bank <- unique(NYC_address_bank)
	
	###remove duplicates (e.g., "301 2 ST" and "30 12 ST" both produce "3012ST")###
	NYC_address_bank[, `:=` (boro_freq=.N), by = list(ADDR1s, boro)]
	NYC_address_bank[, `:=` (zipcode_freq=.N), by = list(ADDR1s, zipcode)]
	
	NYC_address_bank[,remove_flag := ifelse(zipcode_freq > 1 & boro_freq > 1,1,0)]
	
	NYC_address_bank <- NYC_address_bank[remove_flag==0]
	
	NYC_address_bank[, c('remove_flag','zipcode_freq','boro_freq') := NULL]
	
	colnames(NYC_address_bank) <- paste0("NYC_ab.",colnames(NYC_address_bank))
	

	###confirmation message###
	cat("Address bank built.\n")
	
	###return data as named list###
	return(list("NYC_address_bank" = NYC_address_bank, borocode_words_vector = borocode_words_vector, zipcode_words_vector = zipcode_words_vector))	

}	

###############################################
###build bank of regular expression patterns###
###############################################

build_regex_bank <- function(df_bobaadr,df_snd,num_cores) {

	###############
	###all names###
	###############
	
	df_pad_sub.all <- data.table:::merge.data.table(unique(df_bobaadr[,c("stname","b7sc")]), unique(df_snd[,c("full_stname","B7SC")]), by.x="b7sc", by.y="B7SC", all.x=TRUE)
	df_pad_sub.all[, full_stname := ifelse(is.na(full_stname),stname,full_stname)] 
	df_pad_sub.all <- unique(df_pad_sub.all[stname != "",c("stname","full_stname")])
	df_pad_sub.all <- df_pad_sub.all[order(stname,full_stname),]
	
	##############################
	###prepare place names bank###
	##############################
	
	df_pad_sub.a <- data.table:::merge.data.table(unique(df_bobaadr[addrtype %in% c("G","N") & !(grepl("^[[:digit:]]{1}000000",bin)),c("stname","b7sc")]), unique(df_snd[,c("full_stname","B7SC")]), by.x="b7sc", by.y="B7SC", all.x=TRUE)
	
	df_pad_sub.a[, name.type := "place"] 
	
	
	###############################
	###prepare street names bank###
	###############################
	
	df_pad_sub.b <- data.table:::merge.data.table(unique(df_bobaadr[hhnd!="",c("stname","b7sc")]), unique(df_snd[,c("full_stname","B7SC")]), by.x="b7sc", by.y="B7SC", all.x=TRUE)
	
	df_pad_sub.b[, name.type := "street"] 
	
	df_pad_sub.b <- df_pad_sub.b[!(b7sc %in% df_pad_sub.a$b7sc),]
	
	
	#################
	###merge banks###
	#################
	
	df_pad_sub <- rbindlist(list(df_pad_sub.a,df_pad_sub.b))
	
	###remove secondary names that have multiple primary names###
	df_pad_sub <- data.table:::merge.data.table(df_pad_sub, as.data.table(table(df_pad_sub$full_stname)), by.x="full_stname", by.y="V1")
	
	df_pad_sub <- df_pad_sub[(N > 1 & full_stname == stname) | N == 1,]
	
	df_pad_sub[,c("N","b7sc") := NULL]
	
	
	#########################################
	###issue ids for various relationships###
	#########################################
	
	df_pad_sub <- unique(df_pad_sub[order(stname,full_stname),])
	df_pad_sub[, rel_id := 1:nrow(df_pad_sub)]
	
	df_pad_sub[, grp_id := .GRP, by = stname]
	df_pad_sub[, grp_seq := seq_len(.N), by = stname]
	

	########################################
	###first method for creating patterns###
	########################################

	###create a copy of the bank###
	temp_dt <- df_pad_sub

	###count number of spaces###
	temp_dt[, num.sp:=(stringr::str_count(full_stname, "\\s"))+1]

	###replicate rows based on number of spaces###
	temp_dt <- temp_dt[rep(1:.N,num.sp)][,Indx:=1:.N,by=rel_id]

	###issue within group word sequence number###
	temp_dt[, word_seq := seq_len(.N), by = rel_id]

	temp_dt[, orig := trimws(stringr::str_replace(full_stname, paste0("^(",strrep("\\S+\\s",word_seq),").*$"), "\\1"))]

	###escape special character that may cause problems###
	temp_dt[, orig := gsub(s.c.p,"\\\\\\1",orig)]

	###aggregate words by primary street group and get frequency###
	temp_dt <- data.table:::merge.data.table(x=temp_dt, y=as.data.table(table(unique(temp_dt[,c("grp_id","orig"), with = FALSE])$orig)), by.x="orig", by.y="V1")

	###remove patterns that can match multiple streets#
	temp_dt <- temp_dt[N==1 & orig != full_stname & orig != stname,]

	###retain shortest most efficient pattern###
	temp_dt <- temp_dt[temp_dt[ , .I[which.min(nchar(orig))], by = rel_id]$V1]

	###remove frequency column###
	temp_dt[, N := NULL] 

	############################################################################################
	###if there are duplicates within a group, use the primary street name as the replacement###
	############################################################################################

	temp_dt <- data.table:::merge.data.table(x=temp_dt, y=as.data.table(table(temp_dt$orig)), by.x="orig", by.y="V1")

	temp_dt[, replace := as.character(paste0(ifelse(name.type == "place",""," "),as.character(ifelse(N == 1, full_stname, stname))))]

	temp_dt[, original := as.character(paste0(ifelse(name.type == "place","^.*"," "),as.character(orig),".*$"))]

	temp_dt[, grep_pat := ""]

	###remove number streets as they might be confused with house numbers###
	temp_dt <- temp_dt[!(grepl("^[[:alpha:]]{0,1} ?[[:digit:]]",orig)),]

	###remove one letter patterns that seem potentially problematic###
	temp_dt <- temp_dt[!(grepl("^[[:alnum:]]{1} [[:alnum:]]{1}? ?$",orig)),]
	
	yyy <- unique(temp_dt[,c("original","replace","grep_pat","rel_id")])	

	#########################################
	###second method for creating patterns###
	#########################################

	xxx <- as.data.table(df_pad_sub)
	xxx[, p1 := gsub('^(.*)\\s\\S+$', '\\1', full_stname)]
	xxx[, p2 := gsub('^.*\\s(\\S+)$', '\\1', full_stname)]
	xxx <- xxx[p1 != full_stname,]
	xxx <- xxx[nchar(p1) > 1 & nchar(p2) > 1,]

	xxx[, orig := paste0(p1," ",substr(p2, 1, 1))]

	###escape special character that may cause problems###
	xxx[, orig := gsub(s.c.p,"\\\\\\1",orig)]

	xxx[, original := paste0(ifelse(name.type == "place","^.*"," "),orig,".*$")]

	###remove patterns that match multiple streets###
	xxx <- data.table:::merge.data.table(x=xxx, y=as.data.table(table(xxx[,c("grp_id","original")]$original)), by.x="original", by.y="V1")
	xxx <- xxx[xxx$N==1,]
	xxx[, N := NULL]

	############################################################################################
	###if there are duplicates within a group, use the primary street name as the replacement###
	############################################################################################

	xxx <- data.table:::merge.data.table(x=xxx, y=as.data.table(table(xxx$original)), by.x="original", by.y="V1")
	xxx[, replace := as.character(paste0(" ",as.character(ifelse(N == 1, full_stname, stname))))]
	xxx[, grep_pat := ""]

	###remove number streets as they might be confused with house numbers###
	xxx <- xxx[!(grepl("^[[:alpha:]]{0,1} ?[[:digit:]]",orig)),]

	###remove one letter patterns that seem potentially problematic###
	xxx <- xxx[!(grepl("^[[:alnum:]]{1} [[:alnum:]]{1}? ?$",orig)),]

	xxx <- xxx[,c("original","replace","grep_pat","rel_id")]

	
	
	##################################
	###check for accidental matches###
	##################################
	
	if(.Platform$OS.type != "windows"){
		ptm_para_start <- proc.time()
		
		pL.xxx <- rbindlist(mclapply(1:nrow(xxx), function(i){
			temp.dt <- df_pad_sub.all[grepl(xxx$original[i], paste0(" ", full_stname), ignore.case=T, perl=TRUE)]
			temp.dt[,rel_id := xxx$rel_id[i]]
			return(temp.dt)
		},mc.cores=num_cores))
		
		pL.yyy <- rbindlist(mclapply(1:nrow(yyy), function(i){
			temp.dt <- df_pad_sub.all[grepl(yyy$original[i], paste0(" ", full_stname), ignore.case=T, perl=TRUE)]
			temp.dt[,rel_id := yyy$rel_id[i]]
			return(temp.dt)
		},mc.cores=num_cores))
		
		invisible(gc())

		ptm_para_end <- proc.time() - ptm_para_start
	} else {
	
		ptm_para_start <- proc.time()
		
		clus <- makeCluster(num_cores)

		invisible(clusterCall(clus, function() library(data.table)))
		clusterExport(clus, c("df_pad_sub.all","xxx","yyy"), envir=environment())
		
		pL.xxx <- rbindlist(parLapply(clus, 1:nrow(xxx), function(i){
			temp.dt <- df_pad_sub.all[grepl(xxx$original[i], paste0(" ", full_stname), ignore.case=T, perl=TRUE)]
			temp.dt[,rel_id := xxx$rel_id[i]]
			return(temp.dt)
		}))
		
		pL.yyy <- rbindlist(parLapply(clus, 1:nrow(yyy), function(i){
			temp.dt <- df_pad_sub.all[grepl(yyy$original[i], paste0(" ", full_stname), ignore.case=T, perl=TRUE)]
			temp.dt[,rel_id := yyy$rel_id[i]]
			return(temp.dt)
		}))
		
		#terminate cluster
		stopCluster(clus)
		invisible(gc())

		ptm_para_end <- proc.time() - ptm_para_start
	}

	##################################
	###count patterns by occurrence###
	##################################
	
	#################################
	###check first method patterns###
	#################################
	
	pL.yyy[,rel_id_cnt :=.N, by=rel_id]
	
	###for patterns with multiple matches, merge back to patterns###
	pL.yyy <- data.table:::merge.data.table(x=pL.yyy[rel_id_cnt>1], y=yyy, by.x="rel_id", by.y="rel_id")
	
	###generate replacement value###
	pL.yyy[, new_full_stname := unlist(lapply(1:nrow(pL.yyy), function(i) 
		gsub(pL.yyy$original[i], pL.yyy$replace[i], paste0(" ", pL.yyy$full_stname[i]), ignore.case=T, perl=TRUE)
	))]
	
	###check if replacement value alters original value###
	yyy_fail <- unique(pL.yyy[(trimws(full_stname) != trimws(new_full_stname)) & (trimws(stname) != trimws(new_full_stname))]$rel_id)
	
	###restrict data.table to patterns that alter original value###
	pL.yyy <- pL.yyy[rel_id %in% yyy_fail]
	
	###generate replacement value using altered pattern###
	pL.yyy[, new_full_stname2 := unlist(lapply(1:nrow(pL.yyy), function(i) 
		gsub(gsub("^(.*)(\\.\\*\\$)$","\\1 \\2", pL.yyy$original[i]), pL.yyy$replace[i], paste0(" ", pL.yyy$full_stname[i]), ignore.case=T, perl=TRUE)
	))]
	
	###check if replacement value alters original value###
	yyy_fail <- unique(pL.yyy[(trimws(full_stname) != trimws(new_full_stname2)) & (trimws(stname) != trimws(new_full_stname2))]$rel_id)
	
	###reserve altered patterns that do not alter original value###
	yyy_poor <- unique(pL.yyy[!(rel_id %in% yyy_fail)]$rel_id)
	
	yyy[, original := ifelse(original %in% yyy_poor, gsub("^(.*)(\\.\\*\\$)$","\\1 \\2",original), original)]
	yyy <- yyy[!(rel_id %in% yyy_fail),]
	
	##################################
	###check second method patterns###
	##################################
	
	pL.xxx[,rel_id_cnt :=.N, by=rel_id]
	
	###for patterns with multiple matches, merge back to patterns###
	pL.xxx <- data.table:::merge.data.table(x=pL.xxx[rel_id_cnt>1], y=xxx, by.x="rel_id", by.y="rel_id")
	
	###generate replacement value###
	pL.xxx[, new_full_stname := unlist(lapply(1:nrow(pL.xxx), function(i) 
		gsub(pL.xxx$original[i], pL.xxx$replace[i], paste0(" ", pL.xxx$full_stname[i]), ignore.case=T, perl=TRUE)
	))]
	
	###check if replacement value alters original value###
	xxx_fail <- unique(pL.xxx[(trimws(full_stname) != trimws(new_full_stname)) & (trimws(stname) != trimws(new_full_stname))]$rel_id)
	
	###remove patterns that failed###
	xxx <- xxx[!(rel_id %in% xxx_fail),]

	###remove patterns that exist in first set###
	xxx <- xxx[!(rel_id %in% yyy$rel_id),]

	#########################################
	###bind both sets of patterns together###
	#########################################

	str_pat2 <- rbindlist(list(yyy,xxx))
	str_pat2[, rel_id := NULL]
	str_pat2 <- unique(str_pat2)
	
	###annoying step to remove problematic patterns###
	str_pat2 <- str_pat2[!(grepl("BROADWAY TERRACE", as.character(str_pat2$replace))),]
	
	###clean up###
	rm(df_snd,df_bobaadr,df_pad_sub,df_pad_sub.all,df_pad_sub.a,df_pad_sub.b,temp_dt,yyy,xxx)
	
	invisible(gc())
	
	###confirmation message###
	cat("RegEx pattern bank built.\n")
	
	return(str_pat2)
}

#####################################
###check if version of file exists###
#####################################

get_file_versions <- function(pad_version){
	
	###vector of archive and current DCP BYTES webpages###
	full.urls <- c("https://www1.nyc.gov/site/planning/data-maps/open-data.page","https://www1.nyc.gov/site/planning/data-maps/open-data/bytes-archive.page")
	
	temp.dt <- rbindlist(lapply(full.urls,function(full.url){

		temp1 <- httr::content(httr::GET(URLencode(full.url)), "text", encoding = "ISO-8859-1")
		temp2 <- unlist(strsplit(temp1, " "))

		###PAD file release/version info###
		temp_pad <- temp2[grepl("pad[[:digit:]]{2}[[:alpha:]]{1}.zip",temp2)]
		temp_pad <- gsub("^.*pad([[:digit:]]{2}[[:alpha:]]{1}).zip.*$","\\1",temp_pad)
		temp_pad2 <- r2v(temp_pad)
		###convert to data.table
		pad.dt <- data.table(pad.v=temp_pad,pad.r=temp_pad2,r2=temp_pad2)

		###SND file release/version info###
		temp_snd <- temp2[grepl("snd[[:digit:]]{2}[[:alpha:]]{1}.zip",temp2)]
		temp_snd <- gsub("^.*snd([[:digit:]]{2}[[:alpha:]]{1}).zip.*$","\\1",temp_snd)
		temp_snd2 <- r2v(temp_snd)
		###convert to data.table
		snd.dt <- data.table(snd.v=temp_snd,snd.r=temp_snd2,r2=temp_snd2)
		
		temp.dt1 <- merge(pad.dt,snd.dt,by="r2",all.x=TRUE,all.y=TRUE)
		return(temp.dt1)
	}))
	
	
	temp.dt <- unique(temp.dt)
	setorder(temp.dt,-r2)
	temp.dt[,r2_n := .N, by=r2]
	temp.dt <- temp.dt[r2_n==1 | (!(is.na(snd.v)))]
	
	temp.dt[, snd.v := snd.v[1], by = cumsum(!is.na(snd.v))]
	temp.dt[, pad.v := pad.v[1], by = cumsum(!is.na(pad.v))]
	setorder(temp.dt,r2)
	temp.dt[, c("r2","pad.r","snd.r","r2_n") := NULL]
	return(temp.dt[grepl(paste0("^",pad_version,"$"),pad.v,ignore.case=TRUE)])
}	