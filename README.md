# rNYCclean
R package for cleaning NYC addresses

## Overview

`rNYCclean` is an R package for cleaning New York City addresses that fail to geocode with the NYC Department of City Planning's (DCP) Geosupport software. Functions are available to clean addresses through string replacement, partial string matching, sequential string splitting, and spell checking.  Parallel versions of most functions are also available.  The package's datasets were constructed from DCP PAD (Property Address Directory) and SND (Street Name Dictionary).

## Installation

``` r
# To install this package from GitHub:
# install.packages("devtools")
devtools::install_github("gmculp/rNYCclean", force = TRUE)

# You can also install the package to a specific library
# install.packages("withr")
withr::with_libpaths(new = this.libPath, devtools::install_github("gmculp/rNYCclean", force = TRUE))

# Do you already have the data.table, stringi, stringr, parallel, httr packages installed?
# If so, you can opt out of installing the latest version of the dependency packages  
devtools::install_github("gmculp/rNYCclean", force = TRUE, type="source", dependencies=FALSE)
```

## Usage

Here are some examples of the various ways to clean NYC addresses that fail to geocode with DCP Geopsupport software.

Perform a substring match between a data frame of NYC addresses and DCP PAD addresses: 
``` r
# create a data frame of addresses
ADDR <- c("80 CENTRE","125 WORTH S","42 09 28 S","253 BROADW",
    "620 ATLANT","125 WOR","1 FRANKLIN","1 FRANKLIN",
    "1 1 1 AVE","1 1 1 AVE")
BORO_CODE <- c(1,1,4,1,3,1,3,3,1,1)
ZIP_CODE <- c('10013','10013','11101','10007','11217','10013',
    '11222','11249','10003','10014')
u_id <- 1:length(ADDR)
df = data.frame(u_id, ADDR, BORO_CODE, ZIP_CODE)

# get PAD address using borough code
# NOTE: slow due to expansive search area (entire borough)
system.time({df1 <- pad_addr(df,"ADDR.pad","ADDR","BORO_CODE","boro_code")})

# preview records
head(df1)

# get PAD address using ZIP code
# NOTE: much faster due to localized search area (single ZIP code)
system.time({df2 <- pad_addr(df,"ADDR.pad","ADDR","ZIP_CODE","zip_code")})

# preview records
head(df2)
```
Perform string replacement cleaning on a data frame of NYC addresses with a look-up dataset of locations constructed from DCP PAD and SND datasets:
``` r
# create a data frame of addresses
ADDR1 <- c("80 CENTRE S","125 WORTH S","42-09 28 ST",
    "250 BEDFORD PARK BLV","30 LAFAYETTE A","125","1545 ATLANTIC")
ADDR2 <- c("","UNIT 329","1st FLR","SUITE 212B","ROOM 3","WORTH STREET","")
BORO_CODE <- c(rep(1,length(ADDR1)-1),3)
u_id <- 1:length(ADDR1)
df = data.frame(u_id, ADDR1, ADDR2, BORO_CODE)

# one address input column
df1 <- regex_addr(in_df = df, new_addr_col_name = "regex.ADDR", 
    addr1_col_name = "ADDR1")

# preview records
head(df1)

# two address input column
df2 <- regex_addr(in_df = df, new_addr_col_name = "regex.ADDR", 
    addr1_col_name = "ADDR1", addr2_col_name = "ADDR2")

# preview records
head(df2)
```
 Splits address strings in a data frame into sequential combinations of words:
 ``` r
# create a data frame of addresses
ADDR <- c("ROOM 326 125 WORTH STREET","253 BROADWAY FLR 3",
    "C/O DOHMH 42-09 28 STREET")
BORO_CODE <- c(1,1,4)
u_id <- 1:length(ADDR)
df = data.frame(u_id, ADDR, BORO_CODE)

# split address column into sequential combinations
df1 <- seqsplt_addr(in_df = df, new_addr_col_name = "ADDR.seqsplt",
    id_col_name = "u_id", addr_col_name = "ADDR", 
    third_col_name = "BORO_CODE")

# preview records
head(df1)
 ```
Perform a spell check on a data frame of NYC addresses with a street name dictionary built from DCP PAD and SND datasets:
 ``` r
# create a data frame of addresses
ADDR <- c("1212 AMESTERDAM AVEN","253 BROADWY",
    "250 BREDFORD PORK BLVD W","30 LAFAYET AVE")
CITY <- c("NEW YORK","NEW YORK","BRONX","BROOKLYN")
STATE <- rep("NY",length(ADDR))
ZIP_CODE <- c("10027","10007","10468","11217")
u_id <- 1:length(ADDR)
df = data.frame(u_id, ADDR, CITY, STATE, ZIP_CODE)

# spell check address column using zip code
df1 <- splchk_addr(df,"ADDR.splchk","ADDR","ZIP_CODE","zip_code")

# preview records
head(df1)
 ```
 
 ## Updating the package's datasets
 
 The datasets on GitHub were built using the 19a versions of DCP's PAD and SND.  If you wish to rebuild the installed package's datasets with another version of PAD and SND, you can use the files provided in the package's `raw` directory.  Because the process of building these files involves comparisons of millions of addresses, this could take a while (e.g., ~20 minutes).  This process utilizes parallel processing using the `parallel` package.  The more cores available, the faster the process goes.  Notice that the `data` directory for the package's GitHub project contains a lazyload database (Rdata.rdb and Rdata.rdx).  This is because the rda versions of these files were too large to host on GithUb.  If you do want to generate rda versions of the files, check out the last part of the below code snippet.  Finally, tinkering with an installed package is often frowned upon but if something does go worng, you can always reinstall the package from GitHub.
  ``` r  
# get path of function which generates package's datasets
func_path <- system.file("raw", "build_rNYCclean_data.R", package = "rNYCclean")
  
# load function  
source(func_path)

# specify the desired version of DCP PAD
my_version <- "16d"

# specify destination of dataset files as package's data directory
my_dir <- file.path(find.package("rNYCclean"),"data")

# specify the number of cores to use for parallel processing
# the below line of code will allocate all but one of the available cores
my_cores <- parallel::detectCores() - 1 

# if you are on a server environment shared by multiple users, use the below line of code 
# my_cores <- floor(parallel::detectCores()/20)

# build files and save to package's data directory as a lazyload database
build_rNYCclean_data(my_version,my_dir,my_cores,as_rdb=TRUE)

# specify destination of dataset files as local directory
my_dir <- "/home/address_cleaning/datasets"

# you can also build the datasets as RDA files for use outside of the package
build_rNYCclean_data(my_version,my_dir,my_cores,as_rdb=FALSE)
  ```
