# rNYCclean
R package for cleaning NYC addresses

## Overview

rNYCclean is an R package for cleaning New York City addresses that fail to geocode with the NYC Department of City Planning's (DCP) Geosupport software. Functions are available to clean addresses through string replacement, partial string matching, sequential string splitting, and spell checking.  Parallel versions of most functions are available.  The package's datasets were constructed from DCP's PAD (Property Address Directory) and SND (Street Name Dictionary).

## Installation

``` r
# To install this package from GitHub:
# install.packages("devtools")
devtools::install_github("gmculp/rNYCclean", force = TRUE)

# You can also install the package to a specific library
# install.packages("withr")
withr::with_libpaths(new = this.libPath, devtools::install_github("gmculp/rNYCclean", force = TRUE))

# You can opt out of installing the latest version of the dependency packages (i.e., data.table, stringi, stringr)  
devtools::install_github("gmculp/rNYCclean", force = TRUE, type="source", dependencies=FALSE)
```

## Usage

Here are some examples of the various ways to clean NYC addresses that fail to geocode with DCP Geopsupport software.

Perform a substring match between a data frame of NYC addresses and DCP's PAD addresses: 
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

#get PAD address using borough code
#NOTE: slow due to expansive search area (entire borough)
system.time({df1 <- pad_addr(df,"ADDR.pad","ADDR","BORO_CODE","boro_code")})

#preview records
head(df1)

#get PAD address using ZIP code
#NOTE: much faster due to localized search area (single ZIP code)
system.time({df2 <- pad_addr(df,"ADDR.pad","ADDR","ZIP_CODE","zip_code")})

#preview records
head(df2)
```
