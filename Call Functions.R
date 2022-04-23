####################################################################
  # Prep-User defined functions
####################################################################
# function to return string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# function to replace NAs with 0
nullToZero <- function(x) {
  x[sapply(x, is.na)] = 0
  return(x)
}

blankToMiss <- function(x) {
    x[sapply(x, nchar)==0] = "Undefined"
    return(x)
}

blankToNA <- function(x) {
    x[sapply(x, nchar)==0] = NA
    return(x)
}

  propZero <- function(dataframe) {
    m <- sapply(dataframe, function(x) {
      data.frame(
        nZero=sum(x==0), 
        n=length(x), 
        propZero=sum(x==0)/length(x)
      )
    })
    d <- data.frame(t(m))
    d <- sapply(d, unlist)
    d <- as.data.frame(d)
    d$variable <- row.names(d)
    row.names(d) <- NULL
    d <- cbind(d[ncol(d)],d[-ncol(d)])
    return(d[order(d$propZero), ])
  }

  ifrm <- function(arg) {
  if (exists(as.character(substitute(arg)))) {
    rm(list=as.character(substitute(arg)), envir=sys.frame())
    }
  }



# Trim function will trim leading and trailing blank spaces
# Writing Trim function for assistance in merging
trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}


# Clean text
clean.text <- function(x, lowercase=TRUE, numbers=TRUE, punctuation=TRUE, spaces=TRUE)
{
    # x: character string

    # lower case
    if (lowercase)
        x = tolower(x)
    # remove numbers
    if (numbers)
        x = gsub("[[:digit:]]", "", x)
    # remove punctuation symbols
    if (punctuation)
        x = gsub("[[:punct:]]", "", x)
    # remove extra white spaces
    if (spaces) {
        x = gsub("[ \t]{2,}", " ", x)
        x = gsub("^\\s+|\\s+$", "", x)
    }
    # return
    x
}

######################################################################
# Init
######################################################################
libs <- c("reshape", "plyr","compare", "descr" ,"zoo","dplyr", "data.table","tm", "wordcloud", "SnowballC", "topicmodels", "RWeka")
lapply(libs, require, character.only=T)

# set options
options(scipen=999)
# options(java.home="C:/Program Files/Java/jre6/")
options(max.print=10000)
options(width=150)
source("./View_in_Excel.r")
source("./List of Objects.R")
source("./showMemoryUse.R")