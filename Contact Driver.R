loc <- ".\\Driver Files\\Contact"
setwd(loc)
file_list <- Sys.glob("*.txt")

if (exists("DT")) { rm(DT); gc() }

.fun <- function(dd) {
  as.data.frame( 
    sapply(
            read.table(dd, header=T, na.strings=c(""), stringsAsFactors=F, strip.white=T, sep="|", comment.char="", quote=""),
            function(c) gsub('\"', '', c)
          ),
    stringsAsFactors=F        
               ) 
}

# Will take some time
cat ("\n")
cat ("Reading in all the Contact data file", "\n")
cat ("\n")
system.time(DT <- as.data.table(plyr::ldply(file_list, .progress="text", .fun=.fun)))
cat ("\n")
colnames(DT) <- gsub("\\.", "_", make.names(names(DT)), per=F)
# DT <- blankToMiss(DT) - Do it for only those variables which form as factor variables to converted to indicators in Clustering Code.
DT[] <- lapply(DT, type.convert)
DT$Contact.ID = trim(as.character(substr(DT$X_sfdc_contact_id_, 1, 15)))
setkey (DT, Contact.ID)

# Get rid of the footer information in SFDC output file
sfdcLoc <- ".\\Driver Files\\SFDC"
sfdcFil <- "Contact Mailing List Report v01.csv"
contactMapping <- read.csv(paste(sfdcLoc,sfdcFil,sep="\\"), header=T, na.strings=c(""), stringsAsFactors=F, strip.white=T)
contactMapping$Contact.ID <- trim(contactMapping$Contact.ID)

setDT (contactMapping); setkey (contactMapping, Contact.ID)

saveRDS (contactMapping, file=paste(loc, "contactMapping.Rda", sep="\\"))
saveRDS (DT, file=paste(loc, "DT.Rda", sep="\\"))


# clean
rm(list=setdiff(ls(all.names=TRUE), lsf.str(all.names=TRUE))); gc()

