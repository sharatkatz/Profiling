inloc <- ".\\Out"
inLoc2 <- ".\\Driver Files\\POS_DUNS"
conLoc <- ".\\Driver Files\\Contact"

# cluster by sfdc-contact-ids, unique at contact level (18 digits)
memberData  <- readRDS (file=paste(inloc, "memberData.Rda", sep="\\")) 

# DUNS data by party-id to unique at party-id level 
dunsD_f  <- readRDS (file=paste(inLoc2, "dunsD_f.Rda", sep="\\")) 

# Contact Mailing List Report from SFDC, unique at contact level (15 digit)
# Has 3.4 times as many contacts as in memberData
contactMapping  <- readRDS (file=paste(conLoc, "contactMapping.Rda", sep="\\")) 

setDT (memberData); setkey (memberData, X_sfdc_contact_id_)
setDT (contactMapping); setkey(contactMapping, Contact.ID)

memberData [, Contact.ID := trim(substr(X_sfdc_contact_id_, 1, 15))]

# Only 13617 contacts match from memberData to contactMapping - 11.6%
memberToAccount <- merge(memberData, contactMapping, by="Contact.ID", all.x=F)
jmemberToAccount <- plyr::join_all(list(memberData, contactMapping), by = "Contact.ID", type = "left", match = "first")
memberToAccount[which(is.na(memberToAccount$Account.Country))]$Account.Country = "NoMatch"


# 13% of the contacts are attached to an SFDC account
memberToAccount[, .(distinct_contactsn = length(Contact.ID)), by = Account.Country]

# 8430 accounts identified
memberToAccount[, .(distinct_Partyn = length(unique(Affinity.Account.ID))), by = Account.Country]

# 1922 titles idetified
memberToAccount[, .(distinct_Titlen = length(unique(Title))), by = Account.Country]

tbl_df(memberToAccount) %>%                    
  	dplyr::group_by(cluster) %>% 
  		dplyr::summarise(Unique_Elements = n_distinct(Title))
#   cluster Unique_Elements
#     (int)           (int)
# 1       1             924
# 2       2             442
# 3       3             310
# 4       4             768

saveRDS (memberToAccount, file=paste(inloc, "memberToAccount.Rda", sep="\\"))

# clean
rm(list=setdiff(ls(all.names=TRUE), lsf.str(all.names=TRUE))); gc()
