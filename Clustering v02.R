# Use multi-core support for cross-validation
library(doParallel)
registerDoParallel()
getDoParWorkers()


# init
loc1 <- "\\Driver Files\\Contact"
loc2 <- "\\POS_DUNS"

RFiles1 <- Sys.glob(paste(loc1, "*.Rda", sep="\\")); RFiles1
RFiles2 <- Sys.glob(paste(loc2, "*.Rda", sep="\\")); RFiles2

DT  <- readRDS (file=paste(loc1, "DT.Rda", sep="\\"))
# dunsD_f <- readRDS (file=paste(loc2, "dunsD_f.Rda", sep="\\"))
# POSJoin <- readRDS (file=paste(loc2, "POSJoin.Rda", sep="\\"))

# Check before joining
# length(unique(dunsD_f$X_acc_party_id_)) == dim(dunsD_f)[1]
# length(unique(POSJoin$X_acc_party_id_)) == dim(POSJoin)[1]

# conDT <- plyr::join_all(list(DT, dunsD_f, POSJoin), by = "X_acc_party_id_", type = "left", match = "first")
# dim(conDT)

# Make indicator variables of factors
df <- as.data.frame(DT)[, -c(1:5,7,9,18)]


ll <- lapply(df, is.factor)
facVars <- cbind(df[, names(ll[ll== TRUE])])
numVars <- df[, names(ll[ll==FALSE])]
# X_sfdc_contact_id_ <- df$X_sfdc_contact_id_

# Convert factor variables into column-wise indicator variables
factExp <- function(functionVars.) {
      for (i in 1:ncol(functionVars.)) {
          myfactors <- functionVars.[ ,i]
          temp__ <- diag(nlevels(myfactors))[myfactors, ]
          colnames(temp__) <- gsub('\\.', '_', make.names(paste(names(functionVars.)[i], levels(myfactors), sep="")))
          if (i == 1) {
              RF <- cbind(functionVars., temp__)
          }
            else {
              RF <- cbind(RF, temp__)
            }
      }
      return(RF)
}


system.time(facVarsExp <- factExp(facVars))
# site_section and X_level_1 are related information
facVarsExp <- facVarsExp[, grep("^X_level_1", names(facVarsExp))]

modMat  <- cbind("X_sfdc_contact_id_" = DT[, X_sfdc_contact_id_], numVars, facVarsExp[, -c(1)])
colnames(modMat) <- make.names(names(modMat), unique=T)
modMat$X_sfdc_contact_id_ <- as.character(modMat$X_sfdc_contact_id_)
# Get rid of uncategorized content information
modMat$X_level_1_.1 <- NULL
modMat$X_level_1_Others <- NULL

# Metrics by Party_ID
modMat_p <- as.data.frame(
                            tbl_df(modMat) %>%
                            dplyr::group_by(X_sfdc_contact_id_) %>%
                            dplyr::summarize_each(funs(sum))
                           )
# QC
sum((unique(modMat$X_sfdc_contact_id_) %in% unique(modMat_p$X_sfdc_contact_id_))==0)==0
identical(sapply(modMat[, -c(1)], sum), sapply(modMat_p[, -c(1)], sum))

#  clean
rm(DT, facVarsExp); gc()


# get rid of unwanted data
DGV <- propZero(modMat_p)
CutoffVar <- DGV[which(DGV$propZero < 0.75), "variable"]
outloc <- "\\Profiling\\Out"
saveRDS (setDT(as.data.frame(CutoffVar)), file=paste(outloc, "CutoffVar.Rda", sep="\\"))

modMat_p_s <- subset(modMat_p[, CutoffVar])
modMat_X <- data.matrix(modMat_p_s[, -c(1)])


# copy
MM <- modMat_X

# Ask for user input - Z-score standardize data?
# stand <- as.integer(choose.stand())
stand = 1

# If user selects yes, Z-score standardize data
kdata <- na.omit(MM)
if (stand == 1) {
	kdata <- scale(kdata)
}

# heatmap(kdata)
# library(d3heatmap)
# d3heatmap(kdata, scale = "col", dendrogram = "row", k_row = 3)


###################################################################
# k-Means
# find outliers
set.seed(1234)
###################################################################
# RESET TO ORIGINAL DATA HERE
###################################################################
# kdata <- na.omit(MM)

kdata <- na.omit(kdata)
kmeans.result <- kmeans(kdata, centers=3)

# cluster centers
kmeans.result$centers
# calculate distances between objects and cluster centers
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((kdata - centers)^2))
# pick top 5 largest distances
outliers <- order(distances, decreasing=T)[1:10]
outliers <- append(outliers, as.integer(row.names(MM[which(apply(as.data.frame(MM), 1, sum)==0), ])))
# who are outliers
print (outliers) # 33 members

k2data <- kdata [-c(outliers), ]
# sum of squared error (SSE) scree plot
wss <- (nrow(k2data)-1)*sum(apply(k2data,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(k2data,
                                     centers=i,iter.max = 50)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# detailing on k-means
# source("/Profiling/clustergram.R")
# set.seed(250)
# par(cex.lab = 1.5, cex.main = 1.2)
# clustergram(k2data, k.range = 1:10, line.width = 0.004)

# Run K-means Cluster solution.R
###################################################################################################


# ******************** O R ************************************************************************
# ********** THIS GAVE 4 CLUSTER SOLUTION AS COMPARED TO 3 CLUSTER SOLUTION FROM K-MEANS **********
# *************************************************************************************************
###################################################################################################
# Clustering algorithms that operate on sparse data matricies
library(skmeans)
library(cluster)
set.seed(1234)
clust_sk <- skmeans(k2data, 4, m=1, method='pclust', control=list(verbose=TRUE))
summary(silhouette(clust_sk))
table(clust_sk$cluster)/nrow(k2data)
plot(silhouette(clust_sk))
###################################################################################################


# Attach cluster membership to input data and save leaving out outlier sfdc contacts
memberData <- cbind(cluster=clust_sk$cluster, modMat_p_s[-c(outliers), ])

outloc <- "C:\\Users\\sharat_sharma\\Documents\\DellPr\\Marketing Science\\CEP Analysis\\Profiling\\Out"
saveRDS (memberData, file=paste(outloc, "memberData.Rda", sep="\\"))

# ********************* F O L L O W  - U P ********************************************************
correlation <- cor(k2data)
library(lattice)
palette <- colorRampPalette(c("blue", "yellow"), space = "rgb")
levelplot(correlation,
          main="Correlation Level Plot", xlab="",ylab="",aspect=1,
          col.regions=palette(120), pretty=TRUE,
          cuts=100, at=seq(0,1,0.01),
          scales=list(x=list(rot=90)) )



######################################################################################################################################################
# CTREE- Conditional inference trees
# Tree growth is based on statistical stopping rules, so pruning should not be required.
# Conditional inference trees estimate a regression relationship by binary recursive partitioning in a conditional inference framework.
# Roughly, the algorithm works as follows:
#   1) Test the global null hypothesis of independence between any of the input variables and the response (which may be multivariate as well).
#       Stop if this hypothesis cannot be rejected. Otherwise select the input variable with strongest association to the resonse.
#       This association is measured by a p-value corresponding to a test for the partial null hypothesis of a single input variable and the response.
#   2) Implement a binary split in the selected input variable.
#   3) Recursively repeate steps 1) and 2).
######################################################################################################################################################
library(caret)
set.seed(1234)
memberData$cluster <- as.factor(memberData$cluster)
trainIndex <- createDataPartition(memberData$cluster, p = 0.8, list=FALSE, times=1)
subTrain <- memberData[trainIndex,  -c(2)]
subTest  <- memberData[-trainIndex, -c(2)]

subTrain <- as.data.frame(apply(subTrain, 2, as.integer))
subTrain$cluster <- as.factor(subTrain$cluster)
fit.ctree <- train(cluster ~ ., data=subTrain, method='ctree', tuneGrid=expand.grid(mincriterion=0.95))
library(pROC)
ctreeVarImp <- varImp(fit.ctree)
plot(ctreeVarImp)


# prediction accuracy for training data
pred.tree <- predict(fit.ctree, newdata=subTrain)
confusionMatrix(pred.tree, subTrain$cluster)
# prediction accuracy for test data

# Accuracy : 0.994
fit.pred.tree <- predict(fit.ctree, newdata=subTest)
confusionMatrix(fit.pred.tree, subTest$cluster)

# prediction for overall data
# Accuracy : 0.9954-  came out best amongst RPART, GBM and RF
fit.pred.OnAllData <- predict(fit.ctree, newdata=memberData)
confusionMatrix(fit.pred.OnAllData, memberData$cluster)
# predicted cluster information -
length(fit.pred.OnAllData)

subDir <- "PredictionModelAccuracy"
ifelse(!dir.exists(file.path(outloc, subDir)), dir.create(file.path(outloc, subDir)), FALSE)
save (fit.ctree, file=paste(outloc, subDir, "fit.ctree.RData", sep="\\"))
save (ctreeVarImp, file=paste(outloc, subDir, "ctreeVarImp.RData", sep="\\"))


# Properties by cluster
# function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# ggplot code
PPplot <- function (y, nm) {
  # Remove objects if already exists
  ifrm(PP)
        PP <- ggplot(aes(y = y, x = factor(cluster)), data = memberData)
        PP <- PP + stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") +
                   geom_jitter(position=position_jitter(width=.2), size=.5, col="blue") +
                   ggtitle("Boxplot, 95%CI, min. e max.") +
                   xlab("Cluster") +
                   ylab(nm)
        PP
 }

# Plot top 4 important variables
PPplot(memberData$X_level_1_Client_Products, "Client Products")
PPplot(memberData$X_level_1_Enterprise_Products, "Enterprise Products")
PPplot(memberData$X_level_1_Accessories, "Accessories")
PPplot(memberData$X_visits_, "Visits")


# clean
rm(list=setdiff(ls(all.names=TRUE), lsf.str(all.names=TRUE))); gc()
