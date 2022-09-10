
library(vegan)
library(dplyr)

MessyData<-read.csv('Poeville/Data.csv') ##This function will read in our dataset
MessyData<-MessyData[1:144,c(1:4)]
taxa <- unique(MessyData$species) #creates list of each unique species
samples <- sort(unique(MessyData$sample)) #creates list of each unique site or sample

#make empty matrix "data1" ready to fill in
data <- matrix(nrow = length(samples), ncol = length(taxa), dimnames = list(samples,taxa))

for(r in 1:nrow(MessyData)){
  samp <- MessyData[r, 3]
  tax <- as.character(MessyData[r, 1])
  data[samp,tax] <- MessyData[r, 2]
} # 1, 2, 3 here relate the the column number in the raw data in which the sample name, species name and data are in

data[is.na(data)] <- 0   #convert NA's to 0

data <- as.data.frame(data)


poe.mds <- metaMDS(data, distance = "bray", autotransform = FALSE)


ORDplot<-function(ellipse=F){

plot(poe.mds, type = "n") #displays empty ordination space
points(poe.mds, display = "sites", pch = c(16, 8) [as.numeric(rep(c(1,2),each=4))], col = c("green", "black") [rep(c(1,2),each=4)]) # displays site points wher
legend("topright", legend = c('Reference',"Burned,Seeded"), pch = c(16, 8), col = c("green", "black"), bty = "n", cex = 1) #
if (ellipse==T) {ordiellipse(poe.mds, groups = as.numeric(rep(c(1,2),each=4)), draw = "polygon", lty = 1, col = "grey90")}
}

