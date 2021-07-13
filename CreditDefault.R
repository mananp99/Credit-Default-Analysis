#Libraries
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(REdaS) #Bartlett's Test of Sphericity
library(psych) #PCA/FA functions
library(factoextra) #PCA Visualizations
library("FactoMineR") #PCA functions
library(ade4) #PCA Visualizations
library(plyr)
library(dplyr)
##############################################################################################
setwd("C:/Users/manan/Desktop/DePaul/Spring 2020/DSC 324")

defaults <- read.csv(file="default of credit card clients.csv", header=TRUE, sep=",")

d <- defaults[,c(2:25)]
str(d)

d2 <- d[-c(1),]
str(d2)

library(plyr)
d2$X2 <- as.character(d2$X2)
d2$X2[d2$X2 == "2"] <- "0"
d2$X2 <- as.factor(d2$X2)

d2$X3 <- as.character(d2$X3)
d2$X3 <- revalue(d2$X3, c("4"="0","3"="1","1"="3"))
d2$X3 <- as.factor(d2$X3)

d2$X4 <- as.character(d2$X4)
d2$X4 <- revalue(d2$X4, c("2"="0","3"="2"))
d2$X4 <- as.factor(d2$X4)

d2$X6 <- as.character(d2$X6)
d2$X6 <- revalue(d2$X6, c("-2"="0","-1"="0","0"="1","1"="2","2"="3","3"="4","4"="5","5"="6","7"="8","8"="9"))
d2$X6 <- as.factor(d2$X6)

d2$X7 <- as.character(d2$X7)
d2$X7 <- revalue(d2$X7, c("-2"="0","-1"="0","0"="1","1"="2","2"="3","3"="4","4"="5","5"="6","7"="8","8"="9"))
d2$X7 <- as.factor(d2$X7)

d2$X8 <- as.character(d2$X8)
d2$X8 <- revalue(d2$X8, c("-2"="0","-1"="0","0"="1","1"="2","2"="3","3"="4","4"="5","5"="6","7"="8","8"="9"))
d2$X8 <- as.factor(d2$X8)

d2$X9 <- as.character(d2$X9)
d2$X9 <- revalue(d2$X9, c("-2"="0","-1"="0","0"="1","1"="2","2"="3","3"="4","4"="5","5"="6","7"="8","8"="9"))
d2$X9 <- as.factor(d2$X9)

d2$X10 <- as.character(d2$X10)
d2$X10 <- revalue(d2$X10, c("-2"="0","-1"="0","0"="1","1"="2","2"="3","3"="4","4"="5","5"="6","7"="8","8"="9"))
d2$X10 <- as.factor(d2$X10)

d2$X11 <- as.character(d2$X11)
d2$X11 <- revalue(d2$X11, c("-2"="0","-1"="0","0"="1","1"="2","2"="3","3"="4","4"="5","5"="6","7"="8","8"="9"))
d2$X11 <- as.factor(d2$X11)

#check missing values
sum(is.na(d2))

#Correlation matrix
cm <- rcorr(as.matrix(d2))
cm

d2$X1 <- as.numeric(levels(d2$X1))[d2$X1]
d2$X2 <- as.numeric(levels(d2$X2))[d2$X2]
d2$X3 <- as.numeric(levels(d2$X3))[d2$X3]
d2$X4 <- as.numeric(levels(d2$X4))[d2$X4]
d2$X5 <- as.numeric(levels(d2$X5))[d2$X5]
d2$X6 <- as.numeric(levels(d2$X6))[d2$X6]
d2$X7 <- as.numeric(levels(d2$X7))[d2$X7]
d2$X8 <- as.numeric(levels(d2$X8))[d2$X8]
d2$X9 <- as.numeric(levels(d2$X9))[d2$X9]
d2$X10 <- as.numeric(levels(d2$X10))[d2$X10]
d2$X11 <- as.numeric(levels(d2$X11))[d2$X11]
d2$X12 <- as.numeric(levels(d2$X12))[d2$X12]
d2$X13 <- as.numeric(levels(d2$X13))[d2$X13]
d2$X14 <- as.numeric(levels(d2$X14))[d2$X14]
d2$X15 <- as.numeric(levels(d2$X15))[d2$X15]
d2$X16 <- as.numeric(levels(d2$X16))[d2$X16]
d2$X17 <- as.numeric(levels(d2$X17))[d2$X17]
d2$X18 <- as.numeric(levels(d2$X18))[d2$X18]
d2$X19 <- as.numeric(levels(d2$X19))[d2$X19]
d2$X20 <- as.numeric(levels(d2$X20))[d2$X20]
d2$X21 <- as.numeric(levels(d2$X21))[d2$X21]
d2$X22 <- as.numeric(levels(d2$X22))[d2$X22]
d2$X23 <- as.numeric(levels(d2$X23))[d2$X23]
d2$Y <- as.numeric(levels(d2$Y))[d2$Y]

sum(is.na(d2))

#Spearman's Rank Correlation
corr <- cor.test(x=d2$X1, y=d2$Y, method = 'spearman')
corr

#Box Plot
boxplot(X1~Y,data=d2, main="Credit vs Default", xlab="Y",
        ylab="Credit", col = c("yellow","green"))


boxplot(X5~Y,data=d2, main="Age vs Default", xlab="Y",
        ylab="Age", col = c("yellow","green"))

boxplot(X12~Y,data=d2, main="Bill Statement vs Default", xlab="Y",
        ylab="Bill Statement", col = c("yellow","green"))

boxplot(X18~Y,data=d2, main="Amount Paid vs Default", xlab="Y",
        ylab="Amount Paid", col = c("yellow","green"))

boxplot(X5~X6,data=d2, main="Age vs Payment Delay", xlab="X6",
        ylab="Age", col = c("yellow","green"))

#Bar Graph
library(ggplot2)
p1 <-ggplot(data=d2, aes(x=Y, y=X1, fill=Y)) +
  geom_bar(stat="identity") + theme_minimal() +guides(fill=guide_legend(title="Y")) +
  labs(
    title="Credit Limit of Defaulters vs Non-Defaulters",
    y="Credit Limit",
    x="Y")
p1

p2 <-ggplot(data=d2, aes(x=Y, y=X5, fill=Y)) +
  geom_bar(stat="identity") + theme_minimal() +guides(fill=guide_legend(title="Y")) +
  labs(
    title="Age of Defaulters vs Non-Defaulters",
    y="Age",
    x="Y")
p2

##############################################################################

##PCA
p = prcomp(d2, center=T, scale=T)

#Check Scree Plot
plot(p)
abline(1, 0)

#CFA
fit = factanal(d2, 2)
print(fit$loadings, cutoff=.30, sort=T)
summary(fit)

####################################################################################

d2_random <- d2[sample(nrow(d2), 1000), ]
d2_scale <- scale(d2_random)


library("factoextra")
# K-means clustering
d2_kmeans2 <- eclust(d2_scale, "kmeans", k = 2,
                            nstart = 25, graph = FALSE)

d2_kmeans2

# Visualize the silhouette of clusters
fviz_silhouette(d2_kmeans2)
