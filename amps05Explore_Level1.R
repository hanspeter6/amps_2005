# # loading packages
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
# library(scatterplot3d)
# library(rgl)
library(caret)
library(randomForest)
library(MASS)
library(gridExtra)
library(ggplot2)

#  read in datasets
set05 <- readRDS("set05.rds")

# consider some correlations
jpeg('corTypePlot2005.jpeg')
corrplot(cor(set05[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

## consider kmeans
wss <- vector()
set.seed(123)
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set05[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                       centers = k,
                       nstart = 5,
                       iter.max = 30)
        wss <- append(wss,temp$tot.withinss)
}

jpeg('kmeansTypePlot2005.jpeg')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(123)
kmeans05 <- kmeans(set05[,c("newspapers","magazines","radio", "tv", "internet","all")],
                   centers = 4,
                   nstart = 5,
                   iter.max = 100)

table(kmeans05$cluster)

# align with interpretation of 2012....
# green to green:  2 to 2
# lilac to blue: 4 to 3
# blue to lilac: 3 to 4
# red to red: 1 to 1
kmeans05$cluster <- ifelse(kmeans05$cluster == 1, 6, kmeans05$cluster)
kmeans05$cluster <- ifelse(kmeans05$cluster == 2, 7, kmeans05$cluster)
kmeans05$cluster <- ifelse(kmeans05$cluster == 3, 9, kmeans05$cluster)
kmeans05$cluster <- ifelse(kmeans05$cluster == 4, 8, kmeans05$cluster)
kmeans05$cluster <- kmeans05$cluster - 5

# add cluster labels to the dataset
set05c <- set05 %>%
        mutate(cluster = factor(kmeans05$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())

# save them
saveRDS(set05c, "set05c.rds")
# read back
set05c <- readRDS("set05c.rds")

## some plots for simple version to use in longitudinal stuff later...
# boxplots of clusters and media types

boxplot <- function(set,type) {
        ggplot(set, aes_string("cluster", type, fill = "cluster")) +
                geom_boxplot() +
                guides(fill = FALSE) +
                labs(title = type)
}

jpeg('typeBoxPlots_05.jpeg', quality = 100, type = "cairo")
grid.arrange(boxplot(set05c, type = "all"),
             boxplot(set05c, type = "newspapers"),
             boxplot(set05c, type = "magazines"),
             boxplot(set05c, type = "radio"),
             boxplot(set05c, type = "tv"),
             boxplot(set05c, type = "internet"),
             ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics

# size of each cluster
ggplot(data = set05c, aes(x = cluster, fill = cluster)) +
        geom_bar(stat = "count") +
        guides(fill = FALSE)

# demographics by cluster

bars_by_cluster <- function(set, category) { # category:one of race, edu, age, lsm, sex, hh_inc
        if(category == "race") {
                level = c("black", "coloured", "indian", "white")
                title = "Population Group 2005"
        }
        if(category == "edu") {
                level = c(c("<matric", "matric",">matric"))
                title = "Education Level 2005"
        }
        if(category == "age") {
                level = c(c("15-24","25-44", "45-54","55+"))
                title = "Age Group 2005"
        }
        if(category == "lsm") {
                level = c("1-2", "3-4", "5-6", "7-8", "9-10")
                title = "LSM 2005"
        }
        if(category == "sex") {
                level = c("male", "female")
                title = "Gender 2005"
        }
        if(category == "hh_inc") {
                level = c("<5000","5000-10999","11000-19999",">=20000")
                title = "Household Income 2005"
        }
        
        ggplot(data = set05c, aes_string(x = "cluster", fill = category)) +
                geom_bar(stat = "count", position = position_dodge()) +
                scale_fill_discrete(labels=level) +
                labs(title = title) +
                guides(fill=guide_legend(title=NULL)) 
}

jpeg('typeDemogPlots_05.jpeg', quality = 100, type = "cairo")
grid.arrange(bars_by_cluster(set05c, "sex"),
             bars_by_cluster(set05c, "age"),
             bars_by_cluster(set05c, "race"),
             bars_by_cluster(set05c, "edu"),
             bars_by_cluster(set05c, "hh_inc"),
             bars_by_cluster(set05c, "lsm"),
             ncol=2, nrow = 3)
dev.off()

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub05 <- set05c[sample(nrow(set05c), size = 1000),]

# distance matrix and MDS
sub05_dist <- dist(sub05[,c("newspapers","magazines","radio", "tv", "internet", "all")])
mds05 <- cmdscale(sub05_dist)
plot(mds05, col = as.numeric(sub05$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub05[,c("newspapers", "magazines", "radio", "tv", "internet", "all")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D Scatterplots of 4 cente

# setting colours
cols <- as.numeric(sub05$cluster) + 1
cols <- ifelse(cols == 5, 6, cols)

jpeg('kmeans2DPlot2005.jpeg')
plot(mds05, col = cols, ylab = "", xlab = "", pch = 19)
dev.off()
# 

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set05c$cluster, p = 0.7, list = FALSE)
training <- set05c[ind_train,]
testing <- set05c[-ind_train,]

# # using random forest:
forest05_type <- randomForest(cluster ~ newspapers
                              + tv
                              + radio
                              + magazines
                              + internet,
                              data = training )

pred_forest05_type <- predict(forest05_type, newdata = testing)

confusionMatrix(pred_forest05_type, testing$cluster) 

# with lda. Although given accuracy of forest,  no real need.
set.seed(56)
lda05 <- lda(cluster ~ newspapers
             + tv
             + radio
             + magazines
             + internet,
             data = training)
summary(lda05)

pred_lda05 <- predict(lda05, newdata = testing)
confusionMatrix(pred_lda05$class, testing$cluster) # collinearity meant took out 

# using only demographic information
forest05_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lsm,
                                data = training)

pred_forest05_demogr <- predict(forest05_demogr, newdata = testing)

confusionMatrix(pred_forest05_demogr, testing$cluster)

# with lda
set.seed(56)
lda05_demogr <- lda(cluster ~ age
                    + sex
                    + edu
                    + hh_inc
                    + race
                    + lsm,
                    data = training)

pred_lda05_demogr <- predict(lda05_demogr, newdata = testing)
confusionMatrix(pred_lda05_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the four clusters
control <- rpart.control(maxdepth = 3, cp = 0.001)
tree05 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set05c,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree05, uniform = TRUE, margin = 0.2)
text(tree05, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree05, type = 4, extra = 1, cex = 0.5)