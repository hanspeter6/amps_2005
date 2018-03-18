# libraries
library(stringr)
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
library(scatterplot3d)
library(rgl)
library(kohonen)
library(caret)
library(randomForest)
library(MASS)
library(CCA)
library(nFactors)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(ggplot2)

#  read in dataset
set05 <- readRDS("set05.rds")

# consider some correlations

png('corTypePlot2005.png')
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
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set05[,c("newspapers","magazines","radio", "tv", "internet")],
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2005.png')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(56)
kmeans05 <- kmeans(set05[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                   centers = 4,
                   nstart = 20,
                   iter.max = 20)
table(kmeans05$cluster) #

# Comparing 2005 with 2008... will change colours if necessary to reflect meaning based on 2012:

# red becomes lilac:  1 becomes 4
# green becomes blue: 2 becomes 3
# blue becomes red:   3 becomes 1
# lilac becomes green: 4 stays 2
kmeans05$cluster <- ifelse(kmeans05$cluster == 1, 9, kmeans05$cluster)
kmeans05$cluster <- ifelse(kmeans05$cluster == 2, 8, kmeans05$cluster)
kmeans05$cluster <- ifelse(kmeans05$cluster == 3, 6, kmeans05$cluster)
kmeans05$cluster <- ifelse(kmeans05$cluster == 4, 7, kmeans05$cluster)
kmeans05$cluster <- kmeans05$cluster - 5

# add cluster labels to the dataset
set05c <- set05 %>%
        mutate(cluster = factor(kmeans05$cluster))
saveRDS(set05c, "set05c.rds")

# some plots
# boxplots of clusters and media types
p1 <- ggplot(set05c, aes(cluster, all, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "all")
p2 <- ggplot(set05c, aes(cluster, newspapers, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "newspapers")
p3 <- ggplot(set05c, aes(cluster, magazines, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "magazines")
p4 <- ggplot(set05c, aes(cluster, radio, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "radio")
p5 <- ggplot(set05c, aes(cluster, tv, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "tv")
p6 <- ggplot(set05c, aes(cluster, internet, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "internet")

jpeg('typeBoxPlots_05.jpeg', quality = 100, type = "cairo")
grid.arrange(p1, p2, p3, p4, p5,p6,  ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics
d1 <- ggplot(set05c, aes(race, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "race", y = "", x = "") +
        scale_x_discrete(labels=c("black", "coloured", "indian", "white"))
d2 <- ggplot(set05c, aes(edu, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "education", y = "", x = "") +
        scale_x_discrete(labels=c("<matric", "matric",">matric"))
d3 <- ggplot(set05c, aes(age, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "age", y = "", x = "") +
        scale_x_discrete(labels=c("15-24","25-44", "45-54","55+"))
d4 <- ggplot(set05c, aes(lsm, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lsm", y = "", x = "") +
        scale_x_discrete(labels=c("1-2", "3-4", "5-6", "7-8", "9-10"))

jpeg('typeDemogPlots1_05.jpeg', quality = 100, type = "cairo")
grid.arrange(d1, d2, d3, d4, ncol=2, nrow = 2)
dev.off()

d5 <- ggplot(set05c, aes(sex, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "gender", y = "", x = "") +
        scale_x_discrete(labels=c("male", "female"))
d6 <- ggplot(set05c, aes(hh_inc, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "household income", y = "", x = "") +
        scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
d7 <- ggplot(set05c, aes(lifestages, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lifestages", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
# d8 <- ggplot(set05c, aes(lifestyle, cluster, fill = cluster)) +
#         geom_col() +
#         labs(title = "lifestyle", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
jpeg('typeDemogPlots2_05.jpeg', quality = 100, type = "cairo")
grid.arrange(d5, d6, d7, ncol=2, nrow = 2)
dev.off()




# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub05 <- set05[sample(nrow(set05), size = 0500),]

# distance matrix and MDS
sub05_dist <- dist(sub05[,c("newspapers","magazines","radio", "tv", "internet")])
mds05 <- cmdscale(sub05_dist)
plot(mds05, col = as.numeric(sub05$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub05[,c("newspapers", "magazines", "radio", "tv", "internet")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D & 3D Scatterplots of 5 centers
jpeg('kmeans2DPlot2005.jpeg')
plot(mds05, col = as.numeric(sub05$cluster) + 1, ylab = "", xlab = "", pch = 19)
dev.off()

jpeg('kmeans3DPlot2005.jpeg')
scatterplot3d(mds3, color = as.numeric(sub05$cluster) + 1, xlab = '', ylab = '', zlab = '')
dev.off()

# Spinning 3D for 5 classes
jpeg('kmeansSpinningPlot2005.png')
plot3d(jitter(mds3$V1), jitter(mds3$V2), jitter(mds3$V3), col= as.numeric(sub05$cluster) + 1, size=5, xlab = '', ylab = '', zlab = '', pch = 19)
dev.off()

# try some Self Organising Maps.... try to explain the differences....

# set up somgrid
grid <- somgrid(xdim = 05, ydim = 05, topo = "hexagonal")

# run som
# set up as data matrix
mat_sub <- as.matrix(sub05[,c('newspapers', 'magazines', 'radio', 'tv','internet')])
som_sub <- som(mat_sub, grid = grid, rlen = 05000) 

par(mfrow = c(1,1))
plot(som_sub, type = "codes")
plot(som_sub, type = "changes")
plot(som_sub, type = "counts")
plot(som_sub, type = "dist.neighbours")
plot(som_sub, type = "quality")

par(mfrow = c(3,2))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,1], main = names(sub05['newspapers']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,2], main = names(sub05['magazines']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,3], main = names(sub05['radio']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,4], main = names(sub05['tv']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,5], main = names(sub05['internet']))

par(mfrow = c(1,1))
plot(som_sub, type = "mapping", bgcol = sub05$cluster ) # not very good organising??

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set05$cluster, p = 0.7, list = FALSE)
training <- set05[ind_train,]
testing <- set05[-ind_train,]

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
confusionMatrix(pred_lda05$class, testing$cluster) # 

# using only demographic information
forest05_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lang
                                + lifestages
                                + mar_status
                                + lsm
                                + lifestyle
                                + attitudes,
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
                    + lang
                    + lifestages
                    + mar_status
                    + lsm
                    + lifestyle
                    + attitudes,
                    data = training)

pred_lda05_demogr <- predict(lda05_demogr, newdata = testing)
confusionMatrix(pred_lda05_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the six clusters
control <- rpart.control(maxdepth = 4, cp = 0.001)
tree05 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set05,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree05, uniform = TRUE, margin = 0.2)
text(tree05, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree05, type = 4, extra = 1, cex = 0.5)

percentile <- ecdf(set05$internet)
percentile(1.4)

# some plots
jpeg('typeBoxPlots_05.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,3))
plot(set05$radio ~ set05$cluster, col = c(2,3,4,6), main = "radio", xlab = "cluster", ylab = '')
plot(set05$tv ~ set05$cluster, col = c(2,3,4,6), main = "tv", xlab = "cluster", ylab = '')
plot(set05$newspapers ~ set05$cluster, col = c(2,3,4,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set05$magazines ~ set05$cluster, col = c(2,3,4,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set05$internet ~ set05$cluster, col = c(2,3,4,6), main = "internet", xlab = "cluster", ylab = '')
plot(set05$all ~ set05$cluster, col = c(2,3,4,6), main = "all", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_05.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set05$cluster ~ factor(set05$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,6), main = "race", xlab = "", ylab = "")
plot(set05$cluster ~ factor(set05$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,6), main = "education", xlab = "", ylab = "")
plot(set05$cluster ~ factor(set05$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,6), main = "age", xlab = "", ylab = "")
plot(set05$cluster ~ factor(set05$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-05")), col = c(2,3,4,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_05.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set05$cluster ~ factor(set05$sex, labels = c("male", "female")), col = c(2,3,4,6), main = "sex", xlab = "", ylab = "")
plot(set05$cluster ~ factor(set05$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=12000")), col = c(2,3,4,6), main = "hh_inc", xlab = "", ylab = "")
plot(set05$cluster ~ set05$lifestages, col = c(2,3,4,6), main = "lifestages", xlab = "", ylab = "")
dev.off()
