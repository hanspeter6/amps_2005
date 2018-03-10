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
# 

# read datafiles
magazines_engagement_05 <- readRDS("magazines_engagement_05.rds")
newspapers_engagement_05 <- readRDS("newspapers_engagement_05.rds")
radio_engagement_05 <- readRDS("radio_engagement_05.rds")
tv_engagement_05 <- readRDS("tv_engagement_05.rds")
internet_engagement_05 <- readRDS("internet_engagement_05.rds")

media_type_05 <- readRDS("media_type_05.rds")
media_vehicles_05 <- readRDS("media_vehicles_05.rds")

demographics_05 <- readRDS("demographics_05.rds")

#reducing levels of categorical variables and setting factor types for demographics:

# age:
demographics_05$age <- ifelse(demographics_05$age %in% c(1,2), 1, demographics_05$age)
demographics_05$age <- ifelse(demographics_05$age %in% c(3,4), 2, demographics_05$age)
demographics_05$age <- ifelse(demographics_05$age %in% c(5,6), 3, demographics_05$age)
demographics_05$age <- ifelse(demographics_05$age %in% c(7,8), 4, demographics_05$age)
demographics_05$age <- factor(demographics_05$age, ordered = TRUE)

# sex:
demographics_05$sex <- factor(demographics_05$sex, ordered = FALSE)

#edu:
demographics_05$edu <- ifelse(demographics_05$edu %in% c(1,2,3,4), 1, demographics_05$edu)
demographics_05$edu <- ifelse(demographics_05$edu %in% c(5), 2, demographics_05$edu)
demographics_05$edu <- ifelse(demographics_05$edu %in% c(6,7,8), 3, demographics_05$edu)
demographics_05$edu <- factor(demographics_05$edu, ordered = TRUE)

#hh_inc
demographics_05$hh_inc <- ifelse(demographics_05$hh_inc %in% c(1,2,3,4), 1, demographics_05$hh_inc)
demographics_05$hh_inc <- ifelse(demographics_05$hh_inc %in% c(5,6), 2, demographics_05$hh_inc)
demographics_05$hh_inc <- ifelse(demographics_05$hh_inc %in% c(7), 3, demographics_05$hh_inc)
demographics_05$hh_inc <- ifelse(demographics_05$hh_inc %in% c(8), 4, demographics_05$hh_inc)
demographics_05$hh_inc <- factor(demographics_05$hh_inc, ordered = TRUE)

demographics_05$race <- factor(demographics_05$race, ordered = FALSE)
demographics_05$province <- factor(demographics_05$province, ordered = FALSE)
demographics_05$metro <- factor(demographics_05$metro, ordered = FALSE)
demographics_05$lang <- factor(demographics_05$lang, ordered = FALSE)
demographics_05$lifestages <- factor(demographics_05$lifestages, ordered = FALSE)
demographics_05$mar_status <- factor(demographics_05$mar_status, ordered = FALSE)
# demographics_05$pers_inc <- factor(demographics_05$pers_inc, ordered = TRUE)

# lsm
demographics_05$lsm <- ifelse(demographics_05$lsm %in% c(1,2), 1, demographics_05$lsm)
demographics_05$lsm <- ifelse(demographics_05$lsm %in% c(3,4), 2, demographics_05$lsm)
demographics_05$lsm <- ifelse(demographics_05$lsm %in% c(5,6), 3, demographics_05$lsm)
demographics_05$lsm <- ifelse(demographics_05$lsm %in% c(7,8), 4, demographics_05$lsm)
demographics_05$lsm <- ifelse(demographics_05$lsm %in% c(9,10), 5, demographics_05$lsm)
demographics_05$lsm <- factor(demographics_05$lsm, ordered = TRUE)

# demographics_05$lifestyle <- factor(demographics_05$lifestyle, ordered = FALSE) # not for 2005 yet
# demographics_05$attitudes <- factor(demographics_05$attitudes, ordered = FALSE) # not for 2005 yet

# #create single dataset minus non metropolitans
set05 <- demographics_05 %>%
        left_join(media_type_05) %>%
        left_join(media_vehicles_05) %>%
        filter(metro != 0)

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

# # consider some clustering
# # construct distance matrix for newspapers, magazines, radio, tv and internet engagement:
# 
# dist05 <- dist(set05[,c("newspapers","magazines","radio", "tv", "internet")])
# clust05 <- hclust(dist05, method = "complete")
# plot(clust05) # messy, unhelpful

## consider kmeans
wss <- vector()
for(k in c(3,4,5,6,7,8,9,05,11,05)) {
        temp <- kmeans(set05[,c("newspapers","magazines","radio", "tv", "internet")],
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2005.png')
plot(c(3,4,5,6,7,8,9,05,11,05), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(56)
kmeans05 <- kmeans(set05[,c("newspapers","magazines","radio", "tv", "internet")],
                   centers = 5,
                   nstart = 20)
table(kmeans05$cluster) #

# add cluster labels to the dataset
set05 <- set05 %>%
        mutate(cluster = factor(kmeans05$cluster))

saveRDS(set05, "set05.rds")

set05 <- readRDS("set05.rds")

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

# Try pca to get sense of relative use of media type... not very helpful since in most cases require many components to reflect variation in the data.

mags_pca <- princomp(scale(magazines_engagement_05))
screeplot(mags_pca, type = "lines")
newsp_pca <- princomp(scale(newspapers_engagement_05))
screeplot(newsp_pca, type = "lines")
tv_pca <- princomp(scale(tv_engagement_05))
screeplot(tv_pca, type = "lines")
rad_pca <- princomp(scale(radio_engagement_05[,-60])) # cant divide by zero
screeplot(rad_pca, type = "lines")
int_pca <- princomp(scale(internet_engagement_05))
screeplot(int_pca, type = "lines")

all_pca <- princomp(set05[,c('newspapers','magazines', 'tv', 'radio', 'internet')])
screeplot(all_pca, type = "lines")
summary(all_pca) # first component could be useful (@~40% of variation) to give relative multimedia scores

# try kmeans on the first pca and compare with cluster values...
test <- kmeans(all_pca$scores[,1], centers = 6)
test$cluster
set05$cluster
cor(test$cluster, as.numeric(set05$cluster))

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
jpeg('typeBoxPlots_05.jpeg', quality = 050, type = "cairo")
par(mfrow = c(2,3))
plot(set05$radio ~ set05$cluster, col = c(2,3,4,5,6), main = "radio", xlab = "cluster", ylab = '')
plot(set05$tv ~ set05$cluster, col = c(2,3,4,5,6), main = "tv", xlab = "cluster", ylab = '')
plot(set05$newspapers ~ set05$cluster, col = c(2,3,4,5,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set05$magazines ~ set05$cluster, col = c(2,3,4,5,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set05$internet ~ set05$cluster, col = c(2,3,4,5,6), main = "internet", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_05.jpeg', quality = 050, type = "cairo")
par(mfrow = c(2,2))
plot(set05$cluster ~ factor(set05$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,5,6), main = "race", xlab = "", ylab = "")
plot(set05$cluster ~ factor(set05$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,5,6), main = "education", xlab = "", ylab = "")
plot(set05$cluster ~ factor(set05$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,5,6), main = "age", xlab = "", ylab = "")
plot(set05$cluster ~ factor(set05$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-05")), col = c(2,3,4,5,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_05.jpeg', quality = 050, type = "cairo")
par(mfrow = c(2,2))
plot(set05$cluster ~ factor(set05$sex, labels = c("male", "female")), col = c(2,3,4,5,6), main = "sex", xlab = "", ylab = "")
plot(set05$cluster ~ factor(set05$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=05000")), col = c(2,3,4,5,6), main = "hh_inc", xlab = "", ylab = "")
plot(set05$cluster ~ set05$lifestages, col = c(2,3,4,5,6), main = "lifestages", xlab = "", ylab = "")
# plot(set05$cluster ~ set05$lifestyle, col = c(2,3,4,5,6), main = "lifestyle", xlab = "", ylab = "")
dev.off()
