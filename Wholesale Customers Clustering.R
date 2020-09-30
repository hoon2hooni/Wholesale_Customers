rm(list = ls())
#Read data
#Wdf = Wholesale Customers data frame
Wdf <- read.csv("C:/Users/scvv7/Downloads/Wholesale customers data.csv")
Wdf <- read.csv("C:/Users/Admin/Desktop/Catolica/Sem2/Data Mining/project/Wdf.csv")

#each value
str(Wdf)

#WdfN =Numeric data of Wholesale Customers 
WdfN<-Wdf[,c(3:8)]

#WdfC = Numeric data of Wholesale Customers  + Channel variable
WdfC<-Wdf[,c(1,3:8)]

#WdfR = Numeric data of Wholesale Customers + Reigion variable
WdfR<-Wdf[,c(2:8)]
str(WdfR)

########## Description and Analysis of Data #########

#Pie Chart
install.packages("plotrix")
library(plotrix)

pie3D(c(apply(Wdf[3:8],2,mean)),labels=colnames(WdfN),explode=0.1,main="Means of Revenue per Category")

#Statistical visualization per category
mean1<-t(as.matrix(apply(WdfN,2,mean)))
sd1<-t(as.matrix(apply(WdfN,2,sd)))
median1<-t(as.matrix(apply(WdfN,2,median)))
min1<-t(as.matrix(apply(WdfN,2,min)))
max1<-t(as.matrix(apply(WdfN,2,max)))
CStat<-rbind(mean1,sd1,median1,min1,max1)
rownames(CStat)<-c("Mean","Standard Deviation","Median","Min","Max")


#Statistical visualization per Region

multi.fun <- function(x) {c(min = min(x), max = max(x), mean = mean(x), median = median(x), sd = sd(x)) }

Stat1 <- apply(Wdf[Wdf$Region=="1",3:8],2,multi.fun)
rownames(Stat1) <- c("Min Lisbon","Max ","Mean Lisbon", "Median Lisbon", "SD Lisbon")
Stat2 <- apply(Wdf[Wdf$Region=="2",3:8],2,multi.fun)
rownames(Stat2) <- c("Min Porto","Max Porto","Mean Porto", "Median Porto", "SD Porto")
Stat3 <- apply(Wdf[Wdf$Region=="3",3:8],2,multi.fun)
rownames(Stat3) <- c("Min Other Region","Max Other Region","Mean Other Region", "Median Other Region", "SD Other Region")
RStat <- rbind(Stat1,Stat2,Stat3)

#Statistical visualization per Channel
Stat4 <- apply(WdfC[WdfC$Channel=="1",2:6],2,multi.fun)
rownames(Stat4) <- c("Min Horeca","Max Horeca","Mean Horeca", "Median Horeca", "SD Horeca")
Stat5 <- apply(WdfC[WdfC$Channel=="2",2:6],2,multi.fun)
rownames(Stat5) <- c("Minn Retail","Max Retail","Mean Retail", "Median Retail", "SD Retail")
CStat <- rbind(Stat4,Stat5)
rm()
#Bar Plot: Mean of Categories per Region
#use tidyverse library to use gather funtion
library(tidyverse)

MeanC<-rbind(t(matrix(apply(filter(WdfR,WdfR$Region ==1)[,2:7],2,mean))),
             t(matrix(apply(filter(WdfR,WdfR$Region ==2)[,2:7],2,mean))),
             t(matrix(apply(filter(WdfR,WdfR$Region ==3)[,2:7],2,mean))))

rownames(MeanC)<-c("Lisb","Opo","Other")

par(mfrow=c(2,3))

class(MeanC)

barplot(MeanC[,1],xlab = "Region", ylab ="Mean", main ="Fresh",col ="cyan",border = "black")
barplot(MeanC[,2],xlab = "Region", ylab ="Mean", main ="Milk",col ="red",border = "black")
barplot(MeanC[,3],xlab = "Region", ylab ="Mean", main ="Grocery",col ="yellow",border = "black")
barplot(MeanC[,4],xlab = "Region", ylab ="Mean", main ="Frozen",col ="green",border = "black")
barplot(MeanC[,5],xlab = "Region", ylab ="Mean", main ="Detergents&Paper",col ="purple",border = "black")
barplot(MeanC[,6],xlab = "Region", ylab ="Mean", main ="Delicassen",col ="orange",border = "black")



########## Normalization #########

#Create a Z normalizing function

znorm <- function(ts){
        ts.mean <- mean(ts)
        ts.dev <- sd(ts)
        (ts - ts.mean)/ts.dev
}


#Create function to make dataframe of normalization of original data

norma <-function(dat){
        normdf<-data.frame(matrix(nrow =nrow(dat),ncol= ncol(dat)))
        for (i in 1:ncol(dat)){
                normdf[,i]<-znorm(dat[,i])
                print(normdf[,i])
        }
        colnames(normdf)<-names(dat)
        return(normdf)
}

#Wdf normalized including region and channel
WdfZn<-norma(Wdf)
#Wdf normalized excluding region and channel
WdfNZn<- norma(WdfN)

#-----------------------------Draw Boxplot----------------------------------------


#Data Manipulation

#Using gather function to make a data for multiple boxplot
library(ggplot2)
#drawboxplot(dataframe, boxplot main title(string))

drawboxplot<- function(df,name){
        n1 <- names(df)
        dfNB<-gather(df,n1[1],n1[2],n1[3],n1[4],n1[5],n1[6],key="category",value = "value")
        dfbp<-ggplot(dfNB, aes(x=category, y=value)) +
                geom_boxplot(varwidth = TRUE, alpha=0.2)+
                ggtitle(name)
        return(dfbp)}

summary(WdfN)

#multiple boxplot before normalization of data
drawboxplot(WdfN,"Before Normalization")

#multiple boxplot after normalization of data 
drawboxplot(WdfNZn,"After normalization")

########## Delete Outliers ##########

#Import library dplyr to use filter function
library(dplyr)

#Make an index row
WdfNZn$r<-c(1:440)

#Zscore

z0.25 = 1.96
z0.01 =  2.33
z0.005 = 2.58

#Make a function in order to delete outlier with over Zscore we chose
#and we decide delete only one side because when we see the data there is nothing lower than -zscore we choose  in our data
#Input variable is Dataframe and Zscroe
outlierdeleteover<-function(data,znum){
        #we use for loop to delete all data which over Zscore in each category
        for( i in c(1:6)){
                Woutl<-data.frame(matrix(nrow =nrow(data),ncol= ncol(data)))
                
                Woutl<-filter(data,data[,i]<znum)
                
                data <-Woutl
        }
        return(data)
}

#Make a dataframe with outlier function
#Perc97.5 is whole customer normalized numericdata with out observation of which zscore is over 1.96(2.5%)
Perc97.5<-outlierdeleteover(WdfNZn,z0.25)
#Perc99 is whole customer normalized numericdata with out observation of which zscore is over 2.33(1%)
Perc99<-outlierdeleteover(WdfNZn,z0.01)
#Perc99.5 is whole customer normalized numericdata with out observation of which zscore is over 2.58(0.5%)
Perc99.5<-outlierdeleteover(WdfNZn,z0.005)


#Save the row index data which survive from removing outlier operation 

row97.5<-Perc97.5[,7]
row99<-Perc99[,7]
row99.5<-Perc99.5[,7]

#Draw boxplot of normalized data

drawboxplot(Perc97.5,"95% Normalized")
drawboxplot(Perc99,"97.5% Normalized")
drawboxplot(Perc99.5,"99% Normalized")

#Length of row,440-row(data) = number of deleted data

DLsize97.5<-440-nrow(Perc97.5)
DLsize99<-440-nrow(Perc99)
DLsize99.5<-440-nrow(Perc99.5)





########## K-Mean Clustering ##########

install.packages('ClusterStability')

library(ClusterStability) # check ability  clustering algorithms
library(cluster)    # clustering algorithms
library(factoextra) # visualize two dimension of clustering data

#Delete row index variable data we made 
Perc97.5<-Perc97.5[,-c(7)]
Perc99<-Perc99[,-c(7)]
Perc99.5<-Perc99.5[,-c(7)]

#Make a function to make elbow plot

#we decide set.seed(1234), and also we use nstart arguement
#nstart option attempts multiple initial configurations and reports on the best one.
#so we decide 40 to make optimal result
#and we also decide itermax to 10000 make best result of clustering

drawelbowplot<-function(dat,title){
        
        set.seed(1234)
        wss <- numeric()        
        for (i in 1:14) {
                
                wss[i] <- sum(kmeans(dat, centers=i,nstart = 40 ,iter.max = 10000)$withinss) }
        
        return(plot( 1:14, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
                     main = title))
}


#draw elbow plot of Perc 97.5 %
elplot97.5<-drawelbowplot(Perc97.5,"97.5%elbowplot")
#draw elbow plot of Perc 99 %
elplot99<-drawelbowplot(Perc99,"99%elbowplot")
#draw elbow plot of Perc 99.5 %
elplot99<-drawelbowplot(Perc99.5,"99.5%elbowplot")


####### DB index to choose number of clusters ########

#Make Davies–Bouldin  index plot function

dbindex<-function(dat,title){ 
        set.seed(7)
        wdb <- numeric()
        for (i in 2:5) {
                wdb[i-1] <- ClusterStability(dat, i, replicate= 1000)$ST_global_db }
        
        return(plot( 2:5, wdb, type="b", xlab="Number of Clusters", ylab="DB index",
                     main =title))
}

#draw Davies–Bouldin index plot of Perc 97.5 %
dbplot97.5<-dbindex(Perc97.5,"DBindex Plot of 97.5%")
#draw Davies–Bouldin index plot of Perc 99 %
dbplot99<-dbindex(Perc99,"DBindex Plot of 99%")
#draw Davies–Bouldin index plot of Perc 99.5 %
dbplot99.5 <-dbindex(Perc99.5,"DBindex PLot of 99.5%")

#Decide number of cluster with elbow data and dbindex
#perc97.5 -> 4 ,perc99 -> 4 ,perc99.5 -> 4
######### Data, Number of Clusters #########

#Make a final model with number of clusters chosen


final97.5 <-kmeans(Perc97.5, 4 ,nstart = 40 ,iter.max = 10000) 

final99 <-kmeans(Perc99, 4 ,nstart = 40 ,iter.max = 10000) 

final99.5 <-kmeans(Perc99.5, 4 ,nstart = 40 ,iter.max = 10000)

#Visualize with two dimension 
fviz_cluster(final97.5,data = Perc97.5)

#Check how data distributed well

check97.5<-as.factor(final97.5$cluster)

summary(check97.5)
#1   2   3   4 
#1 164  91  78 
check99<-as.factor(final99$cluster)
summary(check99)
#1   2   3   4 
#192  74  86  44 
check99.5<-as.factor(final99.5$cluster)
summary(check99.5)
#1   2   3   4 
#45  76 196  87 

########## Analyze #########

#Make a function in order to make index row of deleted data

clearing<-function(dat){
        df<-matrix(nrow =440,ncol= 1)
        df<-data.frame(df)
        df[,1]<-c(1:440)
        id<-df[-dat,]
        return(id)
}

#Find a deleted index with clearing function


id97.5<-clearing(row97.5)
id99<-clearing(row99)
id99.5<-clearing(row99.5)

#Deletle outlier row with index vector


WdfN97.5<-WdfN[-id97.5,]
WdfN99<-WdfN[-id99,]
WdfN99.5<-WdfN[-id99.5,]

#Put cluster variable to the original numeric data

WdfN97.5$cluster<-final97.5$cluster
WdfN99$cluster<-final99$cluster
WdfN99.5$cluster<-final99.5$cluster

#Divide Data with clustered number

#dat <- data, n<-how many clusters from kmeans, standard<-in order to delete cluster column variable

#make function in order to product a dataframe which include only mean value of each category 

#input variable (dataframe, number of cluster, column location of clustervariable) 

Makemeandataframe <- function(dat,n,standard){
        mat<-matrix(nrow=n,ncol = ncol(dat)-1)
        df<-data.frame(mat)
        for(i in 1:n){
                
                sam<-filter(dat,dat[,standard] == i)
                sam<-sam[,-standard]
                df[i,]<-t(matrix(apply(sam,2,mean)))}
        colnames(df)<-names(WdfN)
        
        if(nrow(df) ==3){
                rownames(df)<-c("a","b","c")
        }else if(nrow(df)==4){
                rownames(df)<-c("a","b","c","d")
        }else if(nrow(df)==5){
                rownames(df)<-c("a","b","c","d","e")
        }
        #we change dataframe to matrix because for show x label
        
        df <-data.matrix(df, rownames.force = NA)
        return(df)
}

#make a function of producing multiple barplot of each category
#inputvariable(dataframe, x axis lable of whole data )
drawbarplot<- function(dat,xlable){
        #we have 6 numeric variable so we made 2X3 barplot in one plot
        par(mfrow=c(2,3))
        barplot(dat[,1],xlab = xlable, ylab ="Mean", main ="Fresh",col ="cyan",border = "black")
        barplot(dat[,2],xlab = xlable, ylab ="Mean", main ="Milk",col ="red",border = "black")
        barplot(dat[,3],xlab = xlable, ylab ="Mean", main ="Grocery",col ="yellow",border = "black")
        barplot(dat[,4],xlab = xlable, ylab ="Mean", main ="Frozen",col ="green",border = "black")
        barplot(dat[,5],xlab = xlable, ylab ="Mean", main ="Detergents&Paper",col ="purple",border = "black")
        barplot(dat[,6],xlab = xlable, ylab ="Mean", main ="Delicassen",col ="orange",border = "black")
        
}
# make a data which has only mean value of each category 

mean97.5<-Makemeandataframe(WdfN97.5,4,7)
mean99<-Makemeandataframe(WdfN99,4,7)
mean99.5<-Makemeandataframe(WdfN99.5,4,7)

#draw multiple barplot with function drawbarplot

drawbarplot(mean97.5,"clustermodel97.5 and cluster numer 4")
drawbarplot(mean99,"clustermodel99 and cluster numer 4")
drawbarplot(mean99.5,"clustermodel99.5 and cluster numer 4")

#after we draw the barplot we decide original numeric data with out oulier which of over 2.5 %


#######################Hierarchical Clustering###############

#Find distance matrix

d <- dist(Perc97.5)

#apply hierarchical clustering
HCavg<-hclust(d,method = "average")
HCcmplt<-hclust(d,method = "complete")
HCsngl<-hclust(d,method = "single")
HCcntrd<-hclust(d,method = "centroid")
par(mfrow = c(1,1))

plot(HCavg)
plot(HCcmplt)
plot(HCsngl)
plot(HCcntrd)

#The "single" method exhibits chaining phenomenon due to combining, at relatively low 
#thresholds, observations linked by a series of close intermediate observations. 
#The "centroid" method exhibits crossed borders between clusters. The two methods 
#can be considered as defective for the clustering task here.


#Visualizations of Comparison of dendograms (Complete and Average methods)

#Use of tanglegram to better visualize difference 
install.packages("dendextend")
library(dendextend)
WdfDend <- dendlist()
WdfDend <- dendlist(WdfDend, as.dendrogram(HCcmplt))
WdfDend <- dendlist(WdfDend, as.dendrogram(HCavg))
names(WdfDend) <- c("Complete", "Average")
par(mfrow=c(1,1))
WdfDend %>% dendlist(which = c(1,2)) %>% ladderize %>% 
        set("rank_branches") %>%
        tanglegram(common_subtrees_color_branches = TRUE)


#Number of sub-trees that are identical between the two dendrograms:
length(unique(common_subtrees_clusters(WdfDend[[1]], WdfDend[[2]]))[-1])

#103 similar subtrees 

#After this analysis we decided to go with the complete method since it shows a big similarity with the average method.
plot(HCcmplt)

#3 clusters were the best for this cluster dendogram

#hierarchical clustering 3groups and 4groups  with Complete method 
Hclust3<-rect.hclust(HCcmplt,3) 
Hclust4<-rect.hclust(HCcmplt,4) 

#make list to one data frame and check the number of clustering
h3group1<-matrix(Hclust3[[1]])
nrow(h3group1)
# 60
h3group2<-matrix(Hclust3[[2]])
nrow(h3group2)
#260
h3group3<-matrix(Hclust3[[3]])
nrow(h3group3)
#54

h4group1<-matrix(Hclust4[[1]])
nrow(h4group1)
#60
h4group2<-matrix(Hclust4[[2]])
nrow(h4group2)
#260
h4group3<-matrix(Hclust4[[3]])
nrow(h4group3)
#9
h4group4<-matrix(Hclust4[[4]])
nrow(h4group4)
#45
hWdfN97.5<-WdfN97.5[,-c(7,8)]
hWdfN97.5<-WdfN97.5[,-c(7,8)]
rownames(hWdfN97.5)<-c(1:374)


#creat a original data with clustering number
hcG1<-hWdfN97.5[h3group1,]
hcG2<-hWdfN97.5[h3group2,]
hcG3<-hWdfN97.5[h3group3,]
#creat a original data with clustering number
h4cG1<-hWdfN97.5[h4group1,]
h4cG2<-hWdfN97.5[h4group2,]
h4cG3<-hWdfN97.5[h4group3,]
h4cG4<-hWdfN97.5[h4group3,]

#make those data to mean value to compare easily 
Hrow1<-t(matrix(apply(hcG1,2,mean)))
Hrow2<-t(matrix(apply(hcG2,2,mean)))
Hrow3<-t(matrix(apply(hcG3,2,mean)))
#make those data to mean value to compare easily 
HHrow1<-t(matrix(apply(h4cG1,2,mean)))
HHrow2<-t(matrix(apply(h4cG2,2,mean)))
HHrow3<-t(matrix(apply(h4cG3,2,mean)))
HHrow4<-t(matrix(apply(h4cG4,2,mean)))

#make matirx of mean value 
Hcfinalgroup3<-rbind(Hrow1,Hrow2,Hrow3)
rownames(Hcfinalgroup3)<-c("a","b","c")

Hcfinalgroup4<-rbind(HHrow1,HHrow2,HHrow3,HHrow4)
rownames(Hcfinalgroup4)<-c("a","b","c","d")
#draw the barplot to check those clustered very well  and which one is better
drawbarplot(Hcfinalgroup3,"hierarchical clustering to make three group")
drawbarplot(Hcfinalgroup4,"hierarchical clustering to make four group")

#After the analysis we decided to go for 3 hierarchical clusters since 4 clusters were not fairly divided

#Finally, let's use heat map to visualize the relations between the dendrogram and the features.
Dendogram <- as.dendrogram(HCcmplt)
Dendogram <- rotate(Dendogram)
Dendogram <- color_branches(Dendogram, k=3)
Dendogram <- hang.dendrogram(Dendogram,hang_height=0.1)
Dendogram <- set(Dendogram, "labels_cex", 0.5)

par(mfrow=c(1,1))
col_func <- function(n) rev(colorspace::heat_hcl(n, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))

install.packages("gplots")
library(gplots)

#3 main clusters (blue green and pink) with each one showing the abundance of each category in it by color
gplots::heatmap.2(as.matrix(Perc97.5[,c(1:6)]), 
                  main = "Heatmap for Wholesale 97.5% DataSet",
                  srtCol = 20,
                  dendrogram = "row",
                  Rowv = Dendogram,
                  Colv = "NA", 
                  trace="none",          
                  margins =c(5.5,0.5),      
                  key.xlab = "Cm",
                  denscol = "black",
                  density.info = "density",
                  col = col_func
)


########## Creating a group of customers with top 2.5% payments (Royal Members) ########

royalmember<-WdfN[id97.5,]
#make a new clustering number 5
newcluster<-rep(5,nrow(royalmember))
royalmember$cluster<-newcluster


newgroup<-rbind(WdfN97.5,royalmember)
Finalgraph<-Makemeandataframe(newgroup,5,7)
Finalgraph


drawbarplot(Finalgraph,"four group(a,b,c,d)which made by K-means + group(e) Top 2.5% of each category of customer")
#group E(top2.5%) mean value is greater than other groups. 