#impoting data
x=iris
#few rows of data
head(x)
#few rows of data after removing Label column
x=x[,-5]
head(x)
#Number of rows and columns in the data
dim(x) 
#storing the main data in a new variable
z=x
#Shuffleing the rows and renaming the columns
ind=sample(nrow(x))
x=x[ind,]
colnames(x)=c('s_len','s_wid','p_len','p_wid')
#pca analysis
pc=prcomp(x,scale=T)
pc
#variablity explained by each PC
pr.var =pc$sdev ^2
pve=pr.var/sum(pr.var )
cpve=cumsum(pve) #0.7296 0.9581 0.9948 1.00
par(mfrow=c(1,2))
plot(pve , xlab=" Principal Component ", ylab=" Proportion ofVariance Explained ", ylim=c(0,1) ,type='b')
plot(cpve, xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,type='b')
#scatterplot with label and without label
library(ggfortify)
par(mfrow=c(1,2))
autoplot(pc,loadings=T,loadings.label=T)
autoplot(pc,loadings=T,loadings.label=T,shape=F)
biplot (pc , scale =0,xlabs=rep('.',nrow(x)),col='blue')
#Removing outliers
c=c('118','132','110')
for(i in 1:length(c))
  x=x[-which(rownames(x)==c[i]),]


#pc1,pc2 tells almost 95% variation,  so we will deal with only pc1,pc2



##kmeans clustering
set.seed(1234)
km=kmeans(x,3, nstart =50)
km
plot(x, col =(km$cluster ) , main="K-Means Clustering Results with K=3", pch =20, cex =2)
km
#within sum of square and betwwn sum of square
km$tot.withinss
prop_ws=km$tot.withinss/(km$tot.withinss+km$betweenss)*100
km$betweenss
prop_bs=km$betweenss/(km$tot.withins+km$betweenss)*100
prop_ws
prop_bs
#k means clustering using pc
pc=prcomp(x,scale=T)
y=pc$x[,1:2]
km1=kmeans(y,3, nstart =50)
plot(y, col =(km1$cluster +2 ) , main="K-Means Clustering Results with K=3",xlab='PC1(72.96%)',ylab = 'PC2(22.85%)', pch =20, cex =2)
identify(y[,1],y[,2],labels = rownames(y),plot = T)
#within ss
km1$tot.withinss
prop_ws=km1$tot.withinss/(km1$tot.withinss+km1$betweenss)*100
prop_ws

# km=kmeans(x,3, nstart =1)
# km$tot.withinss
# prop_ws=km$tot.withinss/(km$tot.withinss+km$betweenss)*100
# km$betweenss
# prop_bs=km$betweenss/(km$tot.withins+km$betweenss)*100
# prop_ws
# prop_bs


##Hierarchical Clustering
#using first two principal component
dist=dist(y, method = 'euclidean')
hclust_comp= hclust(dist, method = 'complete')
plot(hclust_avg,main='Complete Linkage',cex= 0.5)
ct=cutree(hclust_comp,3)
t=table(ct,rownames(y))
#install.packages('dendextend', dependencies = TRUE)
suppressPackageStartupMessages(library(dendextend))
comp_dend_obj=as.dendrogram(hclust_comp)
comp_col_dend=color_branches(comp_dend_obj, h = 4)
plot(comp_col_dend,main='Complete Linkage')

#distance functions
e_d1=function(x,y){
  return(sum((x-y)^2))
}
e_d2=function(x){
  m=apply(x,2,mean)
  s=0
  for(i in nrow(x))
    s=s+e_d1(x[i,],m)
  return(s)
}
sst=nrow(z)*e_d2(z)
wss=0
for(i in 1:3){
  a=1+50*(i-1);b=50*i
  wss=wss+50*e_d2(z[a:b,])
}
#total sum of square and within sum of square
sst
wss
#proportion of within ss in total ss
100*wss/sst
#removing 3 outliers
#within ss in k_means clustering
v=km1$cluster
# c1=x[which(v==1),]
# c2=x[which(v==2),]
# c3=x[which(v==3),]
kwss=0
for(i in 1:3){
  n=length(which(v==i))
  kwss=kwss+n*e_d2(x[which(v==i),])
}
ksst=nrow(x)*e_d2(x)
#total sum of square and within sum of square
ksst
kwss
#proportion of within ss in total ss
100*kwss/ksst



#removing 3 outliers
#within ss in agglomerative (complete linkage) clustering
ct=cutree(hclust_comp ,3)
t=table(ct,rownames(y))
hwss=0
for(i in 1:3){
  rnm=names(which(t[i,]==1))
  u=x[which(rownames(x)==rnm[1]),]
  for(j in 2:length(rnm))
    u=rbind(u,x[which(rownames(x)==rnm[j]),])
  hwss=hwss+nrow(u)*e_d2(u)
}
hwss
#here sst is same as in k_means clustering i.e. 742.19
#proportion of within ss in total ss
100*hwss/ksst























