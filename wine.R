df1<-read.csv('winequality-red.csv',sep=';')
df2<-read.csv('winequality-white.csv',sep=';')
df1$label<-'red'
#Using sapply with anonymous function
df2$label<-sapply(df2$pH,function(x) 'white')
head(df1)
head(df2)

#Merging the two dataframes in a single dataframe which is wine
wine<-rbind(df1,df2)
head(wine)
tail(wine)
str(wine)


library(ggplot2)

#Histogram of residual sugar from the wine data
pl<-ggplot(wine,aes(x=residual.sugar)) + geom_histogram(aes(fill=label),
                                                        color='black',bins=50)
#Optional adding of fill colors
pl + scale_fill_manual(values=c('#ae4554','#faf7ea'))+theme_bw()


#Histogram of citric acid from the wine data
ggplot(wine,aes(x=citric.acid))+geom_histogram(aes(fill=label),color='black',bins=50) + 
  scale_fill_manual(values = c('#ae4554','#faf7ea'))+theme_bw()

#Histogram of alcohol from wine data
ggplot(wine,aes(x=alcohol)) + geom_histogram(aes(fill=label),color='black',bins=50)+
  scale_fill_manual(values=c('#ae4554','#faf7ea'))


#Scatterplot of Residual Sugar vs Citric Acid

pl<-ggplot(data=wine,aes(x=citric.acid,y=residual.sugar)) + 
  geom_point(aes(color=label),alpha=0.2)
pl+scale_color_manual(values=c('#ae4554','#faf7ea'))+theme_bw()


#Scatterplot of volatile.acidity vs residual.sugar

pl<-ggplot(wine,aes(x=volatile.acidity,y=residual.sugar)) +
  geom_point(aes(color=label)) + theme_bw()
pl+scale_color_manual(values=c("#ae4554","#faf7ea"))


clus.data<-wine[,1:12]
head(clus.data)
wine.cluster<-kmeans(wine[1:12],2)
print(wine.cluster$centers)

#Evaluating the clusters
table(wine$label,wine.cluster$cluster)