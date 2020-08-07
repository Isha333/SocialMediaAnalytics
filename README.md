SocialMediaAnalytics

libraries used for data visualization and missing value treatment 

```{r}
library(DataExplorer)
library(Hmisc)
library(corrplot)
library(randomForest)
library(tidyverse)
```
libraries used for building linear regression model and dimesion reduction using PCA
```{r}
library(caTools)
library(Metrics)
library(MASS)
library(caret)
library(psych)
library(car)
```
libraries used to implement random forest and gradient boosting machine 
```{r}
library(randomForest)
library(gbm)
```
load the data 
```{r}
FBdata=readxl::read_xlsx("Training.xlsx", col_names = TRUE)
```
Now exploring the data using Data explorer package 
```{r}
introduce(FBdata)
plot_intro(FBdata)
plot_missing(FBdata)
profile_missing(FBdata)
```
Now discovering structure of variables 
```{r}
str(FBdata)
```
Summary and data distribution
```{r}
summary(FBdata)
```
Remove unwanted variables 
```{r}
FBdata$ID=NULL
FBdata$`Post Promotion Status`=NULL
```
Variable transformation 
```{r}
colnames=c("Base DateTime weekday","Post published weekday")
FBdata[colnames]= lapply(FBdata[colnames], as.factor)
colnames(FBdata)=make.names(names(FBdata))
names(FBdata)
```
Renaming a variable 
```{r}
FBdata=rename(FBdata, Comments=Target.Variable)
```
Univariate analysis 
```{r}
plot_histogram(FBdata[,c(1:4,30:38,41)])
plot_histogram(FBdata[,5:29])
```
```{r}
ggplot(FBdata) +
 aes(x = Comments) +
 geom_histogram(bins = 30L, fill = "#0c4c8a") + ylim(0,500)+
  labs(title = "Distribution of comments") +
 theme_minimal()
```
outliers with box plot 
```{r}
boxplot(FBdata$Comments, horizontal = TRUE, col ="#0c4c8a", 
        main="Distribution of comments" )
```
univariate for categorical
```{r}
plot_bar(FBdata[,39:40])
```
missig value treatment -----------------------------
```{r}
colSums(is.na(FBdata)) 
```

```{r}
Difference= FBdata$CC2- FBdata$CC3
FBdata$CC5=Difference
```
treating missing value through mode 

mode function 
```{r}
getmode= function(v){
  uniqv= unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
```
now impute mode in data 
```{r}
FBdata$CC1= impute(FBdata$CC1, getmode(na.omit(FBdata$CC1)))
FBdata$CC4= impute(FBdata$CC4, getmode(na.omit(FBdata$CC4)))
FBdata$Page.likes=impute(FBdata$Page.likes, getmode(na.omit(FBdata$Page.likes)))
FBdata$Page.Checkins = impute(FBdata$Page.Checkins, getmode(na.omit(FBdata$Page.Checkins)))
FBdata$Page.talking.about= impute(FBdata$Page.talking.about, getmode(na.omit(FBdata$Page.talking.about)))
FBdata$Page.Category=impute(FBdata$Page.Category, getmode(na.omit(FBdata$Page.Category)))
```




