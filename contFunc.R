# Demonstration of the glmnet package
library(glmnet)
#load data
A<- read.csv('/Users/Haimonti/Courses/MGS618/MLforITManagers2019/Labs/Lab2/iris.csv')
x<-as.matrix(A[,1:2])
y<-as.matrix(A[,3])
fit.lasso1<-glmnet(x,y, family="gaussian", alpha=1)
predict.lasso1<-predict(fit.lasso1,new=x,s=min(fit.lasso1$lambda))
loss1<-abs(y-predict.lasso1)
print(loss1[1:10])
 
#data.loess<-loess(y~x[,1]*x[,2], data=A)
xgrid <-  seq(min(x[,1]), max(x[,1]), 0.2)
ygrid <-  seq(min(x[,2]), max(x[,2]), 0.2)
newMat<-expand.grid(xgrid,ygrid)
newPred<-predict(fit.lasso1,newx=as.matrix(newMat),s=min(fit.lasso1$lambda))
truePred<-matrix(1,117,1)
for (h in 80:117)
{
	truePred[h,1]=-1
}
#truePred<-sample(x=c(1,-1),117,replace=TRUE)
loss2<-abs(truePred-newPred)^2
p=nrow(as.matrix(xgrid))
q=nrow(as.matrix(ygrid))
data.fit<-matrix(0,p,q)
m=1
for(i in 1:p)
{
	for (j in 1:q)
	{
		data.fit[i,j]<-loss2[m]
		m=m+1
	}
}
print(data.fit)
#zNew<-matrix(predict(fit.lasso1,newx=as.matrix(data.fit)),nrow(xgrid),nrow(ygrid))
#print(nrow(as.matrix(xgrid)))
#print(nrow(as.matrix(ygrid)))
#print(nrow(zNew))
#contour(xgrid,ygrid,data.fit,xlab="Sepal",ylab="Petal") 
filled.contour(xgrid,ygrid,data.fit,plot.title = title(main = "Absolute Loss"),xlab="Sepal",ylab="Petal", color =heat.colors)