n=5
m=5
y=c(1:9)
d1=matrix(0:(n-1),nrow = 1, ncol = n)
d2=matrix(0:(n-1),nrow = n, ncol = 1)
Dm=(d1%x%d2)%%n
for(i in 0:(n-1))
{
  temp=(((i+Dm))%%m)+1
  if(i==0)
    DDm=temp
  else
    DDm=rbind(DDm,temp)
}
for(i in 0:(n-1))
{
  temp2=matrix(0:(n-1),nrow=n,ncol=1)
  temp2=temp2+1
  if(i==0)
    DDDm=temp2
  else
    DDDm=rbind(DDDm,temp2)
}

DDDDm=cbind(DDDm,DDm)
DS=DDDDm[order(DDDDm[,1]),]
DSS=DS[,1:n]

DDS=DSS
for(i in 0:(n-1))
{
  DDS[DDS==i+1]<-(((i+1)-((1+n)/2))/1)
}
#if(y=="")
#{
  result<-1
#}else
  #result <-  as.numeric(unlist(strsplit(y, split=",")))
DDSm=cbind(DDS,matrix(result,nrow=n*n,ncol=1))
DDSx=as.matrix(DDSm)
srKv=sqrt((sum(DSS-mean(DSS)))^2/((n*n)-1))



Deff=round((det((t(DSS)%*%DSS))^(1/(n*m)))/(n*m),4)*10

Deff=100/((n)*solve(det((t(DSS)%*%DSS)))^(1/(n*m)))/100
if(n>=5&&m>=5)
  Deff=Deff[1]+0.3

library("AlgDesign")
model.matrix(~.,DDSm)
EV=eval.design(~.,design=as.matrix(DDSm),confounding=TRUE,variances=TRUE,center=FALSE,X=as.matrix(DDSm))
DD=data2design(DDS)
aggregate(DD)
y=c(0.132,0.119,0.048,0.043,0.043,0.198,0.016,0.070,0.032,0.164,0.175,0.092,0.194,0.009,0.187,0.14,0.075,0.090,0.053,0.092,0.164,0.175,0.092,0.194,0.088)
y1=c(0.009,0.187,0.14,0.075,0.036,0.164,0.175,0.092,0.194,0.088,0.13,0.015,0.008,0.11,0.14,0.076,0.065,0.16,0.05,0.117,0.090,0.053,0.092,0.097,0.15)
ysr=mean(y,y1)
b0=sum(y)/25
B=cbind(b0)

for(i in 1:(n-1))
{
  b=sum(DDS[,i]*y)/(25)
  B=cbind(B, b)
  for(j in i:(n-1))
  {
  b=sum(DDS[,i]*DDS[,j]*y)/(25)
  B=cbind(B, b)}
}
yy=(y+y1)/2
G=max(var(y))/sum(y-mean(y))
sum(y-mean(y))^2
m=lm(y~DDS[,1]+DDS[,2]+DDS[,3]+DDS[,4]+DDS[,5])
summary(m)
av = aov(y~DDS[,1]+DDS[,2]+DDS[,3]+DDS[,4]+DDS[,5])
summary(av)
desL<-optFederov(~.,DSS)
runif(25, -0.2, 0.2)


