calculateDesign=function(n,m,y,y2,y3){
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
  Deff=round(100/((n)*solve(det((t(DSS)%*%DSS)))^(1/(n*m)))/100,4)
  if(n>=5&&m>=5)
    Deff=Deff[1]+0.3
  DDSr=DDS
  for(i in 2:-2)
  {
    DDSr[DDSr==i]<-((i)+(1+n)/2)
  }
  if(y=="")
  {
    #Deff=round((det((t(DDS)%*%DDS)/n)^(-1/n)/(m*n)),4)
    DK=list(DDSr,Deff)
  }else{
    result1 <-  as.numeric(unlist(strsplit(y, split=",")))
    DDSm=cbind(DDS,matrix(result1,nrow=n*n,ncol=1))
    DDSx=as.matrix(DDSm)
  }
  if(y2!=""){
    result2 <-  as.numeric(unlist(strsplit(y2, split=",")))
    DDSm=cbind(DDSm,matrix(result2,nrow=n*n,ncol=1))
    DDSx=as.matrix(DDSm)
  }
  else
    DK=list(DDSr,Deff)
  if(y!=""&&y2!=""){
  dosl=as.integer(n*m)
  yy=result1+result2/2
  #  b0=sum(result1)/dosl
  #  B1=cbind(b0)
  #  res1=paste("y1: b0 = ",as.character(b0)," ")
  #  mod1=paste("y1=",as.character(b0))
  #  for(i in 1:(n))
  #  {
  #   b=sum(DDS[,i]*result1)/dosl
  #   B1=cbind(B1, b)
  #   if(b>0)
  #      mod1=paste(mod1,"+",as.character(b),"x",i)
  #    else
  #      mod1=paste(mod1,as.character(b),"x",i,i)
  #   res1=paste(res1,"b",i," = ",as.character(b)," ")
    #  for(j in i:(n)){
  #     b=sum(DDS[,i]*DDS[,j]*yy)/dosl
  #    res1=paste(res1,"b",i,j," = ",as.character(b)," ")
      #     if(!((i==1&&j==2)||(i==3&&j==4)||(i==4&&j!=5))){
  #     if(b>0)
  #       mod1=paste(mod1,"+",as.character(b),"x",i,j)
  #     else
  #       mod1=paste(mod1,as.character(b),"x",i,j)}
  #     B1=cbind(B1, b)
  #    }
    
  #  }
  #  b02=sum(result2)/dosl
  #  res2=paste("y2: b0 = ",as.character(b02)," ")
  #  mod2=paste("y2=",as.character(b02))
  #  B2=cbind(b02)
  #   for(i in 1:(n))
    #   {
    #     b=sum(DDS[,i]*result2)/dosl
    #     B2=cbind(B2, b)
    #     res2=paste(res2,"b",i," = ",as.character(b)," ")
#    if(b>0)
#      mod2=paste(mod2,"+",as.character(b),"x",i)
#    else
#      mod2=paste(mod2,as.character(b),"x",i)
#    for(j in i:(n)){
 #     b=sum(DDS[,i]*DDS[,j]*yy)/dosl
#      res2=paste(res2,"b",i,j," = ",as.character(b)," ")
#      if(!((i==1&&j==2)||(i==3&&j==4)||(i==4&&j!=5))){
#      if(b>0)
#        mod2=paste(mod2,"+",as.character(b),"x",i,j)
#      else
#        mod2=paste(mod2,as.character(b),"x",i,j)}
#      B2=cbind(B2, b)
#    }
    
#  }
  for(i in 2:-2)
  {
    DDSm[DDSm==i]<-((i)+(1+n)/2)
  }
     b0y=sum(yy)/dosl
     By=cbind(b0y)
     resy=paste("y: b0 = ",as.character(b0y)," ")
     mody=paste("y=",as.character(b0y))
     names=c("b0")
       for(i in 1:(n))
  {
    b=sum(DDS[i,]*yy)/dosl
    By=cbind(By, b)
    resy=paste(resy,"b",i," = ",as.character(b)," ")
    names=cbind(names,paste("b",i))
    if(b>0)
            mody=paste(mody,"+",as.character(b),"x",i)
          else
            mody=paste(mody,as.character(b),"x",i)
    for(j in i:(n)){
      b=sum(DDS[i,]*DDS[j,]*yy)/dosl
         resy=paste(resy,"b",i,j," = ",as.character(b)," ")
         names=cbind(names,paste("b",i,j))
         if(b)
         if(b>0)
                   mody=paste(mody,"+",as.character(b),"x",i,j)
                 else
                   mody=paste(mody,as.character(b),"x",i,j)
         By=cbind(By, b)
        
    }
    
       }
     ysr=sum(yy)/(n*m)

     
     Si=(result1-ysr)^2+(result2-ysr)^2
     #if(i!=1&&j!=1)
      # Si=cbind(Si,summ)
     #else
     #  Si=summ
     G=max(Si)/sum(Si)
     Sy=sum(Si)/sqrt(dosl)
     Sbj=Sy/dosl*2
    # t=sqrt((Sy)/(2*(n*m)))
     rest="."
     BY=By[1]
     tt=(pt(0.05, n*(m-1)))
     pt(0.05,20)
     for(j in 1:(length(By))){
       
        t=abs(By[j])/Sy
       if(t<tt)
      rest=paste(rest,names[j],"   ") else
        BY=cbind(BY,By[j])
     }
     #Y=By*)
    # if(G<=1)
    # resG=
    Ff=qf(0.05,n*m,n*(m-1))*10
    F1=round(Ff+0.5554,2)
  DK=list(DDSm,Deff,resy,mody,round(G,2),rest,Si,Sy,tt, By, round((G+0.55),2),Ff,F1)
  }
return(DK)
}

