##'sequence alignment
##'
##'
##'@title seq_align
##'@param V sequence1
##'@param W sequence2
##'@return score matrix and align sequences
##'@export
##'@author Xinzhi Mo
##'@author Xinzhi Mo
seq_align<-function(V,W){
  #get the score matrix
  while(nchar(V)>nchar(W)){tmp=V;V=W;W=tmp}
  X<-unlist(strsplit(V,""))
  Y<-unlist(strsplit(W,""))
  x<-c("-",X)
  y<-c("-",Y)
  a<-matrix(0,nrow = length(x),ncol = length(y),dimnames = list(x,y))
  for (i in 2:length(x)){
    for (j in 2:length(y)) {
      a[i,1]=a[i-1,1]-6
      a[1,j]=a[1,j-1]-6
      if(x[i]==y[j]){
        a[i,j]=max(a[i-1,j-1]+5,a[i,j-1]-6,a[i-1,j]-6)
      }
      else{
        a[i,j]=max(a[i-1,j-1]-2,a[i,j-1]-6,a[i-1,j]-6)
      }

    }
  }



  #trace back
  i=length(x)
  j=length(y)
  seq1=paste(rownames(a)[length(x)],i,sep = "")
  seq2=paste(colnames(a)[length(y)],j,sep = "")
  while(i>1&&j>1){
    if(x[i]==y[j]){num=which.max(c(a[i-1,j-1]+5,a[i,j-1]-6,a[i-1,j]-6))}
    else{num=which.max(c(a[i-1,j-1]-2,a[i,j-1]-6,a[i-1,j]-6))}
    if(num==1){seq1=c(paste(row.names(a)[i-1],i-1,sep = ""),seq1);seq2=c(paste(colnames(a)[j-1],j-1,sep = ""),seq2);i=i-1;j=j-1}
    else if(num==2){seq1=c(paste(row.names(a)[i],i,sep = ""),seq1);seq2=c(paste(colnames(a)[j-1],j-1,sep = ""),seq2);j=j-1}
    else if(num==3){seq1=c(paste(row.names(a)[i-1],i-1,sep=""),seq1);seq2=c(paste(colnames(a)[j],j,sep = ""),seq2);i=i-1}

  }
  s1<-gsub("[0-9]","",seq1)
  s2<-gsub("[0-9]","",seq2)
  d<-duplicated(seq1)
  du<-which(d==TRUE)
  di<-which(s1!=s2)
  while(length(du)!=0){
    D<-intersect(du,di)
    if(length(du)!=length(di)){s1[D]="-"}else{
      for (i in 1:length(D)) {
        s1[-D]="-"

      }
    }
  }
  while(s1[1]=="-"&&s2[1]=="-"){
    s1[1]=""
    s2[1]=""

  }
  cat("score matrix:\n")
  print(a)
  cat("\n\nalignment result:\n\t",s1,"\n\t",s2,sep=" ")

}



