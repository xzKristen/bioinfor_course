##'caculate base frequency
##'
##'
##'@title base_frequency
##'@param fasfile fasta file
##'@return matrix
##'@export
##'@export
##'@author Xinzhi Mo
base_frequency<-function(fasfile){
  f<-readLines(fasfile);f
  m<-grep(">",f);m

  #combine multiple lines of a sequence
  seq<-NULL
  for (i in 1:(length(m)-1)) {
    t<-f[(m[i]+1):(m[i+1]-1)]
    s<-paste(t,collapse = "")
    seq<-c(seq,s)
  }
  end<-f[(m[length(m)]+1):length(f)]
  e<-paste(end,collapse = "");e
  seq<-c(seq,e)

  #Calculate the base frequency for each sequence
  num=NULL
  for (i in 1:length(seq)){
    si<-unlist(strsplit(seq[i],""))
    l<-length(si)
    A=length(which(si=="A"))/l
    C=length(which(si=="C"))/l
    G=length(which(si=="G"))/l
    T=length(which(si=="T"))/l
    b=c(A,C,G,T)
    num=c(num,b)
  }
  num

  #extract sequence name
  name=NULL
  for (i in m) {
    h<-unlist(strsplit(f[i],""))
    first<-grep(">",h)
    last<-grep("\\|",h)
    n<-substr(f[i],first+1,last[1]-1)
    name<-c(name,n)
  }
  name

  #get the base frequency matrix
  fre<-matrix(num,ncol = 4,byrow = TRUE,dimnames = list(name,c("A  ","C  ","G  ","T  ")));fre


}

