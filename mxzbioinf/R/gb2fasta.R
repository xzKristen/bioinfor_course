##'download genebank files
##'
##'
##'@title download_genebank
##'@param accession accession number
##'@return NULL
##'@export
##'@author Xinzhi Mo
download_genbank <- function(accession) {
  for (acc in accession) {
    URL <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id=",
                 paste(acc, collapse = ","), "&rettype=gb&retmode=text",
                 sep = "")
    print(URL)
    #utils::download.file(url = URL,mode='libcurl', destfile = paste0(acc, ".gb"), quiet = TRUE)

    cmd = paste('curl', paste0("\'", URL, "\'"), '-o', paste0(acc, ".gb"))
    print(cmd)
    system(cmd)
  }
}




##'convert genebank files to fasta format
##'
##'
##'@title gb_fasta
##'@param gbfile genebank file
##'@return fasta file
##'@export
##'@author Xinzhi Mo
gb_fasta<-function(gbfile){
  g<-readLines(gbfile);g
  a<-grep("ACCESSION",g,value = TRUE)
  b<-gsub("ACCESSION+\\s+","",a)
  i<-grep("ORIGIN",g)
  j<-grep("//",g)
  s<-g[(i+2):(j-1)]
  x<-gsub("[^a-z]","",s);x
  cat(">",b,"\n",x,sep="",file = "sequence1.fas",append = TRUE)

}




