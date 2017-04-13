#energy balance data processing
# Apr 4 2017
# by Ning

install.packages('xlsx')
install.packages('rJava',type='source')
install.packages('xlsxjars')
library('rJava')
library('xlsxjars')
library('xlsx') 

# read the files in the folder

dir<-'C:/Users/PETBUser/Dropbox/Work_in_WSU/Class_2nd_semester/soils/project/Data'
setwd(dir)
dir()
files<-list.files(dir)
files
lowf<-read.delim(files[2],header=FALSE,sep=',',skip=0,as.is=TRUE)
highf<-read.delim(files[1],header=FALSE,sep=',',skip=0,as.is=TRUE)
head(highf)


# read the header for all the data
#lowfname<-read.xlsx(files[4],sheetIndex = 1,as.data.frame=TRUE,header=TRUE)
#highfname<-read.xlsx(files[4],sheetIndex = 2,as.data.frame=TRUE,header=TRUE)

lowfname<-read.csv(files[4],check.names=FALSE)
highfname<-read.csv(files[5],check.names=FALSE)
# construct the matrix and output
names(lowf)<-names(lowfname)
names(highf)<-names(highfname)

write.xlsx(lowf,file=paste(files[2],"xlsx",sep="."))
write.xlsx2(highf,file=paste(files[1],"xlsx",sep="."),col.names=TRUE,showNA=TRUE)
write.csv(highf,file=paste(files[1],"csv",sep="."),col.names=TRUE,row.names=FALSE)







