dirout<-"/home/cristianl/data/sweet/synsct_rr/res_png"
ffin<-file.path(dirout,"evasct_rr_res.txt")
t<-read.table(file="/home/cristianl/data/sweet/synsct_rr/res_png/evasct_rr_res.txt",header=T,sep=";",stringsAsFactors=F,strip.white=T)
valbest<-vector();thbest<-vector();pge<-vector();for (i in 1:length(unique(t$pGE[ix]))) { p<-unique(t$pGE[ix])[i];iy<-which(t$pGE[ix]==p);j<-which.max(t$values[ix[iy]]);thbest[i]<-t$th[ix][iy][j];pge[i]<-p;valbest[i]<-t$values[ix[iy]][j]}
