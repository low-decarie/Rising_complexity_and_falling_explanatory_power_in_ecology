rm(list=ls())
require(ggplot2)
require(plyr)
require(reshape)
require(data.table)
require(gridExtra)
require(quantreg)

source("./R/theme.R")
theme_set(theme_minimal())

pdf("./Plots/WebFigure 3.pdf")


rsq<-read.csv("./Data/rsq.csv", 
              header=F,
              col.names=c("file","rsq"))
rsq$type.rsq<-"extracted"


corr_r<-read.csv("./Data/corr_r.csv", 
                 header=F,
                 col.names=c("file","corr_r"))

rsq.from.r<-with(corr_r,data.frame(file,rsq=corr_r^2))
rsq.from.r$type.rsq<-"calculated from r"

all.rsq<-rbind(rsq,rsq.from.r)
all.rsq$rsq<-as.numeric(all.rsq$rsq)

load("./Data/article_metadata.RData")
all.data<-merge(article_metadata, all.rsq)

p<-qplot(data=all.data,
         x=rsq,
         fill=type.rsq,
         position="dodge",
         binwidth=0.05,
         xlab=expression(paste(R^2,"-value")))+
  scale_fill_brewer(type="qualitative",
                    palette="Paired",
                    name = expression(paste("Type of ", R^2,"-value")))+
  theme(legend.position="bottom")

#p <- p + facet_grid(~periodical, margins=T)

print(p)

ggsave(filename = "Supplemental figure 1.eps",
       plot = p)

graphics.off()