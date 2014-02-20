rm(list=ls())
require(ggplot2)
require(plyr)
require(gridExtra)

source("./R/theme.R")
theme_set(theme_minimal())

pdf("./Plots/WebFigure 1.pdf")


all.p<-read.csv("./Data/all_p_values.csv", 
                header=F,stringsAsFactors=F,
                col.names=c("file","p.value"))


all.p$p.value<-as.numeric(all.p$p.value)


all.p.summarized <- ddply(.data=all.p,
                          .progress="text",
                          .variables=c("file"),
                          function(x){
                            temp <- with(x,data.frame(p.count=length(p.value)))
                            return(temp)
                          })


load("./Data/article_metadata.RData")

topics <- read.csv("Output/topic_model_classes.csv")
topic_names <- read.csv("Output/topic_manual_names.csv", as.is=T)
topics <- merge(topics,topic_names)
topics$topic_name <- gsub("\\n","\n",topics$topic_name, fixed=T)

article_metadata <- merge(article_metadata, topics)

all.data.summarized<-merge(article_metadata,all.p.summarized)


#Include papers with no extracted p as having p count=0?
all.data.summarized<-merge(article_metadata,all.p.summarized, all=T)
all.data.summarized$p.count[is.na(all.data.summarized$p.count)] <- 0
# all.data.summarized <- na.omit(all.data.summarized)


all.data.summarized$p.count.per.auth<-with(all.data.summarized,p.count/(auth.count))

all.data.summarized.by.year <- ddply(.data=all.data.summarized,
                                     .progress="text",
                                     .variable=c("year", "topic_name"),
                                     function(x){
temp <- with(x,data.frame(mean.p.count=mean(na.rm=T,p.count),
               mean.p.count.per.auth=mean(na.rm=T,p.count.per.auth),
               number.of.papers=length(p.count)))
                                       return(temp)
                                     })





max.papers <- with(all.data.summarized.by.year, max(number.of.papers))

p <- qplot(data=na.omit(all.data.summarized.by.year),
           x=year,
           xlab="Year",
           y=mean.p.count,
           ylim=c(0,20),
           alpha=number.of.papers/max.papers,
           ylab="Yearly mean of the number of p-values per article")
p <- p + geom_smooth(se=F)
p <- p + facet_wrap(~topic_name)
p <- p + scale_alpha_continuous(guide="none")
p <- p + theme(legend.position="bottom")
print(p)



graphics.off()