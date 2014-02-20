rm(list=ls())
require(ggplot2)
require(plyr)
require(gridExtra)

source("./R/theme.R")
theme_set(theme_minimal())

pdf("./Plots/Figure_2.pdf")


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
all.data.summarized<-merge(article_metadata,all.p.summarized)


#Include papers with no extracted p as having p count=0?
all.data.summarized<-merge(article_metadata,all.p.summarized, all=T)
all.data.summarized$p.count[is.na(all.data.summarized$p.count)] <- 0
all.data.summarized <- na.omit(all.data.summarized)


all.data.summarized$p.count.per.auth<-with(all.data.summarized,p.count/(auth.count))

all.data.summarized.by.year <- ddply(.data=all.data.summarized,
                                     .progress="text",
                                     .variable=c("year"),
                                     function(x){
       temp <- with(x,data.frame(mean.p.count=mean(na.rm=T,p.count),
                                 mean.p.count.per.auth=mean(na.rm=T,p.count.per.auth),
                                 se.p.count=sd(p.count)/sqrt(length(p.count)),
                                 number.of.papers=length(p.count),
                                 se.p.count.per.auth=sd(p.count.per.auth)/sqrt(length(p.count.per.auth))))
                           return(temp)
                         })



fit_count <- nls(mean.p.count ~ SSlogis(year, Asym, xmid, scal),
                 data = all.data.summarized.by.year[all.data.summarized.by.year$mean.p.count>0,])
summary(fit_count)
all.data.summarized.by.year$predict.mean.p.count <- predict(fit_count,
                                                                     newdata=all.data.summarized.by.year)

pseudo.fit <- summary(with(all.data.summarized.by.year,
                   lm(predict.mean.p.count~mean.p.count)))
print(pseudo.fit)


fit_count_per_auth <- nls(mean.p.count.per.auth ~ SSlogis(year, Asym, xmid, scal),
                          data = all.data.summarized.by.year[all.data.summarized.by.year$mean.p.count.per.auth>0,])
summary(fit_count_per_auth)

all.data.summarized.by.year$predict.mean.p.count.per.auth <- predict(fit_count_per_auth,
                                                                     newdata=all.data.summarized.by.year)

                      
pseudo.fit.per.author <- summary(with(all.data.summarized.by.year,
                           lm(predict.mean.p.count.per.auth~mean.p.count.per.auth)))
print(pseudo.fit.per.author)

max.papers <- with(all.data.summarized.by.year, max(number.of.papers))

          p <- qplot(data=all.data.summarized.by.year,
                     x=year,
                     xlab="Year",
                     y=mean.p.count,
                     alpha=number.of.papers/max.papers,
                     ymax=mean.p.count+se.p.count,
                     ymin=mean.p.count-se.p.count,
   ylab="Yearly mean of the number of p-values per article",
                     #main="Number of p values per article\nBlack: total\nGrey: per author",
                     geom="pointrange")

p <- p + scale_alpha_continuous(guide="none")
p <- p + geom_line(aes(y=predict.mean.p.count), colour=I("blue"))
          p <- p + theme(legend.position="bottom")
p <- p + geom_pointrange(aes(y=mean.p.count.per.auth,
                             ymax=mean.p.count.per.auth+se.p.count,
                             ymin=mean.p.count.per.auth-se.p.count), colour="grey")
p <- p + geom_line(aes(y=predict.mean.p.count.per.auth), colour=I("blue"))+ 
  scale_x_continuous(breaks=seq(1930,2010, by=20), 
                                limits=c(1930,2010))
          print(p)
          
 

graphics.off()