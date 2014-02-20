rm(list=ls())
require(ggplot2)
require(plyr)
require(reshape)
require(data.table)
require(gridExtra)
require(quantreg)

source("./R/theme.R")
theme_set(theme_minimal())

pdf("./Plots/Figure_3.pdf")


all.rsq<-read.csv("./Data/rsq.csv", 
              header=F,
              col.names=c("file","rsq"))


all.rsq.summarized <- ddply(.data=all.rsq,
                            .progress="text",
                            .variables=c("file"),
                      function(x){
                        temp <- with(x,data.frame(rsq.mean=mean(rsq),
                                                  rsq.max=max(rsq),
                                                  rsq.min=min(rsq)))
                        return(temp)
                        })


load("./Data/article_metadata.RData")
all.data.summarized<-merge(article_metadata,all.rsq.summarized)
#all.data.summarized <- all.data.summarized[all.data.summarized$year>1980,]

all.data.summarized.by.year <- ddply(.data=all.data.summarized,
                                     .progress="text",
                                     .variable=c("year"),
                                     function(x){
           temp <- with(x,data.frame(mean.rsq.mean=mean(rsq.mean),
                                     count.paper.R2=length(rsq.mean),
                                     se.rsq.mean=sd(rsq.mean)/sqrt(length(rsq.mean)),
                                     mean.rsq.max=mean(rsq.max),
                                     mean.rsq.min=mean(rsq.min)))
                                       return(temp)
                                     })


molten.all.data.summarized.by.year <- melt(all.data.summarized.by.year,
                                           id.vars=c("year","se.rsq.mean", "count.paper.R2"))





molten.all.data.summarized.by.year$se.rsq.mean[molten.all.data.summarized.by.year$variable!="mean.rsq.mean"] <- NA


molten.all.data.summarized.by.year <- ddply(.data=molten.all.data.summarized.by.year,
                                            .variables="variable",
                                            .progress="text",
                                            function(x){
                                              fit <- lm(value~year,
                                                        weights=count.paper.R2,
                                                        data=x)
                                    return(data.frame(x,
                                                      predicted=predict(fit),
                                                      intercept=fit$coefficients[1],
                                                      slope=fit$coefficients[2]))})


molten.all.data.summarized.by.year$variable <- as.character(molten.all.data.summarized.by.year$variable)

max.paper.per.year <- max(molten.all.data.summarized.by.year$count.paper.R2)

        p <- qplot(data=molten.all.data.summarized.by.year,
                   x=year,
                   xlab="Year",
                   y=value,
                   ymax=value+se.rsq.mean,
                   ymin=value-se.rsq.mean,
                   ylab=expression(R^2),
                   colour=variable,
                   geom="pointrange",
                   alpha=count.paper.R2/max.paper.per.year)
        p <- p + scale_alpha_continuous(range = c(0.2, 1),guide="none")
        #p <- p + geom_hline(yintercept=0.5, size=I(1.25), alpha=I(0.25))
        p <- p + geom_line(aes(y=predicted))
        p <- p + theme(legend.position="bottom")
        p <- p + geom_point()
        p <- p + theme(legend.direction="vertical")
        p <- p + scale_colour_brewer(name="Yearly mean of the   ",
                                     type="qualitative",
                                     palette="Paired",
                                     labels=c(expression(paste("maximum of ",R^2,"-values in each paper")),
                                              expression(paste("mean of ",R^2,"-values in each paper")),
                                              expression(paste("minimum of ",R^2,"-values in each paper"))))
      p <- p +guides(colour=guide_legend(label.hjust=0,title.position="left" ))
p <- p  + scale_x_continuous(breaks=seq(1930,2010, by=20), 
                             limits=c(1930,2010))




        print(p)
        
fit <- lm(mean.rsq.mean~year,
          weights=count.paper.R2,
          data=all.data.summarized.by.year)
print(summary(fit))
coef(fit)[1]/coef(fit)[2]

fit <- lm(mean.rsq.max~year,
          weights=count.paper.R2,
          data=all.data.summarized.by.year)
print(summary(fit))
coef(fit)[1]/coef(fit)[2]


fit <- lm(mean.rsq.min~year,
          weights=count.paper.R2,
          data=all.data.summarized.by.year)
print(summary(fit))
coef(fit)[1]/coef(fit)[2]



graphics.off()