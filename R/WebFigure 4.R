rm(list=ls())
require(ggplot2)
require(plyr)
require(gridExtra)
require(reshape)

source("./R/theme.R")
theme_set(theme_minimal())

pdf("./Plots/WebFigure 4.pdf")


all.f<-read.csv("./Data/f_ratio.csv", 
                header=F,stringsAsFactors=F,
                col.names=c("file","numerator", "denominator", "fratio"))

load("./Data/article_metadata.RData")
all.data<-merge(article_metadata,all.f)

all.data <- all.data[all.data$denominator>0,]

# Mean per article ####


all.data.mean.per.file <- ddply(.data=all.data,
                       .variables=c("file","year"),
                       function(x){
                           data.frame(mean=mean(x$denominator, na.rm=T),
                                      max=max(x$denominator, na.rm=T),
                                      min=min(x$denominator, na.rm=T),
                                      SE=sd(x$denominator, na.rm=T)/sqrt(length(x$denominator)))})

all.data.mean.per.year <- ddply(.data=all.data.mean.per.file,
                                .variables=c("year"),
                                function(x){
                                  data.frame(mean.mean=mean(x$mean, na.rm=T),
                                             mean.max=max(x$max, na.rm=T),
                                             mean.min=min(x$min, na.rm=T),
                                             SE=sd(x$mean)/sqrt(length(x$mean)),
                                             number.articles=length(x$mean))})

molten.all.data.mean.per.year <- melt(all.data.mean.per.year,
                                      measure.vars=c("mean.mean",
                                                     "mean.max",
                                                     "mean.min"))

max.n.year <- max(molten.all.data.mean.per.year$number.articles)


data.with.model <- ddply(.data=molten.all.data.mean.per.year,
                                            .variables="variable",
                                            .progress="text",
                                            function(x){
                                              fit <- lm(log10(value)~year,
                                                        weights=number.articles,
                                                        data=x)
                                              return(data.frame(x,
                                                                predicted=predict(fit),
                                                                intercept=fit$coefficients[1],
                                                                slope=fit$coefficients[2]))})


p <- qplot(data=data.with.model[data.with.model$value>0,],
           x=year,
           xlab="Year",
           y=log10(value),
           alpha=number.articles/max.n.year,
           geom="pointrange",
           ymin=log10(value-SE),
           ymax=log10(value+SE),
           ylab=expression(paste(Log[10]," of mean F-ratio denominator degrees of freedom (sample size)")),
           colour=variable)+
  scale_x_continuous(breaks=seq(1930,2010, by=20), 
                     limits=c(1930,2010))

p <- p + scale_alpha_continuous(range = c(0.2, 1), guide="none")

p <- p + scale_colour_brewer(name="Yearly mean of the   ",
                               type="qualitative",
                               palette="Paired",
                               labels=c(expression(paste("maximum denominator in each paper")),
                      expression(paste("mean denominatorin each paper")),
                        expression(paste("minimum denominator in each paper"))))


p <- p + geom_line(aes(y=predicted))

p <- p +guides(colour=guide_legend(ncol=2, label.hjust=0,title.position="left" ))

p <- p + theme(legend.position="bottom")




print(p)

ggsave(filename = "./Plots/Supplemental figure 2_1.eps",
       plot = p)



data.with.linear.model <- ddply(.data=molten.all.data.mean.per.year,
                                       .variables="variable",
                                       .progress="text",
                                       function(x){
                                         fit <- lm(value~year,
                                                   weights=number.articles,
                                                   data=x)
                                         return(data.frame(x,
                                                           predicted=predict(fit),
                                                           intercept=fit$coefficients[1],
                                                           slope=fit$coefficients[2]))})

p <- qplot(data=data.with.linear.model[data.with.linear.model$value>0 & data.with.linear.model$variable=="mean.mean",],
           x=year,
           xlab="Year",
           y=value,
           alpha=number.articles/max.n.year,
           geom="pointrange",
           ymin=value-SE,
           ymax=value+SE,
           ylab=expression(paste("Mean F-ratio denominator degrees of freedom (sample size)")))

p <- p + scale_alpha_continuous(range = c(0.5, 1), guide="none")

p <- p + geom_line(aes(y=predicted))
p <- p + scale_y_log10()
           
print(p)



graphics.off()



ggsave(filename = "./Plots/Supplemental figure 2_2.eps",
       plot = p)



fit.mean <- with(all.data.mean.per.year, lm(mean.mean~year, weights=number.articles))
summary(fit.mean)
fit.mean.log <- with(all.data.mean.per.year, lm(log2(mean.mean)~year, weights=number.articles))
summary(fit.mean.log)
fit.max <- with(all.data.mean.per.year, lm(log2(mean.max)~year, weights=number.articles))
summary(fit.max)
fit.min <- with(all.data.mean.per.year, lm(log2(mean.min)~year, weights=number.articles))
summary(fit.min)


print(paste("Mean value increases by", round(coef(fit.mean)[2],1), "units every year"))
print(paste("Mean value doubles every", round(1/coef(fit.mean.log)[2]), "years"))
print(paste("Maximum value doubles every", round(1/coef(fit.max)[2]), "years"))
print(paste("Minimum value is halved  every", round(-1/coef(fit.min)[2]), "years"))