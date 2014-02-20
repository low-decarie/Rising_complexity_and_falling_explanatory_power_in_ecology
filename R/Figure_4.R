#Inspired from http://stats.stackexchange.com/questions/50171/sample-size-effects-on-r-squared

library(MASS)
library(plyr)
require(ggplot2)
source("./R/theme.R")
theme_set(theme_minimal())

set.seed(1818)

r<-NA
sample.size<-seq(5,100,1)

r.sim<-function(sample.size) {
  results <- rdply(.n=10,
                   .expr=function(){
    df<-data.frame(mvrnorm(sample.size[1],c(10,10),matrix(c(10,7,7,10),2),T))
    #correlation 0.7 so R2 = 0.49
    temp.results <- data.frame(r=summary(lm(X1 ~ X2, df))$r.squared,
                          p=summary(lm(X1 ~ X2, df))$coefficients[2,4],
                          denom.df=sample.size)
    return(temp.results)})
  return(results)
}       

sim.d<-ldply(.data=sample.size,
           .fun=r.sim,
           .progress="text")


sim.d$sig <- FALSE
sim.d$sig[sim.d$p<0.05] <- TRUE

summarized.sim.d <- ddply(.data=sim.d,
                          .variables=c("denom.df"),
                          function(x){
                            data.frame(lower95=t.test(x$r)$conf.int[1],
                                       upper95=t.test(x$r)$conf.int[2],
                                       max=max(x$r),
                                       min=min(x$r),
                                       mean.r=mean(x$r[x$sig]))})

summarized.sim.d$smooth.lower95 <- predict(with(summarized.sim.d, loess(lower95~denom.df,
                                                                        span=1/2,
                                                                        degree = 1)))
summarized.sim.d$smooth.upper95 <- predict(with(summarized.sim.d, loess(upper95~denom.df,
                                                                        span=1/2,
                                                                        degree = 1)))
summarized.sim.d$smooth.mean.r <- predict(with(summarized.sim.d, loess(mean.r~denom.df,
                                                                       span=1/2,
                                                                       degree = 1)))

sim.d.all <- join(sim.d, summarized.sim.d, type="full")

p <- qplot(data=sim.d.all,
           x=denom.df,
           ylab=expression(paste("Simulated ",R^2)),
           xlab="Sample size of simulation",
           ymax=smooth.upper95,
           ymin=smooth.lower95,
           geom="ribbon",
           alpha=I(0.3),
           guide = 'none')

p <- p + geom_point(aes(y=r,
                        shape=sig,
                        color=sig),
                    alpha=I(1))

p <- p + scale_shape_manual(name="Significant",
                            values=c("FALSE"=1,"TRUE"=16),
                            labels=c("No (P\u22650.05)",
                                     "Yes (P<0.05)"))


p <- p + geom_line(se=F,
                   colour=I("red"),
                   size=I(2),
                   alpha=I(1),
                   aes(y=smooth.mean.r), guide = 'none')


p <- p + scale_colour_brewer(name="Significant",
                             type="qualitative",
                             palette="Paired",
                             labels=c("No (P\u22650.05)",
                                      "Yes (P<0.05)"))

cairo_pdf("./Plots/Figure_4.pdf")
print(p)
graphics.off()

ggsave("./Plots/Figure_4.eps",p)

print(p)
