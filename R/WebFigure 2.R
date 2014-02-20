rm(list=ls())
require(ggplot2)
require(plyr)
require(reshape)
require(data.table)
require(gridExtra)
require(quantreg)
require(tm)

source("./R/theme.R")
theme_set(theme_minimal())



all.rsq<-read.csv("./Data/rsq.csv", 
                  header=F,
                  col.names=c("file","rsq"))


all.rsq.summarized <- ddply(.data=all.rsq,
                            .variables=c("file"),
                            function(x){
                              temp <- with(x,data.frame(rsq.mean=mean(rsq),
                                                        rsq.max=max(rsq),
                                                        rsq.min=min(rsq)))
                              return(temp)
                            })


load("./Data/article_metadata.RData")

topics <- read.csv("Output/topic_model_classes.csv")
topic_names <- read.csv("Output/topic_manual_names.csv", as.is=T)
topics <- merge(topics,topic_names)
topics$topic_name <- gsub("\\n","\n",topics$topic_name, fixed=T)

article_metadata <- merge(article_metadata, topics)

#Select only articles with high classification scores
# qplot(data=article_metadata, x=score, fill=topic_name,position="dodge", binwidth=0.1)
# article_metadata <- article_metadata[article_metadata$score>0.6,]



topic_r2 <- function(topic_name="Molecular ecology"){
  
  print(topic_name)
  
  selected.article_metadata <- article_metadata[article_metadata$topic_name==topic_name,]
  
  selected.data.summarized<-merge(selected.article_metadata,all.rsq.summarized)
    
    all.data.summarized.by.year <- ddply(.data=selected.data.summarized,
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
    
    
    fit <- lm(mean.rsq.mean~year,
              weights=count.paper.R2,
              data=all.data.summarized.by.year)
    slope.mean.rsq.mean <- coef(fit)[2]
    p.slope.mean.rsq.mean <- summary(fit)$coefficients[2,4]
    rsq.slope.mean.rsq.mean <- summary(fit)$r.squared
    
    fit <- lm(mean.rsq.max~year,
              weights=count.paper.R2,
              data=all.data.summarized.by.year)
    slope.mean.rsq.max <- coef(fit)[2]
    
    
    fit <- lm(mean.rsq.min~year,
              weights=count.paper.R2,
              data=all.data.summarized.by.year)
    slope.mean.rsq.min <- coef(fit)[2]
    
    topic.slopes <- data.frame(topic_name=topic_name,
                               rsq.slope.mean.rsq.mean=rsq.slope.mean.rsq.mean,
                               mean.rsq.mean=mean(selected.data.summarized$rsq.mean),
                               p.slope.mean.rsq.mean=p.slope.mean.rsq.mean,
                               slope.mean.rsq.mean=slope.mean.rsq.mean,
                               slope.mean.rsq.max=slope.mean.rsq.max,
                               slope.mean.rsq.min=slope.mean.rsq.min,
                               num.articles=nrow(selected.data.summarized))
    
  
  return(topic.slopes)
  
}

topic.list <- unique(article_metadata$topic_name)

topic.slopes <- ldply(.data=topic.list,
                      topic_r2,
                      .progress="text")


topic.slopes$sig.slope.mean.rsq.mean <- topic.slopes$p.slope.mean.rsq.mean<0.05

topic.slopes.molten <- melt(topic.slopes, id.vars=c("topic_name",
                                                    "num.articles",
                                                    "mean.rsq.mean",
                                                    "p.slope.mean.rsq.mean",
                                                    "sig.slope.mean.rsq.mean",
                                                    "rsq.slope.mean.rsq.mean"))




pdf("./Plots/WebFigure 2.pdf", width=9)


topic.slopes.molten$topic_name <- reorder(topic.slopes.molten$topic_name,topic.slopes.molten$num.articles)

topic.slopes$topic_name <- reorder(topic.slopes$topic_name,topic.slopes$num.articles)

p <- qplot(data=topic.slopes.molten,
           x=topic_name,
           y=value,
           xlab="Topic",
           ylab=expression(paste("Slope of regression (",year^-1,")",sep="")),
           alpha=sig.slope.mean.rsq.mean,
           fill=variable,
           geom="bar",
           position="dodge",
           stat="identity")

p <- p + scale_fill_brewer(name="Slopes over years of",
                           type="qualitative",
                           palette="Paired",
         labels=c(expression(paste("maximum of ",R^2,"-values in each paper")),
                  expression(paste("mean of ",R^2,"-values in each paper")),
                  expression(paste("minimum of ",R^2,"-values in each paper"))))



p <- p + theme(legend.position=c(0.8,0.8),
               legend.text=element_text(size=7),
               legend.title.align=0,
               legend.title=element_text(size=7),
               legend.box.just="left")+scale_alpha_discrete(name="Regression significant for mean values",range = c(0.3, 1))

p <- p+coord_flip()

R2.trend.plot <- p


p <- qplot(data=topic.slopes,
           x=topic_name,
           y=num.articles,
           xlab="Topic",
           ylab="Number of articles",
           geom="bar",
           stat="identity")

p <- p+coord_flip()+theme(plot.margin=unit(c(1,1,1,0), "cm"),
                            panel.margin=unit(c(1,1,0,1), "cm"),
                          axis.text.y=element_blank(),
                          axis.title.y=element_blank(),
                          axis.ticks.y=element_blank())

number.articles.plot <- p




R2.trend.plot <- ggplot_gtable(ggplot_build(R2.trend.plot))
number.articles.plot <- ggplot_gtable(ggplot_build(number.articles.plot))
maxheights = grid::unit.pmax(R2.trend.plot$heights, number.articles.plot$heights)
R2.trend.plot$heights <- as.list(maxheights)
number.articles.plot$heights <- as.list(maxheights)


grid.arrange(R2.trend.plot,
             number.articles.plot,
             widths=c(4, 1),
             ncol=2)

graphics.off()

