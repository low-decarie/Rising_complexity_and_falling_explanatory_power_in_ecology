rm(list=ls())
require(ggplot2)
require(gridExtra)
library(scales)

source("./R/theme.R")
theme_set(theme_minimal())

load("./Data/article_metadata.RData")



p_values<-read.csv("./Data/all_p_values.csv", 
                       header=F,
                       col.names=c("file","all_p_values"))

f_ratio<-read.csv("./Data/f_ratio.csv", 
                   header=F,
                   col.names=c("file","f_numerator", "f_denominator", "f_ratio"))
corr_r<-read.csv("./Data/corr_r.csv", 
                       header=F,
                       col.names=c("file","corr_r"))
rsq<-read.csv("./Data/rsq.csv", 
                       header=F,
                       col.names=c("file","rsq"))



all.data <- article_metadata
all.data$has.p_values<-all.data$file %in% p_values$file
all.data$has.f_ratio<-all.data$file %in% p_values$file
all.data$has.corr_r<-all.data$file %in% corr_r$file
all.data$has.rsq<-all.data$file %in% rsq$file

all.data$has.an.r  <- all.data$has.corr_r|all.data$has.rsq
all.data$has.p_values  <- all.data$has.p_values|all.data$has.f_ratio

all.data$p.vs.r <- "Neither"
all.data$p.vs.r[all.data$has.an.r & all.data$has.p_values]<- "Both p and R2"
all.data$p.vs.r[all.data$has.an.r & !all.data$has.p_values]<- "Only R2"
all.data$p.vs.r[!all.data$has.an.r & all.data$has.p_values]<- "Only p"
all.data$p.vs.r<-as.factor(all.data$p.vs.r)

articles.per.year <- data.frame(table(all.data$year))
names(articles.per.year) <- c("year", "number.of.articles")
all.data <- merge(all.data, articles.per.year)

max.articles <- with(all.data, max(number.of.articles))



p.vs.r.plot <- qplot(data=all.data,
                     x=year,
                     xlab="",
                     fill=reorder(p.vs.r,p.vs.r,function(x)-length(x)),
                     geom="bar",
                     binwidth=1,
                     position="fill",
                     #alpha=mean(number.of.articles/max.articles),
                     ylab="Relative frequency")+
  scale_fill_brewer(name="Statistics present in article    ",
                     labels=c("Neither",
                              "Only p-values      ",
                              expression(paste("Both p and ",R^2, " values")),
                              expression(paste("Only ",R^2,"-values"))),
                    type="qualitative",
                           palette="Paired")+
  scale_y_continuous(labels = percent)+
  theme(axis.text.x=element_blank(),
        #legend.position="none",
        legend.position="top",
        axis.ticks.x=element_blank(),
        plot.margin=unit(c(1,1,-0.5,1), "cm"),
        panel.margin=unit(c(1,1,0,1), "cm"))+
  guides(fill=guide_legend(ncol=2, keywidth = 0.5,label.hjust=0))+ 
  scale_x_continuous(breaks=seq(1930,2010, by=20), 
                     limits=c(1930,2010))


article.counts <- qplot(data=all.data,
                     x=year,
                     xlab="Year",
                     geom="bar",
                     binwidth=1,
                        #fill=periodical,
                     ylab="Number of articles")+
  theme(plot.margin=unit(c(-0.5,1,1,1), "cm"),
        panel.margin=unit(c(1,1,0,1), "cm"))+ 
  scale_x_continuous(breaks=seq(1930,2010, by=20), 
                     limits=c(1930,2010))


p.vs.r.plot <- ggplot_gtable(ggplot_build(p.vs.r.plot))
article.counts <- ggplot_gtable(ggplot_build(article.counts))
maxWidth = grid::unit.pmax(p.vs.r.plot$widths[2:3], article.counts$widths[2:3])
p.vs.r.plot$widths[2:3] <- as.list(maxWidth)
article.counts$widths[2:3] <- as.list(maxWidth)

pdf("./Plots/Figure_1.pdf")

grid.arrange(p.vs.r.plot,
            article.counts,
             heights=c(4, 2),
             ncol=1)

graphics.off()

nrow(all.data[all.data$year>=1999 & all.data$year<=2009 & all.data$p.vs.r=="Only p",])/
  nrow(all.data[all.data$year>=1999 & all.data$year<=2009 & all.data$p.vs.r!="Neither",])