#######################################################################################
##  Filename: bw_fightinwords.r
##  Purpose: Examine language differences between NFL scouting reports
##  about black prospects and white prospects.
##  Extends: http://deadspin.com/which-words-are-used-to-describe-white-and-black-nfl-pr-1573683214
##  Uses data from: https://github.com/reubenfb/Deadspin_Draftwords
##  Assumes packages:dplyr,ggplot2,RCurl
##  To install these packages run:
##  install.packages(c('dplyr','stringr','RCurl','ggplot2'))
##  Output to figures/fightinwords.png, figures/topwords.png
##  Last Edited: 29 January 2015
##  Christopher Boylan, Penn State University
#######################################################################################
## Load Packages and Prepare Directory
#######################################################################################
rm(list=ls())
library(RCurl); library(dplyr); library(ggplot2); library(stringr)
path <- unlist(str_split(getwd(), "/")) ## get path
dir <- ifelse(path[length(path)] == "R", "../", "./") ## set directory 
#######################################################################################
## Get and prepare data
#######################################################################################
## Get data from github repo for deadspin article
url1 <- getURL("https://raw.githubusercontent.com/reubenfb/Deadspin_Draftwords/master/blackcount.csv")
black <- read.csv(text = url1)
url2 <- getURL("https://raw.githubusercontent.com/reubenfb/Deadspin_Draftwords/master/whitecount.csv")
white <- read.csv(text = url2)
## get all words used
words <- union(black$word,white$word)#
## number of total words used
W <- length(words)
## create matrix with 2 rows and W columns
y <- matrix(0,2,W)
## use words as colnames
colnames(y) <- words
rownames(y) <- c("black","white")
## assign word counts to empty columns
y[1,as.character(black$word)] <- black$count
y[2,as.character(white$word)] <- white$count
## Total number times each word was used
y.tot <- colSums(y)
## Total number of words used in the corpus
Y <- sum(y.tot)
#######################################################################################
## Use Monroe et al.'s (2008) method to examine differences in words usage
#######################################################################################
## Prior (in this case an uninformative prior)
kappa <- 0.01
y.prior <- y.tot/Y
k.prior <- kappa*y.prior
## numerator of estimate for b
y.1.a <- y[1,] + k.prior*sum(y[1,])/Y
## numerator of estimate for w
y.2.a <- y[2,] + k.prior*sum(y[2,])/Y
## log-odds estimate for b
theta.1 <- log(y.1.a/(sum(y.1.a)-y.1.a))
##  log-odds estimate for w
theta.2 <- log(y.2.a/(sum(y.2.a)-y.2.a))
delta <- theta.1 - theta.2
## standard error of estimate
se <- sqrt(1/y.1.a + 1/y.2.a)
## z-statistic
zeta <- delta/se
## Top 25 words
M <- 25
max.za.b <- sort.list(-zeta)[1:M] ## top 25 black words based on zeta scores
max.za.w <- sort.list(zeta)[1:M] ## top 25 white words based on zeta scores
bwords <- names(zeta[max.za.b]) ## strings of top 25 black words
wwords <- names(zeta[max.za.w]) ## string of top 25 white words
#######################################################################################
## Plot results of above procedure
#######################################################################################
#######################################################################################
## Plot top 25 words
#######################################################################################
df <- data.frame() ## empty df for plain plot
p1 <- ggplot(df) + 
  geom_point() + ## empty plot
  theme_bw() +  ## bw theme
  xlim(0, 10) + ## set x-axis bounds
  ylim(0, 25) + ## set y-axis bounds
  geom_vline(xintercept=5, linetype="dashed") + ## dashed vertical line
  ggtitle("Top 25 Words") + ## title
  annotate("text",label = bwords, size = 6.5, x = 2.5, y = 25:1, colour = "orange") + ## black words
  annotate("text",label = wwords, size = 6.5, x = 7.5, y = 25:1, colour = "royalblue") + ## white words
  annotate("text",label = "Black Players", size = 4.5, x = 0.4, y = 12.5, colour = "orange") + ## b label
  annotate("text",label = "White Players", size = 4.5, x = 9.6, y = 12.5, colour = "royalblue") + ## w label
  theme(legend.position="none", ## no legend
        title = element_text(size=20,face="bold"), ## title font size and face
        panel.border = element_blank(), ## no border
        panel.grid.major = element_blank(), ## no grid
        panel.grid.minor = element_blank(), ## no grid
        axis.title.x = element_blank(), ## no axis title
        axis.title.y =element_blank(), ## no axis title
        axis.text.x =element_blank(), ## no axis text
        axis.text.y =element_blank(), ## no axis text
        axis.ticks = element_blank()) ## no axis ticks
ggsave('figures/topwords.png',p1, width = 11,height = 8, units = 'in')
#######################################################################################
## Plot estimates against word frequency as in Monroe et al.'s (2008) article
#######################################################################################
## Setup data frame for plotting
plotdf <- as.data.frame(cbind(log(y.tot),zeta)) ## log scale for plotting
plotdf$words <- rownames(plotdf)
colnames(plotdf)[1] <- 'freq'
## remove stop words that have high counts to improve aesthetics
plotdf <- filter(plotdf, freq<7.5)
## create variables for plot
plotdf$bwords <- ifelse(plotdf$zeta > 0, 1, 0) ## >0 are associated with black prospects
plotdf$bwords[abs(plotdf$zeta) < 1.96] <- 2 ## >1.96 are significant
## create cutoffs for plot
plotdf$size <- 0
plotdf$size[abs(plotdf$zeta) < 1.96] <- 1
plotdf$size[abs(plotdf$zeta) >= 1.96 & abs(plotdf$zeta) <= 2.5] <- 2
plotdf$size[abs(plotdf$zeta) > 2.5 & abs(plotdf$zeta) <= 3.5] <- 3
plotdf$size[abs(plotdf$zeta) > 3.5 & abs(plotdf$zeta) <= 5] <- 4
plotdf$size[abs(plotdf$zeta) > 5] <- 5
## Plot logged counts and zeta scores
p2 <- ggplot(plotdf,aes(x=freq, y=zeta, label=words)) +
  theme_bw() + ## bw gg theme
  geom_text(aes(size=size, colour=factor(bwords))) + ## control size, colour
  scale_size_continuous(range = c(1,8)) + ## size for text
  xlab("Frequency of word use") + ## x label
  ylab("Estimated difference in word use") + ## y label
  scale_color_manual(values=c("royalblue", "orange","grey")) + ## colours
  scale_x_continuous(breaks=c(log(1), log(10), log(100),log(1000)), ## x-axis points
                     labels=c("1", "10", "100", "1000"))+  ## x-axis labels
  theme(legend.position="none", ## no legend
        axis.title.x = element_text(size=20,face="bold"), ## axis size and face
        axis.title.y = element_text(size=20,face="bold"), ## axis size and face
        axis.text.x =element_text(size=15), ## label size
        axis.text.y =element_text(size=15), ## label size
        panel.border = element_blank(), ## no border
        panel.grid.major = element_blank(), ## no grid
        panel.grid.minor = element_blank()) + ## no grid
  annotate("text", x = c(.25, .25), y = c(9.8,-9.8), label = c("Black","White"), ## label
           colour = c("orange","royalblue"), fontface="bold",size=6) ## label colours

ggsave('figures/fightinwords.png',p2, width = 13,height = 9, units = 'in')