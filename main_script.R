### script for analysis of swiss xmas market data ###

setwd('/Users/calvinwhealton/GitHub/xmas_markets_ch')

# libraries
library(RColorBrewer)
library(grImport)
library(png)

# reading-in data
# data stored as csv files
timeline <- read.csv(file='market_timeline.csv'
                     ,header=T)
timeline <- timeline[-nrow(timeline),]
timeline <- timeline[,-3]

canton <- read.csv(file='canton_data.csv'
                   ,header=T)

#### general plot of markets with time ####
num_marks <- colSums(timeline[,-c(1,2)],na.rm=T)

# colors
cols_temp <- brewer.pal(n=6,'YlGnBu')

cols_bars <- rep(NA,length(num_marks))

cols_bars[1:16] <- cols_temp[1] # pre-advent
cols_bars[17] <- cols_temp[3] # 1st weekend advent
cols_bars[18:22] <- cols_temp[2] # week day advent
cols_bars[23:24] <- cols_temp[4] # 2nd weekend advent
cols_bars[25:29] <- cols_temp[2] # week day advent
cols_bars[30:31] <- cols_temp[5] # 3rd weekend advent
cols_bars[32:36] <- cols_temp[2] # week day advent
cols_bars[37:38] <- cols_temp[6] # 4th weekend advent
cols_bars[39:40] <- cols_temp[2] # week day advent
cols_bars[41] <- 'black' # christmas
cols_bars[42:53] <- cols_temp[1] # post-advent

png(filename='timeline.png'
    ,width=9
    ,height=3
    ,units='in'
    ,res=500)
par(mar=c(3,3,3,8))

barplot(num_marks
        ,col=cols_bars
        ,xlab=''
        ,ylab=''
        ,las=1
        ,main='Number of Christmas Markets'
        ,xaxt='n'
        ,width = 0.85
        ,space=0.15
        )
par(xpd=T)
legend(x=48
       ,y=30
       ,xjust = 0
       ,yjust=0.5
       ,legend=c('non-Advent','Advent weekday','Advent 1st Weekend','Advent 2nd Weekend','Advent 3rd Weekend','Advent 4th Weekend','Christmas Day')
       ,fill=c(cols_temp,'black')
       )

axis(side=1
     ,at=(seq(0,54,5)+0.5)/(55/54)
     ,c('Nov. 15','Nov. 20','Nov. 25', 'Nov. 30','Dec. 5','Dec. 10','Dec. 15','Dec. 20', 'Dec. 25','Dec. 30','Jan. 4'))
dev.off()


#### number of days per market #####
png(filename='histogram.png'
    ,width=6
    ,height=4
    ,units='in'
    ,res=500)
par(mar=c(4,4,4,1))
hist(rowSums(timeline[,-c(1,2)],na.rm=T)
     ,breaks = seq(0,44,2)
     ,col='seagreen'
     ,las=1
     ,xlab='Duration (days)'
     ,ylab='Count'
     ,main='Distribution of Market Durations')
dev.off()

### scatter of market days per person ###
cantons <- unique(timeline$Canton)
markets_canton <- rep(NA,length(cantons))
market_days_canton <- rep(NA,length(cantons))

for(i in 1:length(cantons)){
  
  markets_canton[i] <- sum(timeline$Canton==cantons[i])
  market_days_canton[i] <- sum((timeline$Canton==cantons[i])*rowSums(timeline[,-c(1,2)],na.rm=T))
  
}

# calculating market days per population
market_days_pop <- rep(NA,length(cantons))
markets_municipality <- rep(NA,length(cantons))

cantons_text <- unique(as.character(timeline$Canton))
canton$code_txt <- as.character(canton$code)

for(i in 1:length(cantons_text)){
  
  market_days_pop[i] <- market_days_canton[i]/as.numeric(canton$pop[which(canton$code_txt==cantons_text[i])])
  market_municipality[i] <- sum(timeline$canton_text == cantons_text[i])/canton$municipalities[which(canton$code_txt==cantons_text[i])]
  
}


png(filename='scatter.png'
    ,width=8
    ,height=8
    ,units='in'
    ,res=300)
par(mar=c(5,5,5,2))
plot(x=log(market_days_canton)
     ,y=log(market_municipality)
     ,xlab='Market Days Per Canton'
     ,ylab='Markets per Municipality'
     ,main='Cantonal Comparisons'
     ,xaxt='n'
     ,yaxt='n'
     )
rect(xleft=par('usr')[1],xright=par('usr')[2]
     ,ybottom=par('usr')[3],ytop=par('usr')[4]
     ,col='lightgray')
axis(side=1
     ,at=log(c(2,5,10,20,50,100,200))
     ,labels=c('2','5','10','20','50','100','200'))
axis(side=2
     ,at=log(c(0.02,0.04,0.06,0.08,0.1,0.2))
     ,labels=c('0.02','0.04','0.06','0.08','0.1','0.2')
     ,las=1)

rect(xleft=par('usr')[1])
for(i in 1:length(cantons_text)){
  
  img <- readPNG(paste(cantons_text[i],'.png',sep=''))
  rasterImage(image=img,xleft=log(market_days_canton[i])-0.15
              ,xright=log(market_days_canton[i])+.15
              ,ybottom=log(market_municipality[i])-0.1
              ,ytop=log(market_municipality[i])+0.1)
}
dev.off()


