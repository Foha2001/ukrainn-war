#**********************************************
# *                                                 #
#     Volatility and return spillovers among global assets: 
#  Comparing health crisis with geopolitical crisis
#*********************************************************
#***********************************************************
#*********************import data*******************####
#*************************************************

library(quantmod)
library(xts)
library(zoo)
w_indice <- c("^GSPC","000001.SS","^N225","^GDAXI","^NSEI","^FTSE",
              "^FCHI","FTSEMIB.MI","^BVSP","^GSPTSE","^KS11","IMOEX.ME",
              "^MXX","XU100.IS","M.BA","^JKII","BTC-USD","ETH-USD",
              "USDT-USD","CL=F","ALI=F","GC=F","SB=F","KE=F", "ZS=F",
              "ZC=F","NG=F")

start_date <- "2010-01-01"
end_date <- "2023-03-08"
envt<- new.env()
getSymbols(w_indice,env=envt,from=start_date, to=end_date)
data <- do.call(merge, eapply(envt, Cl))
library(zoo)
#datarepl <- na.locf(data, fromLast = TRUE) # replace missing value with next one
#*descriptive statistics------------------####

R_data <- diff(log(data))
#R_data <- na.omit(R_data)  #delete NA 
colnames(R_data) <- c("Brasil","Indonesia","NaturalGas","Soybean","Japan",
                      "India","Turkey","China","Bitcoin",
                      "france","Italy","UK","oil","Canada",
                      "Germany","Agentina","corn","Aluminium",
                      "Wheat","Gold","Ethereum","Southkorea","Sugar","Mexico",
                      "Tehther","Russia","US")

library(fBasics)
library(moments)
desc <- do.call(data.frame, 
               list(mean = apply(R_data, 2, mean,na.rm=TRUE),
                    sd = apply(R_data, 2, sd,na.rm=TRUE),
                    median = apply(R_data, 2, median,na.rm=TRUE),
                    min = apply(R_data, 2, min,na.rm=TRUE),
                    max = apply(R_data, 2, max,na.rm=TRUE),
                    skew = apply(R_data, 2, skewness,na.rm = TRUE),
                    kurt = apply(R_data,2,kurtosis, na.rm=TRUE)))
desc


desc <- cbind(rownames(desc),desc)
library(writexl)
write_xlsx(desc,"descstatistics1.xlsx") 

#********normality test************##
#*
library(fBasics)
R_data <- na.locf(R_data, fromLast = TRUE)
R_data <- na.omit(R_data)
normalTest(R_data$Agentina,method="jb")





##**************************************************************#
#*********************************************************####
#
#****           Time Varying Parameter *** #####  
#               *Balcilar, Gabauer and Umar (2021) 
##***************************************************************#
#*#####################################################***#
#*************************TVP by return******************** ####
#*####################################################****#


#Return between markets####
Rcomm <- R_data[,c(13,18,20,23,19,4,17,3)]
Rcomm <- na.locf(Rcomm, fromLast = TRUE)
Rcomm <- na.omit(Rcomm)
Reur <- R_data[,c(10,11,12,15,26)]
Reur <- na.locf(Reur, fromLast = TRUE)
Reur <- na.omit(Reur)
Rasia <- R_data[,c(2,5,6,8,22)]
Rasia <- na.locf(Rasia, fromLast = TRUE)
Rasia <- na.omit(Rasia)
Ramer <-  R_data[,c(1,14,16,24,27)]
Ramer <- na.locf(Ramer, fromLast = TRUE)
Ramer <- na.omit(Ramer)
Rcryp <- R_data[,c(9,21,25)]
Rcryp <- na.locf(Rcryp, fromLast = TRUE)
Rcryp <- na.omit(Rcryp)
# average return
Rcomm$ave <- rowMeans(Rcomm)
Reur$ave <- rowMeans(Reur)
Rasia$ave <- rowMeans(Rasia)
Ramer$ave <- rowMeans(Ramer)
Rcryp$ave <- rowMeans(Rcryp)
Rmarkets <- cbind(Rcomm$ave,Reur$ave,Rasia$ave,Ramer$ave,
                  Rcryp$ave) 
colnames(Rmarkets) <- c("Commoditymarkets","Europemarkets",
                        "Asianmarkets","Americanmarkets","cryptomarkets")
Rmarkets <- na.omit(Rmarkets)

#*connectedness####
library(ConnectednessApproach)
bgu_R_market = ConnectednessApproach(Rmarkets, 
                                   nlag=1, 
                                   nfore=12,
                                   model="TVP-VAR",
                                   connectedness="Extended Joint",
                                   VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
bgutotal_R_markets<- as.data.frame(bgu_R_market$TCI)
bgutotal_R_markets <- cbind(rownames(bgutotal_R_markets),bgutotal_R_markets)
library(writexl)
write_xlsx(bgutotal_R_markets,"bgutotal_R_markets.xlsx")
bgutotal_R_marketsTable<- as.data.frame(bgu_R_market$TABLE)
bgutotal_R_marketsTable <- cbind(rownames(bgutotal_R_marketsTable),bgutotal_R_marketsTable)
write_xlsx(bgutotal_R_marketsTable,"bgutotal_R_marketsTable.xlsx")

bgutotal_R_markets_from<- as.data.frame(bgu_R_market$FROM)
bgutotal_R_markets_from <- cbind(rownames(bgutotal_R_markets_from),bgutotal_R_markets_from)
write_xlsx(bgutotal_R_markets_from,"bgutotal_R_markets_from.xlsx")

bgutotal_R_markets_to<- as.data.frame(bgu_R_market$TO)
bgutotal_R_markets_to <- cbind(rownames(bgutotal_R_markets_to),bgutotal_R_markets_to)
write_xlsx(bgutotal_R_markets_to,"bgutotal_R_markets_to.xlsx")

bgutotal_R_markets_net<- as.data.frame(bgu_R_market$NET)
bgutotal_R_markets_net <- cbind(rownames(bgutotal_R_markets_net),bgutotal_R_markets_net)
write_xlsx(bgutotal_R_markets_net,"bgutotal_R_markets_net.xlsx")

# connectedness between assets####
R_data <- diff(log(data)) #this is because we allocate > 17GO PC limit
R_assets <- na.omit(R_data)

colnames(R_assets) <- c("Brasil","Indonesia","NaturalGas","Soybean","Japan",
                    "India","Turkey","China","Bitcoin",
                    "france","Italy","UK","oil","Canada",
                    "Germany","Agentina","corn","Aluminium",
                    "Wheat","Gold","Ethereum","Southkorea","Sugar","Mexico",
                    "Tehther","Russia","US")
library(ConnectednessApproach)
#delete 14/07/2022 wrong data 
R_assets_corr <- subset(R_assets, index(R_assets) != as.Date("2022-07-14"))
bgu_R_assets = ConnectednessApproach(R_assets_corr, 
                                     nlag=1, 
                                     nfore=12,
                                     model="TVP-VAR",
                                     connectedness="Extended Joint",
                                     VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
library(writexl)
gbu_spill_assets <- as.data.frame(bgu_R_assets$TABLE)
gbu_spill_assets <- cbind(rownames(gbu_spill_assets),gbu_spill_assets)
write_xlsx(gbu_spill_assets,"bgu_spill_assets.xlsx") 


#*graph for all assets*#### 
library(igraph)
#please format as csv in stata 
linksall <- read.csv("bgu_spill_assets.csv", header=T, row.names=1)
linksall <- as.matrix(linksall)
nodesall <- read.csv("nodesall.csv", header=T, row.names=1)
netall <- graph_from_adjacency_matrix(linksall, weighted =TRUE)
netall <- simplify(netall, remove.multiple = F, remove.loops = T) # remove loops
colrs <- c("tomato", "gold")
V(netall)$size <- nodesall$size
V(netall)$color <- colrs[nodesall$type]
E(netall)$width <- E(netall)$weight/10
#net$arrow.size <- E(net)$weight/2
E(netall)$arrow.size <- 0.05
E(netall)$edge.color <- "gray80"
  E(netall)$width <- E(netall)$weight/10
  plot(netall, vertex.label.cex=0.8,
       remove.loops = T,layout=layout.circle)
  tkid <- tkplot(netall) #for fixing coordinates
#-----STEP 1-------#
   l <- tkplot.getcoords(tkid)
  tk_close(tkid, window.close = T)
#-----STEP 2-------#
  
   tiff("fig1R.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
  plot(netall, layout=l)
  legend(x=-1, y=-1, c("Net receiver", "Net transmitter"), 
         pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
  dev.off()
  

#*subset dates before and after for 4  markets**####
Rmarketsbefore_covid <- subset(Rmarkets,
                               index(Rmarkets) <= as.Date("2020-03-11") &
                                 index(Rmarkets) >= as.Date("2019-11-11"))
  
Rmarketsafter_covid <- subset(Rmarkets,
                                index(Rmarkets) <= as.Date("2021-07-11") &
                                  index(Rmarkets) >= as.Date("2020-03-11"))

Rmarketsbefore_war <- subset(Rmarkets,
                             index(Rmarkets) <= as.Date("2021-11-28") &
                               index(Rmarkets) >= as.Date("2021-07-28"))

Rmarketsafter_war <- subset(Rmarkets,
                             index(Rmarkets) <= as.Date("2022-03-28") &
                               index(Rmarkets) >= as.Date("2021-11-28"))  
#**connectedness before and after for 4 markets**####
library(ConnectednessApproach)
#before covid
bgu_R_market_before_covid = ConnectednessApproach(Rmarketsbefore_covid, 
                                     nlag=1, 
                                     nfore=12,
                                     model="TVP-VAR",
                                     connectedness="Extended Joint",
                                     VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))

bgutotal_R_markets_before_covid_Table<- as.data.frame(
  bgu_R_market_before_covid$TABLE)
bgutotal_R_markets_before_covid_Table <- cbind(
  rownames(bgutotal_R_markets_before_covid_Table),
  bgutotal_R_markets_before_covid_Table)
library(writexl)
write_xlsx(bgutotal_R_markets_before_covid_Table,
           "bgutotal_R_markets_before_covid_Table.xlsx")
#after covid
bgu_R_market_after_covid = ConnectednessApproach(Rmarketsafter_covid, 
                                                  nlag=1, 
                                                  nfore=12,
                                                  model="TVP-VAR",
                                                  connectedness="Extended Joint",
                                                  VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
bgutotal_R_markets_after_covid_Table<- as.data.frame(
  bgu_R_market_after_covid$TABLE)
bgutotal_R_markets_after_covid_Table <- cbind(
  rownames(bgutotal_R_markets_after_covid_Table),
  bgutotal_R_markets_after_covid_Table)
library(writexl)
write_xlsx(bgutotal_R_markets_after_covid_Table,
           "bgutotal_R_markets_after_covid_Table.xlsx")
#before war

bgu_R_market_before_war = ConnectednessApproach(Rmarketsbefore_war, 
                                                  nlag=1, 
                                                  nfore=12,
                                                  model="TVP-VAR",
                                                  connectedness="Extended Joint",
                                                  VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))

bgutotal_R_markets_before_war_Table<- as.data.frame(
  bgu_R_market_before_war$TABLE)
bgutotal_R_markets_before_war_Table <- cbind(
  rownames(bgutotal_R_markets_before_war_Table),
  bgutotal_R_markets_before_war_Table)
library(writexl)
write_xlsx(bgutotal_R_markets_before_war_Table,
           "bgutotal_R_markets_before_war_Table.xlsx")

#after war

bgu_R_market_after_war = ConnectednessApproach(Rmarketsafter_war, 
                                                 nlag=1, 
                                                 nfore=12,
                                                 model="TVP-VAR",
                                                 connectedness="Extended Joint",
                                                 VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
bgutotal_R_markets_after_war_Table<- as.data.frame(
  bgu_R_market_after_war$TABLE)
bgutotal_R_markets_after_war_Table <- cbind(
  rownames(bgutotal_R_markets_after_war_Table),
  bgutotal_R_markets_after_war_Table)
library(writexl)
write_xlsx(bgutotal_R_markets_after_war_Table,
           "bgutotal_R_markets_after_war_Table.xlsx")

#**subset dates before and after for each assets##########

Rassets_before_covid <- subset(R_assets,
                               index(R_assets) <= as.Date("2020-03-11") &
                                 index(R_assets) >= as.Date("2019-11-11"))
Rassets_after_covid <- subset(R_assets,
                              index(R_assets) <= as.Date("2021-07-11") &
                                index(R_assets) >= as.Date("2020-03-11"))
Rassets_before_war <- subset(R_assets,
                             index(R_assets) <= as.Date("2021-11-28") &
                               index(R_assets) >= as.Date("2021-07-28"))

Rassets_after_war <- subset(R_assets,
                            index(R_assets) <= as.Date("2022-03-28") &
                              index(R_assets) >= as.Date("2021-11-28")) 

#*
#*
#**connectedness before and after each assets and plot results####

#*----------------------before covid#####
library(ConnectednessApproach)
bgu_R_assets_before_covid = ConnectednessApproach(Rassets_before_covid, 
                                                  nlag=1, 
                                                  nfore=12,
                                                  model="TVP-VAR",
                                                  connectedness="Extended Joint",
                                                  VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))

bgutotal_R_assets_before_covid_Table<- as.data.frame(
  bgu_R_assets_before_covid$TABLE)

bgutotal_R_assets_before_covid_Table <- cbind(
  rownames(bgutotal_R_assets_before_covid_Table),
  bgutotal_R_assets_before_covid_Table)
library(writexl)
write_xlsx(bgutotal_R_assets_before_covid_Table,
           "bgutotal_R_assets_before_covid_Table.xlsx")
#plot graph

library(igraph)
#please format as csv in stata 
linksall <- read.csv("bgu_Rspill_assets_b_covid.csv", header=T, row.names=1)
linksall <- as.matrix(linksall)
nodesall <- read.csv("nodesall.csv", header=T, row.names=1)
netall <- graph_from_adjacency_matrix(linksall, weighted =TRUE)
netall <- simplify(netall, remove.multiple = F, remove.loops = T) # remove loops
colrs <- c("tomato", "gold")
V(netall)$size <- nodesall$size/2
V(netall)$color <- colrs[nodesall$type]
E(netall)$width <- E(netall)$weight/10
#net$arrow.size <- E(net)$weight/2
E(netall)$arrow.size <- 0.05
E(netall)$edge.color <- "gray80"
  E(netall)$width <- E(netall)$weight/10
  plot(netall, vertex.label.cex=0.8,
       remove.loops = T,layout=layout.circle)
  tkid <- tkplot(netall) #for fixing coordinates
  #-----STEP 1-------#
  l <- tkplot.getcoords(tkid)
  tk_close(tkid, window.close = T)
  #-----STEP 2-------#
  
  tiff("fig7a.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
  plot(netall, layout=l)
  legend(x=-1, y=-1, c("Net receiver", "Net transmitter"), 
         pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
  dev.off()
  
#*
#*----------------------after covid#####
library(ConnectednessApproach)
  bgu_R_assets_after_covid = ConnectednessApproach(Rassets_after_covid, 
                                                    nlag=1, 
                                                    nfore=12,
                                                    model="TVP-VAR",
                                                    connectedness="Extended Joint",
                                                    VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
  
  bgutotal_R_assets_after_covid_Table<- as.data.frame(
    bgu_R_assets_after_covid$TABLE)
  
  bgutotal_R_assets_after_covid_Table <- cbind(
    rownames(bgutotal_R_assets_after_covid_Table),
    bgutotal_R_assets_after_covid_Table)
  library(writexl)
  write_xlsx(bgutotal_R_assets_after_covid_Table,
             "bgutotal_R_assets_after_covid_Table.xlsx")
#plot graph

  
  library(igraph)
  #please format as csv in stata 
  linksall <- read.csv("bgu_Rspill_assets_A_covid.csv", header=T, row.names=1)
  linksall <- as.matrix(linksall)
  nodesall <- read.csv("nodesall.csv", header=T, row.names=1)
  netall <- graph_from_adjacency_matrix(linksall, weighted =TRUE)
  netall <- simplify(netall, remove.multiple = F, remove.loops = T) # remove loops
  colrs <- c("tomato", "gold")
  V(netall)$size <- nodesall$size
  V(netall)$color <- colrs[nodesall$type]
  E(netall)$width <- E(netall)$weight/10
  #net$arrow.size <- E(net)$weight/2
  E(netall)$arrow.size <- 0.05
  E(netall)$edge.color <- "gray80"
    E(netall)$width <- E(netall)$weight/10
    plot(netall, vertex.label.cex=0.8,
         remove.loops = T,layout=layout.circle)
    tkid <- tkplot(netall) #for fixing coordinates
    #-----STEP 1-------#
    l <- tkplot.getcoords(tkid)
    tk_close(tkid, window.close = T)
    #-----STEP 2-------#
    
    tiff("fig7b.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
    plot(netall, layout=l)
    legend(x=-1, y=-1, c("Net receiver", "Net transmitter"), 
           pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
    dev.off()
    
  
#*-----------------------before war#####
    
library(ConnectednessApproach)
bgu_R_assets_before_war = ConnectednessApproach(Rassets_before_war, 
                                                     nlag=1, 
                                                     nfore=12,
                                                     model="TVP-VAR",
                                                     connectedness="Extended Joint",
                                                     VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
bgutotal_R_assets_before_war_Table<- as.data.frame(
  bgu_R_assets_before_war$TABLE)

bgutotal_R_assets_before_war_Table <- cbind(
  rownames(bgutotal_R_assets_before_war_Table),
  bgutotal_R_assets_before_war_Table)
library(writexl)
write_xlsx(bgutotal_R_assets_before_war_Table,
           "bgutotal_R_assets_before_war_Table.xlsx")    
    
#plot graph

library(igraph)
#please format as csv in stata 
linksall <- read.csv("bgu_Rspill_assets_B_war.csv", header=T, row.names=1)
linksall <- as.matrix(linksall)
nodesall <- read.csv("nodesall.csv", header=T, row.names=1)
netall <- graph_from_adjacency_matrix(linksall, weighted =TRUE)
netall <- simplify(netall, remove.multiple = F, remove.loops = T) # remove loops
colrs <- c("tomato", "gold")
V(netall)$size <- nodesall$size
V(netall)$color <- colrs[nodesall$type]
E(netall)$width <- E(netall)$weight/10
#net$arrow.size <- E(net)$weight/2
E(netall)$arrow.size <- 0.05
E(netall)$edge.color <- "gray80"
  E(netall)$width <- E(netall)$weight/10
  plot(netall, vertex.label.cex=0.8,
       remove.loops = T,layout=layout.circle)
  tkid <- tkplot(netall) #for fixing coordinates
  #-----STEP 1-------#
  l <- tkplot.getcoords(tkid)
  tk_close(tkid, window.close = T)
  #-----STEP 2-------#
  
  tiff("fig6a.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
  plot(netall, layout=l)
  legend(x=-1, y=-1, c("Net receiver", "Net transmitter"), 
         pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
  dev.off()
  


   
    
#*
#*-----------------------after war######
  library(ConnectednessApproach)
  bgu_R_assets_after_war = ConnectednessApproach(Rassets_after_war, 
                                                  nlag=1, 
                                                  nfore=12,
                                                  model="TVP-VAR",
                                                  connectedness="Extended Joint",
                                                  VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
  bgutotal_R_assets_after_war_Table<- as.data.frame(
    bgu_R_assets_after_war$TABLE)
  
  bgutotal_R_assets_after_war_Table <- cbind(
    rownames(bgutotal_R_assets_after_war_Table),
    bgutotal_R_assets_after_war_Table)
  library(writexl)
  write_xlsx(bgutotal_R_assets_after_war_Table,
             "bgutotal_R_assets_after_war_Table.xlsx")    
  
  #plot graph
  
  library(igraph)
  #please format as csv in stata 
  linksall <- read.csv("bgu_Rspill_assets_A_war.csv", header=T, row.names=1)
  linksall <- as.matrix(linksall)
  nodesall <- read.csv("nodesall.csv", header=T, row.names=1)
  netall <- graph_from_adjacency_matrix(linksall, weighted =TRUE)
  netall <- simplify(netall, remove.multiple = F, remove.loops = T) # remove loops
  colrs <- c("tomato", "gold")
  V(netall)$size <- nodesall$size
  V(netall)$color <- colrs[nodesall$type]
  E(netall)$width <- E(netall)$weight/10
  #net$arrow.size <- E(net)$weight/2
  E(netall)$arrow.size <- 0.05
  E(netall)$edge.color <- "gray80"
    E(netall)$width <- E(netall)$weight/10
    
    plot(netall, vertex.label.cex=0.8,
         remove.loops = T,layout=layout.circle)
    tkid <- tkplot(netall) #for fixing coordinates
    #-----STEP 1-------#
    l <- tkplot.getcoords(tkid)
    tk_close(tkid, window.close = T)
    #-----STEP 2-------#
    
    tiff("fig6b.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
    plot(netall, layout=l)
    legend(x=-1, y=-1, c("Net receiver", "Net transmitter"), 
           pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
    dev.off()
  
#*
#*********************************************************####
#*
#*
# ***********************TVP by variance ********************####
#**********************************************************          
#*              
#             Variance measure                ###
#modelling Garch**####
library(xts)
#repeat all these step to measure volatility for each markets
#--------------------------------------------------------#
alpha <- 0.1
beta <- 0.8 
omega <- var(Rcryp$ave)*(1-alpha-beta)
e <-Rcryp$ave-mean(Rcryp$ave)  
e2 <- e^2  
nobs <- length(Rcryp$ave)
predvar <- rep(NA,nobs)
predvar[1] <- var(Rcryp$ave)
for (t in 2:nobs) {
  predvar[t] <- omega + alpha * e2[t-1] +beta * predvar[t-1]
}
predvol <- sqrt(predvar)
predvol <- xts(predvol, order.by = index(Rcryp$ave))
#-----------------------------------------------------------------#
var_comm <- predvol
colnames(var_comm) <- c("commodity market")
var_eur <- predvol
colnames(var_eur) <- c("European market")
var_asia <- predvol
colnames(var_asia) <- c("Asian market")
var_amer <- predvol
colnames(var_amer) <- c("American market")
var_cryp <- predvol
colnames(var_cryp) <- c("Cryptocurrency market")
volatility <- cbind(var_comm,var_eur,var_asia,var_amer,var_cryp)
volatility <- na.omit(volatility)

library(ConnectednessApproach)
bgu_volatility = ConnectednessApproach(volatility, 
                                     nlag=1, 
                                     nfore=12,
                                     model="TVP-VAR",
                                     connectedness="Extended Joint",
                                     VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))


bgutotal_V_markets<- as.data.frame(bgu_volatility$TCI)
bgutotal_V_markets <- c


bgutotal_v_marketsTable<- as.data.frame(bgu_volatility$TABLE)
bgutotal_v_marketsTable <- cbind(rownames(bgutotal_v_marketsTable),bgutotal_v_marketsTable)
write_xlsx(bgutotal_v_marketsTable,"bgutotal_v_marketsTable.xlsx")

bgutotal_V_markets_from<- as.data.frame(bgu_volatility$FROM)
bgutotal_V_markets_from <- cbind(rownames(bgutotal_V_markets_from),bgutotal_V_markets_from)
write_xlsx(bgutotal_V_markets_from,"bgutotal_V_markets_from.xlsx")

bgutotal_V_markets_to<- as.data.frame(bgu_volatility$TO)
bgutotal_V_markets_to <- cbind(rownames(bgutotal_V_markets_to),bgutotal_V_markets_to)
write_xlsx(bgutotal_V_markets_to,"bgutotal_V_markets_to.xlsx")


bgutotal_V_markets_net<- as.data.frame(bgu_volatility$NET)
bgutotal_V_markets_net <- cbind(rownames(bgutotal_V_markets_net),bgutotal_V_markets_net)
write_xlsx(bgutotal_V_markets_net,"bgutotal_V_markets_net.xlsx")

#Variance measure*for every assets between 2017/2023*####
#                                   

library(tidyverse)
library(rugarch)
library(xts)
R_data <- diff(log(data))
R_data <- na.omit(R_data)  #delete N
colnames(R_data) <- c("Brasil","Indonesia","NaturalGas","Soybean","Japan",
                      "India","Turkey","China","Bitcoin",
                      "france","Italy","UK","oil","Canada",
                      "Germany","Agentina","corn","Aluminium",
                      "Wheat","Gold","Ethereum","Southkorea","Sugar","Mexico",
                      "Tehther","Russia","US")

#**************GARCH measure ********************************###
library(rugarch)

# Create a list of column names to use in the loop
return_columns <- colnames(R_data)
# Create an empty dataframe to store the sigma values for each column

n_rows <- length(index(R_data))
sigma_df <- data.frame(matrix(nrow = n_rows, ncol = 0))

# Loop over each column of returns data and fit a GARCH(1,1) model
for (col in return_columns) {
  # Extract the returns data for the current column
  current_returns <- R_data[, col]
  
  # Create a spec object for the GARCH(1,1) model
  spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                     variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     distribution.model = "norm")
  
  # Fit the GARCH(1,1) model to the returns data
  fit <- ugarchfit(spec, data = current_returns)
  
  # Extract the sigma values for the same dates as the returns data
  sigma_values <- as.numeric(sigma(fit))
  
  # Add the sigma values to the dataframe
  sigma_df[[col]] <- sigma_values
}

sigma_xts <- as.xts(sigma_df, order.by = index(R_data))
#connectedness for every assets***#### 
#delete 14/07/2022 wrong data 
sigma_xts_corr <- subset(sigma_xts, index(sigma_xts) != as.Date("2022-07-20"))
library(ConnectednessApproach)
bgu2021_v = ConnectednessApproach(sigma_xts_corr, 
                                  nlag=1, 
                                  nfore=12,
                                  model="TVP-VAR",
                                  connectedness="Extended Joint",
                                  VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))

library(writexl)
gbu_spill_assets_V <- as.data.frame(bgu2021_v$TABLE)
gbu_spill_assets_V <- cbind(rownames(gbu_spill_assets_V),gbu_spill_assets_V)
write_xlsx(gbu_spill_assets_V,"bgu_spill_assets_V.xlsx") 
#after this delete unnecessary row and column in Excel
#and use stata to exoport to csv format
#*subset dates before and after for 4 markets**####
volatility_before_covid <- subset(volatility,
        index(volatility) <= as.Date("2020-03-11") &
          index(volatility) >= as.Date("2019-11-11"))

volatility_after_covid <- subset(volatility,
                              index(volatility) <= as.Date("2021-07-11") &
                                index(volatility) >= as.Date("2020-03-11"))

volatility_before_war <- subset(volatility,
                             index(volatility) <= as.Date("2021-11-28") &
                               index(volatility) >= as.Date("2021-07-28"))

volatility_after_war <- subset(volatility,
                            index(volatility) <= as.Date("2022-03-28") &
                              index(volatility) >= as.Date("2021-11-28")) 


#connectedness before and after for 4 markets**####
library(ConnectednessApproach)
#before covid
bgu_V_market_before_covid = ConnectednessApproach(volatility_before_covid, 
                                                  nlag=1, 
                                                  nfore=12,
                                                  model="TVP-VAR",
                                                  connectedness="Extended Joint",
                                                  VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))

bgutotal_V_markets_before_covid_Table<- as.data.frame(
  bgu_V_market_before_covid$TABLE)
bgutotal_V_markets_before_covid_Table <- cbind(
  rownames(bgutotal_V_markets_before_covid_Table),
  bgutotal_V_markets_before_covid_Table)
library(writexl)
write_xlsx(bgutotal_V_markets_before_covid_Table,
           "bgutotal_V_markets_before_covid_Table.xlsx")


#after covid
bgu_V_market_after_covid = ConnectednessApproach(volatility_after_covid, 
                                                 nlag=1, 
                                                 nfore=12,
                                                 model="TVP-VAR",
                                                 connectedness="Extended Joint",
                                                 VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
bgutotal_V_markets_after_covid_Table<- as.data.frame(
  bgu_V_market_after_covid$TABLE)
bgutotal_V_markets_after_covid_Table <- cbind(
  rownames(bgutotal_V_markets_after_covid_Table),
  bgutotal_V_markets_after_covid_Table)
library(writexl)
write_xlsx(bgutotal_V_markets_after_covid_Table,
           "bgutotal_V_markets_after_covid_Table.xlsx")

#before war

bgu_V_market_before_war = ConnectednessApproach(volatility_before_war, 
                                                nlag=1, 
                                                nfore=12,
                                                model="TVP-VAR",
                                                connectedness="Extended Joint",
                                                VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))

bgutotal_V_markets_before_war_Table<- as.data.frame(
  bgu_V_market_before_war$TABLE)

bgutotal_V_markets_before_war_Table <- cbind(
  rownames(bgutotal_V_markets_before_war_Table),
  bgutotal_V_markets_before_war_Table)
library(writexl)
write_xlsx(bgutotal_V_markets_before_war_Table,
           "bgutotal_V_markets_before_war_Table.xlsx")

#after war

bgu_V_market_after_war = ConnectednessApproach(volatility_after_war, 
                                               nlag=1, 
                                               nfore=12,
                                               model="TVP-VAR",
                                               connectedness="Extended Joint",
                                               VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
bgutotal_V_markets_after_war_Table<- as.data.frame(
  bgu_V_market_after_war$TABLE)

bgutotal_V_markets_after_war_Table <- cbind(
  rownames(bgutotal_V_markets_after_war_Table),
  bgutotal_V_markets_after_war_Table)
library(writexl)
write_xlsx(bgutotal_V_markets_after_war_Table,
           "bgutotal_V_markets_after_war_Table.xlsx")



#graph for all assets**####
library(igraph)
#please format as csv in stata 
linksall <- read.csv("bgu_spill_assetsV.csv", header=T, row.names=1)
linksall <- as.matrix(linksall)
nodesall <- read.csv("nodesall.csv", header=T, row.names=1)
netall <- graph_from_adjacency_matrix(linksall, weighted =TRUE)
netall <- simplify(netall, remove.multiple = F, remove.loops = T) # remove loops
colrs <- c("tomato", "gold")
V(netall)$size <- nodesall$size
V(netall)$color <- colrs[nodesall$type]
E(netall)$width <- E(netall)$weight/10
#net$arrow.size <- E(net)$weight/2
E(netall)$arrow.size <- 0.05
E(netall)$edge.color <- "gray80"
  E(netall)$width <- E(netall)$weight/10
  plot(netall, vertex.label.cex=0.8,
       remove.loops = T,layout=layout.circle)
  tkid <- tkplot(netall) #for fixing coordinates
  #-----STEP 1-------#
  l <- tkplot.getcoords(tkid)
  tk_close(tkid, window.close = T)
  #-----STEP 2-------#
  
  tiff("fig1V.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
  plot(netall, layout=l)
  legend(x=-1, y=-1, c("Net receiver", "Net transmitter"), 
         pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
  dev.off()
  
  #---------------------------------------#
#merge two bgu total return and volatility
bgutotal <- cbind(bgutotal_V_markets,bgutotal_R_markets)
colnames(bgutotal) <- c("","vol","","R")
bgutotal <- bgutotal[,c(2,4)]
library(writexl)
bgutotal <- cbind(rownames(bgutotal),bgutotal)
write_xlsx(bgutotal,"bgutotal.xlsx")
#------------------------------------------#
















#**connectedness before and after each assets and plot results####
#**subset dates before and after for each assets##########

Vassets_before_covid <- subset(sigma_xts,
                               index(sigma_xts) <= as.Date("2020-03-11") &
                                 index(sigma_xts) >= as.Date("2019-11-11"))
Vassets_after_covid <- subset(sigma_xts,
                              index(sigma_xts) <= as.Date("2021-07-11") &
                                index(sigma_xts) >= as.Date("2020-03-11"))
Vassets_before_war <- subset(sigma_xts,
                             index(sigma_xts) <= as.Date("2021-11-28") &
                               index(sigma_xts) >= as.Date("2021-07-28"))

Vassets_after_war <- subset(sigma_xts,
                            index(sigma_xts) <= as.Date("2022-03-28") &
                              index(sigma_xts) >= as.Date("2021-11-28")) 

#*----------------------before covid#####
library(ConnectednessApproach)
bgu_V_assets_before_covid = ConnectednessApproach(Vassets_before_covid, 
                                                  nlag=1, 
                                                  nfore=12,
                                                  model="TVP-VAR",
                                                  connectedness="Extended Joint",
                                                  VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))

bgutotal_V_assets_before_covid_Table<- as.data.frame(
  bgu_V_assets_before_covid$TABLE)

bgutotal_V_assets_before_covid_Table <- cbind(
  rownames(bgutotal_V_assets_before_covid_Table),
  bgutotal_V_assets_before_covid_Table)
library(writexl)
write_xlsx(bgutotal_V_assets_before_covid_Table,
           "bgutotal_V_assets_before_covid_Table.xlsx")
#plot graph

library(igraph)
#please format as csv in stata 
linksall <- read.csv("bgu_Vspill_assets_b_covid.csv", header=T, row.names=1)
linksall <- as.matrix(linksall)
nodesall <- read.csv("nodesall.csv", header=T, row.names=1)
netall <- graph_from_adjacency_matrix(linksall, weighted =TRUE)
netall <- simplify(netall, remove.multiple = F, remove.loops = T) # remove loops
colrs <- c("tomato", "gold")
V(netall)$size <- nodesall$size/2
V(netall)$color <- colrs[nodesall$type]
E(netall)$width <- E(netall)$weight/10
#net$arrow.size <- E(net)$weight/2
E(netall)$arrow.size <- 0.05
E(netall)$edge.color <- "gray80"
  E(netall)$width <- E(netall)$weight/10
  plot(netall, vertex.label.cex=0.8,
       remove.loops = T,layout=layout.circle)
  tkid <- tkplot(netall) #for fixing coordinates
  #-----STEP 1-------#
  l <- tkplot.getcoords(tkid)
  tk_close(tkid, window.close = T)
  #-----STEP 2-------#
  
  tiff("fig7c.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
  plot(netall, layout=l)
  legend(x=-1, y=-1, c("Net receiver", "Net transmitter"), 
         pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
  dev.off()
  
  #*




#*----------------------after covid#####
  library(ConnectednessApproach)
  bgu_V_assets_after_covid = ConnectednessApproach(Vassets_after_covid, 
                                                   nlag=1, 
                                                   nfore=12,
                                                   model="TVP-VAR",
                                                   connectedness="Extended Joint",
                                                   VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
  
  bgutotal_V_assets_after_covid_Table<- as.data.frame(
    bgu_V_assets_after_covid$TABLE)
  
  bgutotal_V_assets_after_covid_Table <- cbind(
    rownames(bgutotal_V_assets_after_covid_Table),
    bgutotal_V_assets_after_covid_Table)
  library(writexl)
  write_xlsx(bgutotal_V_assets_after_covid_Table,
             "bgutotal_V_assets_after_covid_Table.xlsx")
  #plot graph
  
  
  library(igraph)
  #please format as csv in stata 
  linksall <- read.csv("bgu_Vspill_assets_A_covid.csv", header=T, row.names=1)
  linksall <- as.matrix(linksall)
  nodesall <- read.csv("nodesall.csv", header=T, row.names=1)
  netall <- graph_from_adjacency_matrix(linksall, weighted =TRUE)
  netall <- simplify(netall, remove.multiple = F, remove.loops = T) # remove loops
  colrs <- c("tomato", "gold")
  V(netall)$size <- nodesall$size
  V(netall)$color <- colrs[nodesall$type]
  E(netall)$width <- E(netall)$weight/10
  #net$arrow.size <- E(net)$weight/2
  E(netall)$arrow.size <- 0.05
  E(netall)$edge.color <- "gray80"
    E(netall)$width <- E(netall)$weight/10
    plot(netall, vertex.label.cex=0.8,
         remove.loops = T,layout=layout.circle)
    tkid <- tkplot(netall) #for fixing coordinates
    #-----STEP 1-------#
    l <- tkplot.getcoords(tkid)
    tk_close(tkid, window.close = T)
    #-----STEP 2-------#
    
    tiff("fig7d.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
    plot(netall, layout=l)
    legend(x=-1, y=-1, c("Net receiver", "Net transmitter"), 
           pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
    dev.off()
    
    
#*-----------------------before war#####
    
    library(ConnectednessApproach)
    bgu_V_assets_before_war = ConnectednessApproach(Vassets_before_war, 
                                                    nlag=1, 
                                                    nfore=12,
                                                    model="TVP-VAR",
                                                    connectedness="Extended Joint",
                                                    VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
    bgutotal_V_assets_before_war_Table<- as.data.frame(
      bgu_V_assets_before_war$TABLE)
    
    bgutotal_V_assets_before_war_Table <- cbind(
      rownames(bgutotal_V_assets_before_war_Table),
      bgutotal_V_assets_before_war_Table)
    library(writexl)
    write_xlsx(bgutotal_V_assets_before_war_Table,
               "bgutotal_V_assets_before_war_Table.xlsx")    
    
    #plot graph
    
    library(igraph)
    #please format as csv in stata 
    linksall <- read.csv("bgu_Vspill_assets_B_war.csv", header=T, row.names=1)
    linksall <- as.matrix(linksall)
    nodesall <- read.csv("nodesall.csv", header=T, row.names=1)
    netall <- graph_from_adjacency_matrix(linksall, weighted =TRUE)
    netall <- simplify(netall, remove.multiple = F, remove.loops = T) # remove loops
    colrs <- c("tomato", "gold")
    V(netall)$size <- nodesall$size/2
    V(netall)$color <- colrs[nodesall$type]
    E(netall)$width <- E(netall)$weight/10
    #net$arrow.size <- E(net)$weight/2
    E(netall)$arrow.size <- 0.05
    E(netall)$edge.color <- "gray80"
      E(netall)$width <- E(netall)$weight/10
      plot(netall, vertex.label.cex=0.8,
           remove.loops = T,layout=layout.circle)
      tkid <- tkplot(netall) #for fixing coordinates
      #-----STEP 1-------#
      l <- tkplot.getcoords(tkid)
      tk_close(tkid, window.close = T)
      #-----STEP 2-------#
      
      tiff("fig6c.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
      plot(netall, layout=l)
      legend(x=-1, y=-1, c("Net receiver", "Net transmitter"), 
             pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
      dev.off()
      
      
      
      
      
      #*
  

    
    
#*-----------------------after war######
library(ConnectednessApproach)
bgu_V_assets_after_war = ConnectednessApproach(Vassets_after_war, 
                                                     nlag=1, 
                                                     nfore=12,
                                                     model="TVP-VAR",
                                                     connectedness="Extended Joint",
                                                     VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
bgutotal_V_assets_after_war_Table<- as.data.frame(
  bgu_V_assets_after_war$TABLE)
      
bgutotal_V_assets_after_war_Table <- cbind(
        rownames(bgutotal_V_assets_after_war_Table),
        bgutotal_V_assets_after_war_Table)
library(writexl)
write_xlsx(bgutotal_V_assets_after_war_Table,
                 "bgutotal_V_assets_after_war_Table.xlsx")    
      
#plot graph
      
library(igraph)
#please format as csv in stata 
linksall <- read.csv("bgu_Vspill_assets_A_war.csv", header=T, row.names=1)
linksall <- as.matrix(linksall)
nodesall <- read.csv("nodesall.csv", header=T, row.names=1)
netall <- graph_from_adjacency_matrix(linksall, weighted =TRUE)
netall <- simplify(netall, remove.multiple = F, remove.loops = T) # remove loops
colrs <- c("tomato", "gold")
V(netall)$size <- nodesall$size/2
V(netall)$color <- colrs[nodesall$type]
E(netall)$width <- E(netall)$weight/10
#net$arrow.size <- E(net)$weight/2
E(netall)$arrow.size <- 0.05
E(netall)$edge.color <- "gray80"
E(netall)$width <- E(netall)$weight/10

plot(netall, vertex.label.cex=0.8,
remove.loops = T,layout=layout.circle)
tkid <- tkplot(netall) #for fixing coordinates
#-----STEP 1-------#
l <- tkplot.getcoords(tkid)
tk_close(tkid, window.close = T)
#-----STEP 2-------#
  
tiff("fig6d.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
plot(netall, layout=l)
legend(x=-1, y=-1, c("Net receiver", "Net transmitter"), 
               pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
dev.off()
        
#*

      
      

#* robustness test ############
#*----------------- DCC GARCH ####
european <- var_eur
asian <- Rmarkets$Asianmarkets
american <- na.omit(Rmarkets$Americanmarkets)
crypto <- Rmarkets$cryptomarkets
comm <- Rmarkets$Commoditymarkets
wheat <- Rcomm$Wheat
NG <- Rcomm$NaturalGas[-c(4324:4327),]

library(tseries)
library(rugarch)
library(rmgarch)
library(xts)
library(zoo)


model1 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                     variance.model = list(garchOrder=c(1,1),
                                           model="sGARCH"),
                     distribution.model = "norm")

modelspec<-dccspec(uspec=multispec(replicate(2,model1)),
                   dccOrder=c(1,1),distribution="mvnorm")
#before covid
sdata <- cbind(american["2019-11-11/2020-03-11"],
               european["2019-11-11/2020-03-11"])
#after covid19
sdata <- cbind(american["2020-03-11/2021-07-11"],
               european["2020-03-11/2021-07-11"])
#------------------------------------------------------------#
#before war 
sdata <- cbind(european["2021-07-28/2021-11-28"],
                 asian["2021-07-28/2021-11-28"])
#after war
sdata <- cbind(european["2021-11-28/2023-03-03"],
               asian["2021-11-28/2023-03-03"])

#fit model
modelfit <- dccfit(modelspec,data=sdata)

plot(modelfit)

ts.plot(rcor(modelfit)[1,2,])
plot(modelfit, which=4)

#* ----------------frequency connectedness####
library(frequencyConnectedness)
#*-------------for volatility####
logvolatility <- na.omit(diff(log(volatility["2015-01-01/2023-03-03"])))
colnames(logvolatility) <- c("Com","Eur",
                        "Asia","Amer","cryp")

#overallspillover
sp_dy <- spilloverRollingDY12(logvolatility["2015-01-01/2023-03-03"],
                              n.ahead = 100, no.corr = F, func_est = "VAR", 
                              params_est = list(), window = 100)
plotOverall(sp_dy)
#frequency spillover
bounds <- c(pi+0.00001,pi/7, pi/30,pi/300)
params_est = list(p = 2, type = "const")

sp <- spilloverRollingBK12(logvolatility["2015-01-01/2023-03-03"],
  n.ahead = 100, no.corr = F, func_est = "VAR", 
  params_est = list(), window = 100, partition = bounds)
plotOverall(sp)
plotPairwise(sp)
#*------------for return####
Rmarkets_freq <- Rmarkets
colnames(Rmarkets_freq) <- c("Com","Eur",
                             "Asia","Amer","cryp")

#overallspillover
sp_dy_R <- spilloverRollingDY12(Rmarkets_freq["2015-01-01/2023-03-03"],
                              n.ahead = 100, no.corr = F, func_est = "VAR", 
                              params_est = list(), window = 100)
plotOverall(sp_dy_R)
#frequency spillover
bounds <- c(pi+0.00001,pi/7, pi/30,pi/300)
params_est = list(p = 2, type = "const")

sp_R <- spilloverRollingBK12(Rmarkets_freq["2015-01-01/2023-03-03"],
                           n.ahead = 100, no.corr = F, func_est = "VAR", 
                           params_est = list(), window = 100, partition = bounds)
plotOverall(sp_R)
plotPairwise(sp_R)


#####################################################*
#####################################################*
#####################################################*
#########################END############################################










