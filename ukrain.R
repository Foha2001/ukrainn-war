#**********************************************
# *                                                 #
#     Volatility spillovers among global assets: 
#  Comparing health crisis with geopolitical crisis
#**********************************************

#************************************************
#              import data                     ####
#*************************************************

library(quantmod)
library(xts)
library(zoo)
w_indice <- c("^GSPC","000001.SS","^N225","^GDAXI","^NSEI","^FTSE",
              "^FCHI","FTSEMIB.MI","^BVSP","^GSPTSE","^KS11","IMOEX.ME",
              "^MXX","XU100.IS","M.BA","^JKII","BTC-USD","ETH-USD",
              "USDT-USD","CL=F","ALI=F","GC=F","SB=F","KE=F", "ZS=F",
              "ZC=F","NG=F")



start_date <- Sys.Date()-2689
end_date <- Sys.Date()
envt1 <- new.env()
getSymbols(w_indice,env=envt1,from=start_date, to=end_date)
data <- do.call(merge, eapply(envt1, Cl))
#data <- data[,-c(21,25)] #delete Ethereum anf Thether unavailable crpto 
dataframe<- as.data.frame(na.omit (data))  #data before 2007 unavailable for Ethereum 

#----------------descriptive statistics------------------####
#*

datana <- na.omit(data)  # as zoo
R_data <- diff(log(datana)) # as zoo
R_data <- R_data[-1,]
R_dataframe <- as.data.frame(R_data)
R_dataframe <- na.omit(R_dataframe)
colnames(R_dataframe) <- c("Brasil","Indonesia","NaturalGas","Soybean","Japan",
                      "India","Turkey","China","Bitcoin",
                      "france","Italy","UK","oil","Canada",
                      "Germany","Agentina","corn","Aluminium",
                      "Wheat","Gold","Ethereum","Southkorea","Sugar","Mexico",
                      "Tehther","Russia","US")

library(fBasics)
desc <- do.call(data.frame, 
               list(mean = apply(R_dataframe, 2, mean),
                    sd = apply(R_dataframe, 2, sd),
                    median = apply(R_dataframe, 2, median),
                    min = apply(R_dataframe, 2, min),
                    max = apply(R_dataframe, 2, max),
                    skew = apply(R_dataframe, 2, sampleSKEW),
                    kurt = apply(R_dataframe,2,sampleKURT)))
desc


desc <- cbind(rownames(desc),desc)
library(writexl)
write_xlsx(desc,"descstatistics.xlsx") 
return <- cbind(rownames(R_dataframe),R_dataframe)
colnames(return)[1] <- "Date"
write_xlsx(return,"return.xlsx")


#********normality test************##
normalTest(R_dataframe$US,method="jb")





########################################################
#****           Time Varying Parameter *** #####  
#########                                               ##
##               TVP by retutn               ####
var_d <- read.zoo(return)
library(ConnectednessApproach)

# #dy2012 model 
# dy2012 = ConnectednessApproach(var_d, 
#                              nlag=4, 
#                              nfore=100,
#                              model="VAR",
#                              connectedness="Time",
#                              Connectedness_config=list(TimeConnectedness=list(generalized=TRUE)))
# spill2012 <- as.data.frame(dy2012$TABLE)
# spill2012 <- cbind(rownames(spill2012),spill2012)
# library(writexl)
# write_xlsx(spill2012,"spill2012.xlsx")
#acg2020 model
# acg2020 = ConnectednessApproach(var_d, 
#                             nlag=1, 
#                             nfore=12,
#                             window.size=100,
#                             model="TVP-VAR",
#                             connectedness="Time",
#                             VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))
# 
# spillacg2020 <- as.data.frame(acg2020$TABLE)
# spillacg2020 <- cbind(rownames(spillacg2020),spillacg2020)
# write_xlsx(spillacg2020,"spillacg2020.xlsx")
# 
# 
# 
# PlotTCI(acg2020, ca=DCA, ylim=c(20,100))  #the average
# PlotFROM(acg2020, ylim=c(0,130))
# PlotTO(acg2020, ylim=c(0,200))
# PlotNET(acg2020, ylim=c(-100,100))
# #PlotNPDC(acg2020, ylim=c(-10,20))
# PlotPCI(acg2020)
#*********************************************************
#*                    TVP by variance           ####
#             Variance measure             ###
lowp <- do.call(merge, eapply(envt1, Lo))
hip <- do.call(merge, eapply(envt1, Hi))
vari <- 0.361*(log(hip)-log(lowp))^2
vari <- na.omit(vari)
vari <- as.zoo(vari)
colnames(vari) <- c("Brasil","Indonesia","NaturalGas","Soybean","Japan",
                             "India","Turkey","China","Bitcoin",
                             "france","Italy","UK","oil","Canada",
                             "Germany","Agentina","corn","Aluminium",
                             "Wheat","Gold","Ethereum","Southkorea","Sugar","Mexico",
                             "Tehther","Russia","US")




# connectedness for all ############
acg2020v = ConnectednessApproach(vari, 
                                nlag=1, 
                                nfore=12,
                                window.size=100,
                                model="TVP-VAR",
                                connectedness="Time",
                                VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))
spillacg2020v <- as.data.frame(acg2020v$TABLE)
spillacg2020v <- cbind(rownames(spillacg2020v),spillacg2020v)
write_xlsx(spillacg2020v,"spillacg2020v.xlsx")
# #Net Total and Net Pairwise Directional Connectedness Measures
# PlotNET(acg2020v,ca=acg2020v, ylim=c(-100,250))
# # Dynamic Total Connectedness
# PlotTCI(acg2020v,ca=acg2020v, ylim=c(10,100))
# # Directional Volatility Spillovers, FROM 
# PlotFROM(acg2020v, ylim=c(0,130))
# # Directional Volatility Spillovers, TO 
# PlotTO(acg2020v, ylim=c(0,300))


#------------vari for commodity-------------- 
varicomm <- vari[,c(13,18,20,23,19,4,17,3)]

acg2020comm = ConnectednessApproach(varicomm, 
                                 nlag=1, 
                                 nfore=12,
                                 window.size=100,
                                 model="TVP-VAR",
                                 connectedness="Time",
                                 VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))

spillacg2020comm <- as.data.frame(acg2020comm$TABLE)
spillacg2020comm <- cbind(rownames(spillacg2020comm),spillacg2020comm)
write_xlsx(spillacg2020comm,"spillacg2020comm.xlsx")
#-------------variequity--------------------
variequity <- vari[,c(27,8,5,15,6,12,10,11,1,14,22,26,24,7,16,2)]
acg2020equity = ConnectednessApproach(variequity, 
                                    nlag=1, 
                                    nfore=12,
                                    window.size=100,
                                    model="TVP-VAR",
                                    connectedness="Time",
                                    VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))
spillacg2020equity <- as.data.frame(acg2020equity$TABLE)
spillacg2020equity <- cbind(rownames(spillacg2020equity),spillacg2020equity)
write_xlsx(spillacg2020equity,"spillacg2020equity.xlsx")
#------------for cryptocurrency--------------------

varicryp <- vari[,c(9,21,25)]
acg2020cryp = ConnectednessApproach(varicryp, 
                                      nlag=1, 
                                      nfore=12,
                                      window.size=100,
                                      model="TVP-VAR",
                                      connectedness="Time",
                                      VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))
spillacg2020cryp <- as.data.frame(acg2020cryp$TABLE)
spillacg2020cryp <- cbind(rownames(spillacg2020cryp),spillacg2020cryp)
write_xlsx(spillacg2020cryp,"spillacg2020cryp.xlsx")


#*********** apply connectedness between regions *************** ####
#1
comm <- varicomm
comm$commodity <- apply(comm, 1, mean)

rcomm <- var_d[,c(13,18,20,23,19,4,17,3)] #for DCC garch analyis
rcomm$commodity <- apply(rcomm,1,mean)
#2
cryp <- varicryp
cryp$crypto <- apply(cryp, 1, mean)

rcryp <- var_d[,c(9,21,25)]
rcryp$crypto <- apply(rcryp,1,mean)
#3
equiEUR <- variequity[,c(4,6,7,8,12,14)]
equiEUR$EUR <- apply(equiEUR,1,mean)

requi <- var_d[,c(27,8,5,15,6,12,10,11,1,14,22,26,24,7,16,2)]
requiEUR <- requi[,c(4,6,7,8,12,14)]
requiEUR$EUR <- apply(requiEUR,1,mean)
#4
equiASI <- variequity[,c(2,3,5,11,16)]
equiASI$asia <- apply(equiASI,1,mean)

requiASI <- requi[,c(2,3,5,11,16)]
requiASI$asia <- apply(requiASI,1,mean)
#5
equiAME <- variequity[,c(1,9,10,13,15)]
equiAME$america <- apply(equiAME,1,mean)

requiAME <- requi[,c(1,9,10,13,15)]
requiAME$america <- apply(requiAME,1,mean)

markets <-cbind(comm$commodity,equiEUR$EUR,equiASI$asia,equiAME$america,
                cryp$crypto) 
colnames(markets) <- c("Commoditymarkets","Europemarkets",
                       "Asianmarkets","Americanmarkets","cryptomarkets")
Rmarkets <- cbind(rcomm$commodity,requiEUR$EUR,requiASI$asia,requiAME$america,
                 rcryp$crypto) 
colnames(Rmarkets) <- c("Commoditymarkets","Europemarkets",
                       "Asianmarkets","Americanmarkets","cryptomarkets")


#date before 28novembre2021
marketbefore <- markets[607:659,]
marketafter <- markets[660:707,]
varivafterwar <- vari[660:707,]
marketbeforecovid <- markets[323:380,]
marketaftercovid <- markets[380:600,]
varibeforewar <- vari[607:659,]
varibeforecovid <- vari[323:380,]


acg2020markets = ConnectednessApproach(markets, 
                                   nlag=1, 
                                   nfore=12,
                                   window.size=100,
                                   model="TVP-VAR",
                                   connectedness="Time",
                                   VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))
spillacg2020markets<- as.data.frame(acg2020markets$TABLE)
spillacg2020markets <- cbind(rownames(spillacg2020markets),spillacg2020markets)
write_xlsx(spillacg2020markets,"spillacg2020markets.xlsx")

# #Net Total and Net Pairwise Directional Connectedness Measures
# PlotNET(acg2020markets,ylim=c(-20,50))
# # Dynamic Total Connectedness
# PlotTCI(acg2020markets, ylim=c(10,80))
# # Directional Volatility Spillovers, FROM 
# PlotFROM(acg2020markets, ylim=c(0,130))
# # Directional Volatility Spillovers, TO 
# PlotTO(acg2020markets, ylim=c(0,80))

#extract total connectedness (TCI)#####
spillacg2020total<- as.data.frame(acg2020markets$TCI)
spillacg2020total <- cbind(rownames(spillacg2020total),spillacg2020total)
write_xlsx(spillacg2020total,"spillacg2020total.xlsx")
#extract from 
spillacg2020from<- as.data.frame(acg2020markets$FROM)
spillacg2020from <- cbind(rownames(spillacg2020from),spillacg2020from)
write_xlsx(spillacg2020from,"spillacg2020from.xlsx")
#extract NET
spillacg2020net<- as.data.frame(acg2020markets$NET)
spillacg2020net <- cbind(rownames(spillacg2020net),spillacg2020net)
write_xlsx(spillacg2020net,"spillacg2020net.xlsx")
#extract to
spillacg2020to<- as.data.frame(acg2020markets$TO)
spillacg2020to <- cbind(rownames(spillacg2020to),spillacg2020to)
write_xlsx(spillacg2020to,"spillacg2020to.xlsx")




###########################################*
#              plot network             ####
###########################################*

library(igraph)
####  graph for markets##############
#please format as csv in stata
links <- read.csv("spillacg2020markets.csv", header=T, row.names=1)
links <- as.matrix(links)
nodes <- read.csv("nodesmarkets.csv", header=T, row.names=1)
net <- graph_from_adjacency_matrix(links, weighted =TRUE)
net <- simplify(net, remove.multiple = F, remove.loops = T) # remove loops
colrs <- c("tomato", "gold")
V(net)$color <- colrs[nodes$type]
E(net)$width <- E(net)$weight
#net$arrow.size <- E(net)$weight/2
E(net)$arrow.size <- 0.5
E(net)$edge.color <- "gray80"
E(net)$width <- E(net)$weight
plot(net, vertex.label.cex=0.8, vertex.size=55,
     remove.loops = T,layout=layout_in_circle)

legend(x=-1.5, y=-1, c("Net receiver", "Net transmitter"),
       pch=21, col="#777777", pt.bg=colrs, pt.cex=5, cex=1.1, bty="n", ncol=1)

# graph for all assets ###########
library(igraph)
#please format as csv in stata 
linksall <- read.csv("spillacg2020v.csv", header=T, row.names=1)
linksall <- as.matrix(linksall)
nodesall <- read.csv("nodesall.csv", header=T, row.names=1)
netall <- graph_from_adjacency_matrix(linksall, weighted =TRUE)
netall <- simplify(netall, remove.multiple = F, remove.loops = T) # remove loops
colrs <- c("tomato", "gold")
V(netall)$size <- nodesall$size
V(netall)$color <- colrs[nodesall$type]
E(netall)$width <- E(netall)$weight/10
net$arrow.size <- E(net)$weight/2
E(netall)$arrow.size <- 0.05
E(netall)$edge.color <- "gray80"
E(netall)$width <- E(netall)$weight/10
plot(netall, vertex.label.cex=0.8,
     remove.loops = T,layout=layout.circle)
tkid <- tkplot(netall) #for fixing coordinates
l <- tkplot.getcoords(tkid)
tk_close(tkid, window.close = T)
tiff("fig11.jpg",width = 10, height = 10, units = 'in', res = 350) #for high resolution
plot(netall, layout=l)
legend(x=-1.5, y=-1, c("Net receiver", "Net transmitter"), 
       pch=21, col="#777777", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
dev.off()

#                  DCC garch                 ####
###################################################***

dca = ConnectednessApproach(vari,
                            nfore=10,
                            corrected=TRUE,
                            model="DCC-GARCH")

PlotNetwork(dca = dca)

dcc<- as.data.frame(dca$TABLE)
dccresult <- cbind(rownames(dcc),dcc)
write_xlsx(dccresult,"dccall.xlsx")

PlotTCI(dca, ylim=c(0,100))
PlotNET(dca, ylim=c(-100,100))
PlotTO(dca, ylim=c(0,120))
PlotFROM(dca, ylim=c(0,120))

library(tseries)
library(rugarch)
library(rmgarch)
library(xts)
oil <- var_d$oil
gold <- var_d$Gold
crypto<- Rmarkets$cryptomarkets
comm <- Rmarkets$Commoditymarkets
american <- Rmarkets$Americanmarkets
asian <- Rmarkets$Asianmarkets
european <- Rmarkets$Europemarkets
model1 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                     variance.model = list(garchOrder=c(1,1),
                                           model="sGARCH"),
                     distribution.model = "norm")

modelspec<-dccspec(uspec=multispec(replicate(2,model1)),
                     dccOrder=c(1,1),distribution="mvnorm")

modelfit <- dccfit(modelspec,data=cbind(oil,gold))
plot(modelfit)
correlation <- rcor(modelfit)
cor_oil_cryp <- correlation[2,1,]




plot.ts(cor_oil_gold)

 ##################################################****
#                Article 2                   ####
##################################################*
# ****     star and segment plot ****  ####

library(fBasics)
library(xts)
R_data1 <- R_data[endpoints(R_data,'month')]
colnames(R_data1) <- c("Brasil","Indonesia","Naturalgaz","Japan","India","Turkey",
                       "China","Bitcoin","France","Silver",
                       "Italy","UK","Crudeoil","Canada","Germany",
                       "Argentina","Aluminium","Gold","Southkorea",
                       "Suger","Mexico","Litecoin","Russia","Sp500")
lab <- list("2021/06","2021/07","2021/08","2021/09",
            "2021/10","2021/11","2021/12","2022/01",
            "2022/02","2022/03","2022/04")
stars(R_data1, draw.segments = TRUE, labels = lab, ncol = 10,
      cex=0.5,key.loc = c(7, 0.5), mar = c(4, 0, 0, 0))
#-----------------export to excel-----------------------####
library(writexl)  
nw <- data.frame(date=index(R_data), coredata(R_data)) # add index and convert to dataframe
colnames(nw) <- c("date","Brasil","Indonesia","Naturalgaz","Japan","India","Turkey",
                  "China","Bitcoin","France","Silver",
                  "Italy","UK","Crudeoil","Canada","Germany",
                  "Argentina","Aluminium","Gold","Southkorea",
                  "Suger","Mexico","Litecoin","Russia","Sp500")

write_xlsx(nw,"NT.xlsx") 












#********************************************************
# ****   Granger causality Network         ****   ####
#*********************************************************
library(igraph)
library(readxl)
edge <- read_excel("edge ukrain.xlsx")
noeuds <- read_excel("vertice.xlsx")
edge <- read_excel("edge sincenov21.xlsx") # after Nov-21
Neural <- graph_from_data_frame(d=edge, vertices=noeuds, directed=T)


#E(Neural)$weight <- E(Neural)$pro
#Neural[c(1:3),c(1:3)]
#------------------degree centrality--------------------####
Neural_deg <- degree(Neural,mode=c("all"))
V(Neural)$degree <- Neural_deg
V(Neural)$degree
which.max(Neural_deg)
#----------------eigenvector centrality------------------####
Neural_eig <- evcent(Neural)$vector  
V(Neural)$eigen <- Neural_eig
which.max(Neural_eig)

#------------Betweenness centrality-----------------####
Neural_bet <- betweenness(Neural,directed=F)
V(Neural)$betweeness <- Neural_bet
which.max(Neural_bet)

#---------------plot graph--------------------####
DF <- as_long_data_frame(Neural)
set.seed(1010)
colrss <- c("grey", "tomato","orange","green","black")
V(Neural)$color <- colrss[V(Neural)$type]
edge.start <- ends(Neural, es=E(Neural), names=F)[,1]
edge.col <- V(Neural)$color[edge.start]
plot(Neural,layout=layout_on_sphere, vertex.shape="circle",
     vertex.size=Neural_bet,vertex.label.cex=0.7,edge.width=2,
     edge.arrow.size=0.5) 
legend(x=-1.5, y=-1.1, c("CRYPTO","EUROPE","ASIA","AMERICA","VIX"),
       pch=21, col="#777777", pt.bg=colrss, pt.cex=2, cex=.8, bty="n",
       ncol=1)
#-----------Other plots------------------####
set.seed(1001)
ceb <- cluster_edge_betweenness(Neural)
plot(ceb, Neural,edge.arrow.size=0.5,vertex.size=Neural_deg*2.5)
clp <- cluster_label_prop(Neural)
plot(ceb, Neural,edge.arrow.size=0.5,vertex.size=Neural_deg*2.5)











