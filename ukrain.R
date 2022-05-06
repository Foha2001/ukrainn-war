#**********************************************
# *****       u*k*r*a*i*n*e war    *****    ####
#**********************************************

#************************************************
# ****        import data       ****    ####
#*************************************************

library(quantmod)
library(xts)
library(zoo)
w_indice <- c("^GSPC","000001.SS","^N225","^GDAXI","^NSEI","^FTSE",
              "^FCHI","FTSEMIB.MI","^BVSP","^GSPTSE","^KS11","IMOEX.ME",
              "^MXX","XU100.IS","M.BA","^JKII","^VIX","BTC-USD","LTC-USD",
              "USDT-USD","ETH-USD")

start_date <- Sys.Date()-310
end_date <- Sys.Date()
envt1 <- new.env()
getSymbols(w_indice,env=envt1,from=start_date, to=end_date)
data <- do.call(merge, eapply(envt1, Cl))
dataframe<- as.data.frame(na.omit (data))

#----------------------plot vix--------------------####

getSymbols("^VIX") # download vix 
plot(diff(log(VIX$VIX.Adjusted)), subset="2021-03-01/2022-03-31",
     main="VIX returns between March 2021 and April 2022", cex=0.4,
     yaxis.right = FALSE)
#----------------descriptive statistics------------------####
#*

datana <- na.omit(data)  # as zoo
R_data <- diff(log(datana)) # as zoo
R_data <- R_data[-1,]
R_dataframe <- as.data.frame(R_data)
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
#********normality test************
normalTest(R_dataframe$FTSEMIB.MI.Close,method="jb")

##*******************************************
# ****     star and segment plot ****  ####
#*******************************************
library(fBasics)
library(xts)
R_data1 <- R_data[endpoints(R_data,'month')]
colnames(R_data1) <- c("India","Tether","Ethereum","litecoin","Russia","S&P500",
                       "Argentina","Mexico","China","Vix","Germany",
                       "South korea","Turkey","france","UK","Japan",
                       "Brasil","Indonesia","Canada","italy","Bitcoin")
lab <- list("2021/06","2021/07","2021/08","2021/09",
            "2021/10","2021/11","2021/12","2022/01",
            "2022/02","2022/03","2022/04")
stars(R_data1, draw.segments = TRUE, labels = lab, ncol = 5,
     cex=0.5,key.loc = c(7, 0.5), mar = c(4, 0, 0, 0))



#*******************************
# **** wavelet analysis **** #####
#*******************************
library(biwavelet)
library(zoo)
library(xts)
colnames(R_dataframe) <- c("India","Tether","Ethereum","litecoin","Russia","S&P500",
                       "Argentina","Mexico","China","Vix","Germany",
                       "South korea","Turkey","france","UK","Japan",
                       "Brasil","Indonesia","Canada","italy","Bitcoin")
t1 <- cbind(1:132,R_dataframe$litecoin)
t2 <- cbind(1:132,R_dataframe$Canada)
t1[is.na(t1)] <- 0
t2[is.na(t2)] <- 0

nrands=100
sum(is.na(t1))
sum(is.na(t2))

wtcr <- wtc(t1,t2,nrands=nrands)
par(mar=c(5,4,5,5),+0.1)
#par(oma=c(1,1,1,0),mar=c(0,4,0.5,5),+0.1)

plot(wtcr,plot.phase=TRUE,xaxt='n',lty.coi=1,col.coi="grey",lwd.coi=2, 
     lwd.sig=2, arrow.lwd=0.03, arrow.len=0.08, ylab="Frequency",xlab="Years-Month",
     plot.cb= T, main="WTC : Litecoin-TSE index", cex.main=0.8)


n <- c("2021-06","2021-7","2021-08","2021-09","2021-10","2021-11",
          "2021-12","2022-01","2022-02","2022-03","2022-04")

axis(1, at = c(seq(0,132,13)), n )

#-----------------export to excel-----------------------####
library(writexl)  
nw <- data.frame(date=index(R_data), coredata(R_data)) # add index and convert to dataframe
colnames(nw) <- c("date","India","Tether","Ethereum","litecoin","Russia","S&P500",
                           "Argentina","Mexico","China","Vix","Germany",
                           "South korea","Turkey","france","UK","Japan",
                           "Brasil","Indonesia","Canada","italy","Bitcoin")

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




#*******************************************************************

###                  THIS PART IS NOT YET FINISHED                        #
#********************************************************
#*****       Volatility Spillovers and Connectedness *****####
#*************************************************************
library(xts)
library(PerformanceAnalytics)
library(quantmod)
datav <- do.call(merge, eapply(envt1, cbind))
datav <- na.omit(datav)
v_datav <- 0.361*(log(Hi(datav))-log(Lo(datav)))^2
volt <- 1/sinh(sqrt(252*v_datav))
volt[volt == Inf] <- lag(volt)
volt <- na.omit(volt)
chart.TimeSeries(volt,lwd=2,auto.grid=F,
                 ylab="Annualized Log Volatility",xlab="Time",
                 main="Log Volatility",lty=1,
                 legend.loc="topright") 
# ------Step 1. Vector Autoregression (VAR)---------####

library(vars)
vol_var = VAR(volt,p=3,type="const")
amat <- diag(ncol(volt))
amat[lower.tri(amat)] <- NA
vol_svar = SVAR(vol_var,Amat = amat,estmethod = "direct")
### extract residuals of the VAR
res_t <- residuals(vol_var)
svar_ecov <- vol_svar$Sigma.U

#---------Step 2. Moving Average Representation-------####

MA_lag <- 10
theta_temp <- Phi(vol_var,nstep = MA_lag)
svar_theta_temp <- Phi(vol_svar,nstep = MA_lag)
### extract MA coefficients
library(plyr)
theta.list <- alply(theta_temp,3)
svar_theta_list <- alply(svar_theta_temp,3)
#--------Step 3. Impulse Response Function (IRF)-------####
spil <- fevd(vol_var, 10)

#########################spillover Table##################################

library(vars)
vol_var = VAR(volt,p=3,type="const")
library(frequencyConnectedness)

spill <- spilloverDY12(vol_var, n.ahead = 100, no.corr=F)

table <-as.data.frame(spill$tables)
to(spill)



library(BigVAR)
data(volatilities)
big_var_est <- function(data) {
  Model1 = constructModel(as.matrix(data), p = 4, struct = "Basic", gran = c(50, 50), VARX = list(), verbose = F)
  Model1Results = cv.BigVAR(Model1)
}

# Perform the estimation
oo <- big_var_est(log(volatilities[apply(volatilities>0, 1, all),]))

spilloverDY12(oo, n.ahead = 100, no.corr = F)


