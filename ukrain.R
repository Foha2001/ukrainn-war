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


