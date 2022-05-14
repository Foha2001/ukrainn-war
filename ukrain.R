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
              "^MXX","XU100.IS","M.BA","^JKII","BTC-USD","ETH-USD",
              "USDT-USD","CL=F","ALI=F","GC=F","SB=F","KE=F", "ZS=F",
              "ZC=F","NG=F")



start_date <- Sys.Date()-2689
end_date <- Sys.Date()
envt1 <- new.env()
getSymbols(w_indice,env=envt1,from=start_date, to=end_date)
data <- do.call(merge, eapply(envt1, Cl))
data <- data[,-c(21,25)] #delete Ethereum anf Thether unavailable crpto 
dataframe<- as.data.frame(na.omit (data))  #data before 2007 unavailable for Ethereum 

#----------------descriptive statistics------------------####
#*

datana <- na.omit(data)  # as zoo
R_data <- diff(log(datana)) # as zoo
R_data <- R_data[-1,]
R_dataframe <- as.data.frame(R_data)
R_dataframe <- na.omit(R_dataframe)
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
rownames(desc) <- c("Brasil","Indonesia","NG","Soybean","Japan",
                      "India","Turkey","China","Bitcoin",
                      "france","Italy","UK","oil","Canada",
                      "Germany","Agentina","corn","Aluminium",
                      "Wheat","Gold","Southkorea","Sugar","Mexico",
                      "Russia","US")


desc <- cbind(rownames(desc),desc)
library(writexl)
write_xlsx(desc,"descstatistics.xlsx") 
return <- cbind(rownames(R_dataframe),R_dataframe)
colnames(return) <- c("date","Brasil","Indonesia","NG","Soybean","Japan",
                       "India","Turkey","China","Bitcoin",
                       "france","Italy","UK","oil","Canada",
                       "Germany","Agentina","corn","Aluminium",
                       "Wheat","Gold","Southkorea","Sugar","Mexico",
                      "Russia","US")



write_xlsx(return,"return.xlsx")

#********normality test************
normalTest(R_dataframe$FTSEMIB.MI.Close,method="jb")

##*******************************************
# ****     star and segment plot ****  ####
#*******************************************
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



########################################################
#****           Time Varying Parameter *** ###  
########################################################







