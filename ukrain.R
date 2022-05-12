#**********************************************
# *****       ukrain war    *****    ####
#**********************************************

#************************************************
# ****        import data       ****    ####
#*************************************************

library(quantmod)
library(xts)
library(zoo)
w_indice <- c("^GSPC","000001.SS","^N225","^GDAXI","^NSEI","^FTSE",
              "^FCHI","FTSEMIB.MI","^BVSP","^GSPTSE","^KS11","IMOEX.ME",
              "^MXX","XU100.IS","M.BA","^JKII","BTC-USD","LTC-USD",
              "USDT-USD","ETH-USD","CL=F","NG=F","GC=F","ALI=F",
              "SB=F","SI=F")



start_date <- Sys.Date()-2689
end_date <- Sys.Date()
envt1 <- new.env()
getSymbols(w_indice,env=envt1,from=start_date, to=end_date)
data <- do.call(merge, eapply(envt1, Cl))
data <- data[,-c(19,24)] #delete 2 unavailable crpto 
dataframe<- as.data.frame(na.omit (data))  #data before 2007 unavailable for some cripto

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


########################################################
#****           Time Varying Parameter *** ###  
########################################################

#' @title Time-varying parameter vector autoregression
#' @description Estimate TVP-VAR model
#' @param x zoo data matrix
#' @param nlag Lag length
#' @param prior List of prior VAR coefficients and variance-covariance matrix
#' @param l forgetting factors (kappa1, kappa2)
#' @param configuration model configuration
#' @return Estimate TVP-VAR model
#' @examples
#' \donttest{
#' data(dy2012)
#' prior = BayesPrior(dy2012, nlag=1)
#' fit = TVPVAR(dy2012, configuration=list(nlag=1, prior=prior, l=c(0.99,0.99)))
#' }
#' @importFrom MASS ginv
#' @importFrom stats cov
#' @importFrom progress progress_bar
#' @references
#' Koop, G., & Korobilis, D. (2014). A new index of financial conditions. European Economic Review, 71, 101-116.\\
#' Antonakakis, N., Chatziantoniou, I., & Gabauer, D. (2020). Refined measures of dynamic connectedness based on time-varying parameter vector autoregressions. Journal of Risk and Financial Management, 13(4), 84.
#' @author David Gabauer
#' @export
TVPVAR = function(x, configuration=list(l=c(0.99,0.99), nlag=1, prior=NULL)){
  l = as.numeric(configuration$l)
  nlag = configuration$nlag
  prior = configuration$prior
  if (nlag<=0) {
    stop("nlag needs to be a positive integer")
  }
  if (class(x)!="zoo") {
    stop("Data needs to be of type 'zoo'")
  }
  if (l[1] <= 0 || l[1] >= 1) {
    stop("kappa1 needs to be within 0 and 1")
  }
  if (l[2] <= 0 || l[2] >= 1) {
    stop("kappa2 needs to be within 0 and 1")
  }
  k = ncol(x)
  if (is.null(prior)) {
    prior = UninformativePrior(k, nlag)
  }
  NAMES = colnames(x)
  if (is.null(NAMES)) {
    NAMES = 1:k
  }
  beta_0.mean = prior$bprior
  beta_0.var = prior$Vprior
  Q = prior$Q
  if (is.null(Q)) {
    Q = cov(x)
  }
  create_RHS_NI = function(templag, r, nlag, t){
    K = nlag*(r^2)
    x_t = matrix(0, (t-nlag)*r, K)
    for (i in 1:(t-nlag)){
      ztemp = NULL
      for (j in 1:nlag) {
        xtemp = templag[i,((j-1)*r+1):(j*r)]
        xtemp = t(kronecker(diag(r),xtemp))
        ztemp = cbind(ztemp, xtemp)
      }
      x_t[((i-1)*r+1):(i*r),] = ztemp
    }
    return=list(x_t=x_t, K=K)
  }
  x = scale(x,TRUE,FALSE)
  YX = cbind(x,x)
  r = p = n = ncol(x)
  m = nlag*(r^2)
  k = nlag*r
  t = nrow(x)
  q = n + p
  
  # Initialize matrices
  beta_0_prmean = beta_0.mean
  beta_0_prvar = beta_0.var
  
  beta_pred = matrix(0,m,t)
  beta_update = matrix(0,m,t)
  
  Rb_t = array(0,c(m,m,t))
  Sb_t = array(0,c(m,m,t))
  
  beta_t = array(0, c(k,k,t))
  Q_t = array(0, c(r,r,t), dimnames=list(NAMES, NAMES, as.character(zoo::index(x))))
  
  # Decay and forgetting factors
  l_2 = l[2]
  l_4 = l[1]
  
  # Define lags of the factors to be used in the state (VAR) equation
  yy = x[(nlag+1):t,]
  xx = embed(x, nlag+1)[,-c(1:r)]
  templag = embed(x, nlag+1)[,-c(1:r)]
  RHS1 = create_RHS_NI(templag,r,nlag,t)
  Flagtemp = RHS1$x_t
  Flag = rbind(matrix(0, k,m), Flagtemp)
  
  ###-----| 1. KALMAN FILTER
  pb = progress_bar$new(total=t)
  for (irep in 1:t) {
    #-----| Update the state covariances
    # 1. Get the variance of the factor
    # Update Q[t]
    if (irep==1) {
      Q_t[,,irep] = Q
    } else if (irep > 1) {
      if (irep <= (nlag+1)) {
        Gf_t = 0.1*(t(matrix(x[irep,],nrow=1))%*%(x[irep,]))
      } else {
        Gf_t = t(yy[(irep-nlag),]-xx[(irep-nlag),]%*%t(B[1:r,1:k])) %*% (yy[(irep-nlag),]-xx[(irep-nlag),]%*%t(B[1:r,1:k]))
      }
      Q_t[,,irep] = l_2*Q_t[,,(irep-1)] + (1-l_2)*Gf_t[1:r,1:r]
    }
    # -for beta
    if (irep <= (nlag+1)) {
      beta_pred[,irep] = beta_0_prmean
      beta_update[,irep] = beta_pred[,irep]
      Rb_t[,,irep] = beta_0_prvar
    } else if (irep > (nlag+1)) {
      beta_pred[,irep] = beta_update[,(irep-1)]
      Rb_t[,,irep] = (1/l_4)*Sb_t[,,(irep-1)]
    }
    
    # -for beta
    if (irep >= (nlag+1)) {
      # 2/ Update VAR coefficients conditional on Principal Componets estimates
      Rx = Rb_t[,,irep]%*%t(Flag[((irep-1)*r+1):(irep*r),])
      KV_b = Q_t[,,irep] + Flag[((irep-1)*r+1):(irep*r),]%*%Rx
      KG = Rx%*%ginv(KV_b)
      beta_update[,irep] = matrix(beta_pred[,irep], ncol=1) + (KG%*%(t(matrix(x[irep,], nrow=1))-Flag[((irep-1)*r+1):(irep*r),]%*%matrix(beta_pred[,irep], ncol=1)) )
      Sb_t[,,irep] = Rb_t[,,irep] - KG%*%(Flag[((irep-1)*r+1):(irep*r),]%*%Rb_t[,,irep])
    }
    
    # Assign coefficients
    bb = matrix(beta_update[,irep], ncol=1)
    splace = 0
    biga = matrix(0, r,r*nlag)
    for (ii in 1:nlag) {
      for (iii in 1:r) {
        biga[iii,((ii-1)*r+1):(ii*r)] = t(bb[(splace+1):((splace+r)),1])
        splace = splace + r
      }
    }
    B = rbind(biga, cbind(diag(r*(nlag-1)), matrix(0, nrow=r*(nlag-1), ncol=r)))
    
    if ((max(abs(eigen(B)$values))<=1)||(irep==1)){
      beta_t[,,irep] = B
    } else {
      beta_t[,,irep] = beta_t[,,(irep-1)]
      beta_update[,irep] = 0.99*beta_update[,(irep-1)]
    }
    pb$tick()
  }
  B_t = beta_t[1:ncol(Q_t),,]
  return = list(B_t=B_t, Q_t=Q_t)
}




