# library
library(xts)
library(forecast)
library(ggplot2)
library(reshape2)
library(bstats)		# unavailable for R 3.11, deprecated from next codes
library(tseries)	# masked tseries::white.test
library(urca)
library(cpm) 		# structural change detection, unavailable for R 3.11
library(fitdistrplus)
library(ccgarch)
library(rugarch) 	# masked fitdistrplus::fitdist 
library(rmgarch)
library(parallel)	# unavailable for R 3.11
library(FinTS)
library(psych)
library(fUnitRoots)
library(vars)
library(igraph)
library(tcltk)		# unavailable for R 3.11
library(PerformanceAnalytics)
library(statnet)
library(statnet.common)
library(rgl)

# import data
base.date<-read.delim("D:/Thesis/R/base.date.txt")
base.name<-read.delim("D:/Thesis/R/base.name.txt")
base.fin<-read.delim("D:/Thesis/R/base.fin.txt")
base.cg<-read.delim("D:/Thesis/R/base.cg.txt")
base.ind<-read.delim("D:/Thesis/R/base.ind.txt")

# remove date column from base
base.fin<-base.fin[,-1]
base.cg<-base.cg[,-1]
base.ind<-base.ind[,-1]

# extract date and establist time stamp structure
base.date<-as.Date(base.date[,1],"%m/%d/%Y")

ret.fin=diff(log(as.matrix(base.fin)))
colnames(ret.fin)<-paste("ret.",colnames(base.fin),sep="")
ret.fin=as.data.frame(ret.fin)
ret.fin=xts(ret.fin,date.ret)

ret.cg=diff(log(as.matrix(base.cg)))
colnames(ret.cg)<-paste("ret.",colnames(base.cg),sep="")
ret.cg=as.data.frame(ret.cg)
ret.cg=xts(ret.cg,date.ret)

ret.ind=diff(log(as.matrix(base.ind)))
colnames(ret.ind)<-paste("ret.",colnames(base.ind),sep="")
ret.ind=as.data.frame(ret.ind)
ret.ind=xts(ret.ind,date.ret)

# extract financial, costumer goods, industrial indices from base
for(i in 1:15)
	{
		assign(paste("fin.",colnames(base.name[i]),sep=""),xts((base.fin[,i]),base.date))
		assign(paste("cg.",colnames(base.name[i]),sep=""),xts(base.cg[,i],base.date))
		assign(paste("ind.",colnames(base.name[i]),sep=""),xts(base.ind[,i],base.date))
	}
	
# plot indices
pdf("base.pdf",paper="a4r",width=11.69,height=8.27)
# FIN
	ggplot(melt(cbind(base.date,base.fin),id="base.date"),aes(x=base.date,y=value,colour=variable,group=variable))+geom_line()+xlab("Date") +  ylab("base.fin") +ggtitle("Financial Indices")
# CG
	ggplot(melt(cbind(base.date,base.cg),id="base.date"),aes(x=base.date,y=value,colour=variable,group=variable))+geom_line()+xlab("Date") +  ylab("base.cg") +ggtitle("Consumer Goods Indices")
# IND
	ggplot(melt(cbind(base.date,base.ind),id="base.date"),aes(x=base.date,y=value,colour=variable,group=variable))+geom_line()+xlab("Date") +  ylab("base.ind") +ggtitle("Industrial Indices")
dev.off()

# calculate return
for(i in 1:15)
	{
	assign(paste("ret.fin.",colnames(base.name[i]),sep=""),diff(log(get(paste("fin.",colnames(base.name[i]),sep=""))))[-1])
	assign(paste("ret.cg.",colnames(base.name[i]),sep=""),diff(log(get(paste("cg.",colnames(base.name[i]),sep=""))))[-1])
	assign(paste("ret.ind.",colnames(base.name[i]),sep=""),diff(log(get(paste("ind.",colnames(base.name[i]),sep=""))))[-1])
	}
	
ret.fin<-diff(as.matrix(log(base.fin)))
colnames(ret.fin)<-paste("ret.",colnames(base.fin),sep="")

# find non stationary ret. and treat them if necessary
for (i in 1:15)
{
if (ndiffs(ts(get(paste("ret.fin.",colnames(base.name[i]),sep=""))))!=0)
	{
	if (exists("dcc.fit.adj.ret.fin")==FALSE)
		{
		assign("dcc.fit.adj.ret.fin",0)
		}
	if((dcc.fit.adj.ret.fin==0) | (dcc.fit.adj.ret.fin<=ndiffs(ts(get(paste("ret.fin.",colnames(base.name[i]),sep=""))))))
		{
		assign(paste("dcc.fit.adj.ret.fin"),ndiffs(ts(get(paste("ret.fin.",colnames(base.name[i]),sep="")))))
		}
	print(c(paste("ret.fin.",colnames(base.name[i]),sep=""),paste("ndiffs=",ndiffs(ts(get(paste("ret.fin.",colnames(base.name[i]),sep="")))),sep="")))
	assign(paste("old.ret.fin.",colnames(base.name[i]),sep=""),get(paste("ret.fin.",colnames(base.name[i]),sep="")))
	assign(paste("ret.fin.",colnames(base.name[i]),sep=""),diff(get(paste("ret.fin.",colnames(base.name[i]),sep=""))))
	assign(paste("ret.fin.",colnames(base.name[i]),sep=""),get(paste("ret.fin.",colnames(base.name[i]),sep=""))[-1])
	}
if (ndiffs(ts(get(paste("ret.cg.",colnames(base.name[i]),sep=""))))!=0)
	{
	if (exists("dcc.fit.adj.ret.cg")==FALSE)
		{
		assign("dcc.fit.adj.ret.cg",0)
		}
	if((dcc.fit.adj.ret.cg==0) | (dcc.fit.adj.ret.cg<=ndiffs(ts(get(paste("ret.cg.",colnames(base.name[i]),sep=""))))))
		{
		assign(paste("dcc.fit.adj.ret.cg"),ndiffs(ts(get(paste("ret.cg.",colnames(base.name[i]),sep="")))))
		}
	print(c(paste("ret.cg.",colnames(base.name[i]),sep=""),paste("ndiffs=",ndiffs(ts(get(paste("ret.cg.",colnames(base.name[i]),sep="")))),sep="")))
	assign(paste("old.ret.cg.",colnames(base.name[i]),sep=""),get(paste("ret.cg.",colnames(base.name[i]),sep="")))
	assign(paste("ret.cg.",colnames(base.name[i]),sep=""),diff(get(paste("ret.cg.",colnames(base.name[i]),sep=""))))
	assign(paste("ret.cg.",colnames(base.name[i]),sep=""),get(paste("ret.cg.",colnames(base.name[i]),sep=""))[-1])
	}
if (ndiffs(ts(get(paste("ret.ind.",colnames(base.name[i]),sep=""))))!=0)
	{
	if (exists("dcc.fit.adj.ret.ind")==FALSE)
		{
		assign("dcc.fit.adj.ret.ind",0)
		}
	if((dcc.fit.adj.ret.ind==0) | (dcc.fit.adj.ret.ind<=ndiffs(ts(get(paste("ret.ind.",colnames(base.name[i]),sep=""))))))
		{
		assign(paste("dcc.fit.adj.ret.ind"),ndiffs(ts(get(paste("ret.ind.",colnames(base.name[i]),sep="")))))
		}
	print(c(paste("ret.ind.",colnames(base.name[i]),sep=""),paste("ndiffs=",ndiffs(ts(get(paste("ret.ind.",colnames(base.name[i]),sep="")))),sep="")))
	assign(paste("old.ret.ind.",colnames(base.name[i]),sep=""),get(paste("ret.ind.",colnames(base.name[i]),sep="")))
	assign(paste("ret.ind.",colnames(base.name[i]),sep=""),diff(get(paste("ret.ind.",colnames(base.name[i]),sep=""))))
	assign(paste("ret.ind.",colnames(base.name[i]),sep=""),get(paste("ret.ind.",colnames(base.name[i]),sep=""))[-1])
	}
}

####################################################################################################################
# STATIONARITY TEST
# testing for stationarity: pp test
for(i in 1:15)
	{
	assign(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""),urppTest(ts(get(paste("ret.fin.",colnames(base.name[i]),sep=""))),type="Z-tau"))
	assign(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""),urppTest(ts(get(paste("ret.cg.",colnames(base.name[i]),sep=""))),type="Z-tau"))
	assign(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""),urppTest(ts(get(paste("ret.ind.",colnames(base.name[i]),sep=""))),type="Z-tau"))
	}


# testing for stationarity: kpss test
for(i in 1:15)
	{
	assign(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""),urkpssTest(ts(get(paste("ret.fin.",colnames(base.name[i]),sep="")))))
	assign(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""),urkpssTest(ts(get(paste("ret.cg.",colnames(base.name[i]),sep="")))))
	assign(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""),urkpssTest(ts(get(paste("ret.ind.",colnames(base.name[i]),sep="")))))
	}


# getting whether H0 rejected or not
# pp test
num.pp.ret<-c()
num.pp.ret<-data.frame(num.pp.ret)
for (i in 1:15)
	{
	num.pp.ret["num.pp.ret.fin.1pct",i]<-abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.pp.ret["num.pp.ret.fin.1pct.cval",i]<-abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	num.pp.ret["num.pp.ret.fin.5pct",i]<-abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.pp.ret["num.pp.ret.fin.5pct.cval",i]<-abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	num.pp.ret["num.pp.ret.fin.10pct",i]<-abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.pp.ret["num.pp.ret.fin.10pct.cval",i]<-abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	num.pp.ret["num.pp.ret.cg.1pct",i]<-abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.pp.ret["num.pp.ret.cg.1pct.cval",i]<-abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	num.pp.ret["num.pp.ret.cg.5pct",i]<-abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.pp.ret["num.pp.ret.cg.5pct.cval",i]<-abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	num.pp.ret["num.pp.ret.cg.10pct",i]<-abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.pp.ret["num.pp.ret.cg.10pct.cval",i]<-abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	num.pp.ret["num.pp.ret.ind.1pct",i]<-abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.pp.ret["num.pp.ret.ind.1pct.cval",i]<-abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	num.pp.ret["num.pp.ret.ind.5pct",i]<-abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.pp.ret["num.pp.ret.ind.5pct.cval",i]<-abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	num.pp.ret["num.pp.ret.ind.10pct",i]<-abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.pp.ret["num.pp.ret.ind.10pct.cval",i]<-abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	}
colnames(num.pp.ret)<-paste("ret.",colnames(base.name),sep="")

pp.ret<-c()
pp.ret<-data.frame(pp.ret)
for (i in 1:15)
	{
	pp.ret["pp.ret.fin.1pct",i]<-abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	pp.ret["pp.ret.fin.5pct",i]<-abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	pp.ret["pp.ret.fin.10pct",i]<-abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	pp.ret["pp.ret.cg.1pct",i]<-abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	pp.ret["pp.ret.cg.5pct",i]<-abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	pp.ret["pp.ret.cg.10pct",i]<-abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	pp.ret["pp.ret.ind.1pct",i]<-abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	pp.ret["pp.ret.ind.5pct",i]<-abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	pp.ret["pp.ret.ind.10pct",i]<-abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	}
colnames(pp.ret)<-paste("ret.",colnames(base.name),sep="")

# kpss test

num.kpss.ret<-c()
num.kpss.ret<-data.frame(num.kpss.ret)
for (i in 1:15)
	{
	num.kpss.ret["num.kpss.ret.fin.1pct",i]<-abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret["num.kpss.ret.fin.1pct.cval",i]<-abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	num.kpss.ret["num.kpss.ret.fin.5pct",i]<-abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret["num.kpss.ret.fin.5pct.cval",i]<-abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	num.kpss.ret["num.kpss.ret.fin.10pct",i]<-abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret["num.kpss.ret.fin.10pct.cval",i]<-abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	num.kpss.ret["num.kpss.ret.cg.1pct",i]<-abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret["num.kpss.ret.cg.1pct.cval",i]<-abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	num.kpss.ret["num.kpss.ret.cg.5pct",i]<-abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret["num.kpss.ret.cg.5pct.cval",i]<-abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	num.kpss.ret["num.kpss.ret.cg.10pct",i]<-abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret["num.kpss.ret.cg.10pct.cval",i]<-abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	num.kpss.ret["num.kpss.ret.ind.1pct",i]<-abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret["num.kpss.ret.ind.1pct.cval",i]<-abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	num.kpss.ret["num.kpss.ret.ind.5pct",i]<-abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret["num.kpss.ret.ind.5pct.cval",i]<-abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	num.kpss.ret["num.kpss.ret.ind.10pct",i]<-abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret["num.kpss.ret.ind.10pct.cval",i]<-abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	}
colnames(num.kpss.ret)<-paste("ret.",colnames(base.name),sep="")

kpss.ret<-c()
kpss.ret<-data.frame(kpss.ret)
for (i in 1:15)
	{
	kpss.ret["kpss.ret.fin.1pct",i]<-abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	kpss.ret["kpss.ret.fin.5pct",i]<-abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	kpss.ret["kpss.ret.fin.10pct",i]<-abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	kpss.ret["kpss.ret.cg.1pct",i]<-abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	kpss.ret["kpss.ret.cg.5pct",i]<-abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	kpss.ret["kpss.ret.cg.10pct",i]<-abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	kpss.ret["kpss.ret.ind.1pct",i]<-abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[1])
	kpss.ret["kpss.ret.ind.5pct",i]<-abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[2])
	kpss.ret["kpss.ret.ind.10pct",i]<-abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[3])
	}
colnames(kpss.ret)<-paste("ret.",colnames(base.name),sep="")


#####################################################################################
# DECOMPOSITION
# assign series to ts class to be used in decomposition
for (i in 1:15)
	{
	assign(paste("ts.fin.",colnames(base.name[i]),sep=""),ts(data=get(paste("fin.",colnames(base.name[i]),sep="")),frequency=22))
	assign(paste("ts.cg.",colnames(base.name[i]),sep=""),ts(data=get(paste("cg.",colnames(base.name[i]),sep="")),frequency=22))
	assign(paste("ts.ind.",colnames(base.name[i]),sep=""),ts(data=get(paste("ind.",colnames(base.name[i]),sep="")),frequency=22))
	}

for (i in 1:15)
	{
	assign(paste("ts.ret.fin.",colnames(base.name[i]),sep=""),ts(data=get(paste("ret.fin.",colnames(base.name[i]),sep="")),frequency=22))
	assign(paste("ts.ret.cg.",colnames(base.name[i]),sep=""),ts(data=get(paste("ret.cg.",colnames(base.name[i]),sep="")),frequency=22))
	assign(paste("ts.ret.ind.",colnames(base.name[i]),sep=""),ts(data=get(paste("ret.ind.",colnames(base.name[i]),sep="")),frequency=22))
	}
	
	
# How to Choose Between Additive and Multiplicative Decompositions
# The additive model is useful when the seasonal variation is relatively constant over time.
# The multiplicative model is useful when the seasonal variation increases over time.
# decomposition of base series

# write function for plotting decomposed object so that we may change tittle
decomp.plot <- function(x, main = NULL, ...)
{
    if(is.null(main))
        main <- paste("Decomposition of", x$type, "time series")
    plot(cbind(observed = x$random + if (x$type == "additive")
        x$trend + x$seasonal
               else x$trend * x$seasonal, trend = x$trend, seasonal = x$seasonal,
               random = x$random), main = main, ...)
} 

pdf("base decomp.pdf",paper="a4r",width=11.69,height=8.27)
for (i in 1:15)
	{
	decomp.plot(decompose(get(paste("ts.fin.",colnames(base.name[i]),sep="")),type = c("additive")),main=paste("Decomposition of additive time series","- fin.",colnames(base.name[i]),sep=""))
	decomp.plot(decompose(get(paste("ts.cg.",colnames(base.name[i]),sep="")),type = c("additive")),main=paste("Decomposition of additive time series","- cg.",colnames(base.name[i]),sep=""))
	decomp.plot(decompose(get(paste("ts.ind.",colnames(base.name[i]),sep="")),type = c("additive")),main=paste("Decomposition of additive time series ","- ind.",colnames(base.name[i]),sep=""))
	}
dev.off()

pdf("ret decomp.pdf",paper="a4r",width=11.69,height=8.27)
for (i in 1:15)
	{
	decomp.plot(decompose(get(paste("ts.ret.fin.",colnames(base.name[i]),sep="")),type = c("additive")),main=paste("Decomposition of additive time series","- ret.fin.",colnames(base.name[i]),sep=""))
	decomp.plot(decompose(get(paste("ts.ret.cg.",colnames(base.name[i]),sep="")),type = c("additive")),main=paste("Decomposition of additive time series","- ret.cg.",colnames(base.name[i]),sep=""))
	decomp.plot(decompose(get(paste("ts.ret.ind.",colnames(base.name[i]),sep="")),type = c("additive")),main=paste("Decomposition of additive time series","- ret.ind.",colnames(base.name[i]),sep=""))
	}
dev.off()


#####################################################################################
# DISTRIBUTION FIT
# distribution test of ret.
for (i in 1:15)
	{
	assign(paste("dist.ret.fin.",colnames(base.name[i]),sep=""),fitdistrplus::fitdist(as.numeric(get(paste("ret.fin.",colnames(base.name[i]),sep=""))), "norm"))
	assign(paste("dist.ret.cg.",colnames(base.name[i]),sep=""),fitdistrplus::fitdist(as.numeric(get(paste("ret.cg.",colnames(base.name[i]),sep=""))), "norm"))
	assign(paste("dist.ret.ind.",colnames(base.name[i]),sep=""),fitdistrplus::fitdist(as.numeric(get(paste("ret.ind.",colnames(base.name[i]),sep=""))), "norm"))
	}
	
		
#plotting the dist test
pdf("dist.pdf",paper="a4r",width=11.69,height=8.27)
for(i in 1:15)
	{
	plot(get(paste("dist.ret.fin.",colnames(base.name[i]),sep="")),sub=paste("ret.fin.",colnames(base.name[i]),sep=""))
	plot(get(paste("dist.ret.cg.",colnames(base.name[i]),sep="")),sub=paste("ret.cg.",colnames(base.name[i]),sep=""))
	plot(get(paste("dist.ret.ind.",colnames(base.name[i]),sep="")),sub=paste("ret.ind.",colnames(base.name[i]),sep=""))
	}
dev.off()


####################################################################################################################
# ACF & PACF
# plot acf and pacf
pdf("ret acf-pacf.pdf",paper="a4r",width=11.69,height=8.27)
	for(i in 1:15)
		{
		Acf(get(paste("ret.",colnames(base.fin[i]),sep="")),main=paste("ret.",colnames(base.fin[i]),sep=""))
		Acf(get(paste("ret.",colnames(base.cg[i]),sep="")),main=paste("ret.",colnames(base.cg[i]),sep=""))
		Acf(get(paste("ret.",colnames(base.ind[i]),sep="")),main=paste("ret.",colnames(base.ind[i]),sep=""))
		Pacf(get(paste("ret.",colnames(base.fin[i]),sep="")),main=paste("ret.",colnames(base.fin[i]),sep=""))
		Pacf(get(paste("ret.",colnames(base.cg[i]),sep="")),main=paste("ret.",colnames(base.cg[i]),sep=""))
		Pacf(get(paste("ret.",colnames(base.ind[i]),sep="")),main=paste("ret.",colnames(base.ind[i]),sep=""))
		}
dev.off()

# find the appropriate ARIMA(p,d,q) function
for(i in 1:15)
	{
	assign(paste("arima.ret.fin.",colnames(base.name[i]),sep=""),auto.arima(ts(get(paste("ret.fin.",colnames(base.name[i]),sep=""))),trace=TRUE,allowdrift=TRUE,approximation=FALSE,parallel=TRUE,stepwise=FALSE,num.cores=2))
	assign(paste("arima.ret.cg.",colnames(base.name[i]),sep=""),auto.arima(ts(get(paste("ret.cg.",colnames(base.name[i]),sep=""))),trace=TRUE,allowdrift=TRUE,approximation=FALSE,parallel=TRUE,stepwise=FALSE,num.cores=2))
	assign(paste("arima.ret.ind.",colnames(base.name[i]),sep=""),auto.arima(ts(get(paste("ret.ind.",colnames(base.name[i]),sep=""))),trace=TRUE,allowdrift=TRUE,approximation=FALSE,parallel=TRUE,stepwise=FALSE,num.cores=2))
	}

# extract residual from ARIMA process
	for (i in 1:15)
	{
assign(paste("resid.ret.fin.",colnames(base.name[i]),sep=""),xts(residuals(get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))),date.ret[1:(2749-ndiffs(diff(log(ts(base.fin[i])))))]))

assign(paste("resid.ret.cg.",colnames(base.name[i]),sep=""),xts(residuals(get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))),date.ret[1:(2749-ndiffs(diff(log(ts(base.cg[i])))))]))

assign(paste("resid.ret.ind.",colnames(base.name[i]),sep=""),xts(residuals(get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))),date.ret[1:(2749-ndiffs(diff(log(ts(base.ind[i])))))]))


#####################################################################################
# WRAPPER TO COLLECT ARIMA ORDER	
# arima order
arima.ret.fin<-c()
arima.ret.fin<-data.frame(arima.ret.fin)
for (i in 1:15)
	{
	arima.ret.fin[paste(colnames(ret.fin[,i])),"ARp"]<-get(paste("arima.",colnames(ret.fin[,i]),sep=""))$arma[1]
	arima.ret.fin[paste(colnames(ret.fin[,i])),"MAq"]<-get(paste("arima.",colnames(ret.fin[,i]),sep=""))$arma[2]
	}
	
arima.ret.cg<-c()
arima.ret.cg<-data.frame(arima.ret.cg)
for (i in 1:15)
	{
	arima.ret.cg[paste(colnames(ret.cg[,i])),"ARp"]<-get(paste("arima.",colnames(ret.cg[,i]),sep=""))$arma[1]
	arima.ret.cg[paste(colnames(ret.cg[,i])),"MAq"]<-get(paste("arima.",colnames(ret.cg[,i]),sep=""))$arma[2]
	}
	
arima.ret.ind<-c()
arima.ret.ind<-data.frame(arima.ret.ind)
for (i in 1:15)
	{
	arima.ret.ind[paste(colnames(ret.ind[,i])),"ARp"]<-get(paste("arima.",colnames(ret.ind[,i]),sep=""))$arma[1]
	arima.ret.ind[paste(colnames(ret.ind[,i])),"MAq"]<-get(paste("arima.",colnames(ret.ind[,i]),sep=""))$arma[2]
	}


#################################################################################	
# FIRST UNIVARIATE GARCH
# specify ugarchspec
for(i in 1:15)
	{
	assign(paste("spec.ugarch.ret.fin.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "norm", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.ugarch.ret.cg.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "norm", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.ugarch.ret.ind.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "norm", start.pars = list(), fixed.pars = list()))
	}
	
# fit univariate garch
for (i in 1:15)
	{
	assign(paste("ugarch.ret.fin.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.ugarch.ret.fin.",colnames(base.name[i]),sep="")), get(paste("ret.fin.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("ugarch.ret.cg.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.ugarch.ret.cg.",colnames(base.name[i]),sep="")), get(paste("ret.cg.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("ugarch.ret.ind.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.ugarch.ret.ind.",colnames(base.name[i]),sep="")), get(paste("ret.ind.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	}
	

#####################################################################################
#COEFFICIENT
coef.ret.fin<-c()
coef.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1))
	{
	if (i==1)
		{
		colnames(coef.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(coef.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(coef.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(coef.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(coef.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(coef.ret.fin)[i]<-paste("beta1",sep="")
		}	
	}
rownames(coef.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1))
		{
		if (is.na(coef(get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")))[j])!=TRUE)
			{
		coef.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),attributes(coef(get(paste("ugarch.ret.",colnames(base.fin[i]),sep=""))))$names[j]]<-coef(get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")))[j]
			}
		}
	}
	
coef.ret.cg<-c()
coef.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1))
	{
	if (i==1)
		{
		colnames(coef.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(coef.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(coef.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(coef.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(coef.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(coef.ret.cg)[i]<-paste("beta1",sep="")
		}	
	}
rownames(coef.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1))
		{
		if (is.na(coef(get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")))[j])!=TRUE)
			{
		coef.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),attributes(coef(get(paste("ugarch.ret.",colnames(base.cg[i]),sep=""))))$names[j]]<-coef(get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")))[j]
			}
		}
	}
	
coef.ret.ind<-c()
coef.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1))
	{
	if (i==1)
		{
		colnames(coef.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(coef.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(coef.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(coef.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(coef.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(coef.ret.ind)[i]<-paste("beta1",sep="")
		}	
	}
rownames(coef.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1))
		{
		if (is.na(coef(get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")))[j])!=TRUE)
			{
		coef.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),attributes(coef(get(paste("ugarch.ret.",colnames(base.ind[i]),sep=""))))$names[j]]<-coef(get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")))[j]
			}
		}
	}
	
# Nyblom Stability Test

nyblom.test.ret.fin<-c()
nyblom.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1))
	{
	if (i==1)
		{
		colnames(nyblom.test.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(nyblom.test.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(nyblom.test.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(nyblom.test.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(nyblom.test.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(nyblom.test.ret.fin)[i]<-paste("beta1",sep="")
		}	
	}
rownames(nyblom.test.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1))
		{
		if (is.na(nyblom(get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		nyblom.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),rownames(nyblom(get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
nyblom.test.ret.cg<-c()
nyblom.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1))
	{
	if (i==1)
		{
		colnames(nyblom.test.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(nyblom.test.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(nyblom.test.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(nyblom.test.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(nyblom.test.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(nyblom.test.ret.cg)[i]<-paste("beta1",sep="")
		}	
	}
rownames(nyblom.test.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1))
		{
		if (is.na(nyblom(get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		nyblom.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),rownames(nyblom(get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
nyblom.test.ret.ind<-c()
nyblom.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1))
	{
	if (i==1)
		{
		colnames(nyblom.test.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(nyblom.test.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(nyblom.test.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(nyblom.test.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(nyblom.test.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(nyblom.test.ret.ind)[i]<-paste("beta1",sep="")
		}	
	}
rownames(nyblom.test.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1))
		{
		if (is.na(nyblom(get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		nyblom.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),rownames(nyblom(get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	

#####################################################################################
# SIGN BIAS TEST

signbias.test.ret.fin<-c()
signbias.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(signbias.test.ret.fin)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(signbias.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(signbias.test.ret.fin)[j]]<-signbias(get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
signbias.test.ret.cg<-c()
signbias.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(signbias.test.ret.cg)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(signbias.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		signbias.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(signbias.test.ret.cg)[j]]<-signbias(get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")))$prob[j]
		}
	}
	
signbias.test.ret.ind<-c()
signbias.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(signbias.test.ret.ind)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(signbias.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		signbias.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(signbias.test.ret.ind)[j]]<-signbias(get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")))$prob[j]
		}
	}


# Goodness of Fit Test
gof.test.ret.fin<-c()
gof.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(gof.test.ret.fin)<-list("20", "30", "40", "50")
rownames(gof.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		gof.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(gof.test.ret.fin)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")),groups=as.numeric(paste(colnames(gof.test.ret.fin)[j])))[3]
		}
	}
	
gof.test.ret.cg<-c()
gof.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(gof.test.ret.cg)<-list("20", "30", "40", "50")
rownames(gof.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		gof.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(gof.test.ret.cg)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")),groups=as.numeric(paste(colnames(gof.test.ret.cg)[j])))[3]
		}
	}
	
gof.test.ret.ind<-c()
gof.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))


colnames(gof.test.ret.ind)<-list("20", "30", "40", "50")
rownames(gof.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		gof.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(gof.test.ret.ind)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")),groups=as.numeric(paste(colnames(gof.test.ret.ind)[j])))[3]
		}
	}


#################################################################################	
# SST UGARCH
# specify ugarchspec
for(i in 1:15)
	{
	assign(paste("spec.sst.ugarch.ret.fin.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "sstd", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.sst.ugarch.ret.cg.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "sstd", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.sst.ugarch.ret.ind.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "sstd", start.pars = list(), fixed.pars = list()))
	}
	
# fit univariate garch
for (i in 1:15)
	{
	assign(paste("sst.ugarch.ret.fin.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.sst.ugarch.ret.fin.",colnames(base.name[i]),sep="")), get(paste("ret.fin.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("sst.ugarch.ret.cg.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.sst.ugarch.ret.cg.",colnames(base.name[i]),sep="")), get(paste("ret.cg.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("sst.ugarch.ret.ind.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.sst.ugarch.ret.ind.",colnames(base.name[i]),sep="")), get(paste("ret.ind.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	}
	
#####################################################################################
#COEFFICIENT
sst.ugarch.coef.ret.fin<-c()
sst.ugarch.coef.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.ugarch.coef.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(sst.ugarch.coef.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(sst.ugarch.coef.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(sst.ugarch.coef.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(sst.ugarch.coef.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(sst.ugarch.coef.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(sst.ugarch.coef.ret.fin)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.ugarch.coef.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
		{
		if (is.na(coef(get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep="")))[j])!=TRUE)
			{
		sst.ugarch.coef.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),attributes(coef(get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep=""))))$names[j]]<-coef(get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep="")))[j]
			}
		}
	}
	
sst.ugarch.coef.ret.cg<-c()
sst.ugarch.coef.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.ugarch.coef.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(sst.ugarch.coef.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(sst.ugarch.coef.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(sst.ugarch.coef.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(sst.ugarch.coef.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(sst.ugarch.coef.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(sst.ugarch.coef.ret.cg)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.ugarch.coef.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
		{
		if (is.na(coef(get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep="")))[j])!=TRUE)
			{
		sst.ugarch.coef.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),attributes(coef(get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep=""))))$names[j]]<-coef(get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep="")))[j]
			}
		}
	}
	
sst.ugarch.coef.ret.ind<-c()
sst.ugarch.coef.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.ugarch.coef.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(sst.ugarch.coef.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(sst.ugarch.coef.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(sst.ugarch.coef.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(sst.ugarch.coef.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(sst.ugarch.coef.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(sst.ugarch.coef.ret.ind)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.ugarch.coef.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
		{
		if (is.na(coef(get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep="")))[j])!=TRUE)
			{
		sst.ugarch.coef.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),attributes(coef(get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep=""))))$names[j]]<-coef(get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep="")))[j]
			}
		}
	}
	
# Nyblom Stability Test
sst.ugarch.nyblom.test.ret.fin<-c()
sst.ugarch.nyblom.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.ugarch.nyblom.test.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(sst.ugarch.nyblom.test.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(sst.ugarch.nyblom.test.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.fin)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.ugarch.nyblom.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		sst.ugarch.nyblom.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),rownames(nyblom(get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
sst.ugarch.nyblom.test.ret.cg<-c()
sst.ugarch.nyblom.test.ret.cg<-data.frame(matrix(NA, nrow = 10, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.ugarch.nyblom.test.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(sst.ugarch.nyblom.test.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(sst.ugarch.nyblom.test.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.cg)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.ugarch.nyblom.test.ret.cg)<-colnames(ret.cg)


for (i in 1:10)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		sst.ugarch.nyblom.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),rownames(nyblom(get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j]
			}
		}
	}

sst.ugarch.nyblom.test.ret.ind<-c()
sst.ugarch.nyblom.test.ret.ind<-data.frame(matrix(NA, nrow = 10, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.ugarch.nyblom.test.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(sst.ugarch.nyblom.test.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(sst.ugarch.nyblom.test.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(sst.ugarch.nyblom.test.ret.ind)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.ugarch.nyblom.test.ret.ind)<-colnames(ret.ind)


for (i in 1:10)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		sst.ugarch.nyblom.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),rownames(nyblom(get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
#####################################################################################
# SIGN BIAS TEST


sst.ugarch.signbias.test.ret.fin<-c()
sst.ugarch.signbias.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.ugarch.signbias.test.ret.fin)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(sst.ugarch.signbias.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.ugarch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(sst.ugarch.signbias.test.ret.fin)[j]]<-signbias(get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
sst.ugarch.signbias.test.ret.cg<-c()
sst.ugarch.signbias.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.ugarch.signbias.test.ret.cg)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(sst.ugarch.signbias.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.ugarch.signbias.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(sst.ugarch.signbias.test.ret.cg)[j]]<-signbias(get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep="")))$prob[j]
		}
	}
	
sst.ugarch.signbias.test.ret.ind<-c()
sst.ugarch.signbias.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.ugarch.signbias.test.ret.ind)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(sst.ugarch.signbias.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.ugarch.signbias.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(sst.ugarch.signbias.test.ret.ind)[j]]<-signbias(get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep="")))$prob[j]
		}
	}

for (i in 1:10)
	{
	for (j in 1:4)
		{
		sst.ugarch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(sst.ugarch.signbias.test.ret.fin)[j]]<-signbias(get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
	
# Goodness of Fit Test
sst.ugarch.gof.test.ret.fin<-c()
sst.ugarch.gof.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.ugarch.gof.test.ret.fin)<-list("20", "30", "40", "50")
rownames(sst.ugarch.gof.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.ugarch.gof.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(sst.ugarch.gof.test.ret.fin)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")),groups=as.numeric(paste(colnames(sst.ugarch.gof.test.ret.fin)[j])))[3]
		}
	}
	
sst.ugarch.gof.test.ret.cg<-c()
sst.ugarch.gof.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.ugarch.gof.test.ret.cg)<-list("20", "30", "40", "50")
rownames(sst.ugarch.gof.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.ugarch.gof.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(sst.ugarch.gof.test.ret.cg)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")),groups=as.numeric(paste(colnames(sst.ugarch.gof.test.ret.cg)[j])))[3]
		}
	}
	
sst.ugarch.gof.test.ret.ind<-c()
sst.ugarch.gof.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))


colnames(sst.ugarch.gof.test.ret.ind)<-list("20", "30", "40", "50")
rownames(sst.ugarch.gof.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.ugarch.gof.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(sst.ugarch.gof.test.ret.ind)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")),groups=as.numeric(paste(colnames(sst.ugarch.gof.test.ret.ind)[j])))[3]
		}
	}

#################################################################################	
# EGARCH
# specify ugarchspec
for(i in 1:15)
	{
	assign(paste("spec.egarch.ret.fin.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "norm", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.egarch.ret.cg.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "norm", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.egarch.ret.ind.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "norm", start.pars = list(), fixed.pars = list()))
	}
	
# fit univariate garch
for (i in 1:15)
	{
	assign(paste("egarch.ret.fin.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.egarch.ret.fin.",colnames(base.name[i]),sep="")), get(paste("retfin.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("egarch.ret.cg.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.egarch.ret.cg.",colnames(base.name[i]),sep="")), get(paste("retcg.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("egarch.ret.ind.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.egarch.ret.ind.",colnames(base.name[i]),sep="")), get(paste("retind.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	}
	

#####################################################################################
#COEFFICIENT
egarch.coef.ret.fin<-c()
egarch.coef.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(egarch.coef.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(egarch.coef.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(egarch.coef.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(egarch.coef.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(egarch.coef.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(egarch.coef.ret.fin)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(egarch.coef.ret.fin)[i]<-paste("gamma1",sep="")
		}
	}
rownames(egarch.coef.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
		{
		if (is.na(coef(get(paste("egarch.ret.",colnames(base.fin[i]),sep="")))[j])!=TRUE)
			{
		egarch.coef.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),attributes(coef(get(paste("egarch.ret.",colnames(base.fin[i]),sep=""))))$names[j]]<-coef(get(paste("egarch.ret.",colnames(base.fin[i]),sep="")))[j]
			}
		}
	}

egarch.coef.ret.cg<-c()
egarch.coef.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(egarch.coef.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(egarch.coef.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(egarch.coef.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(egarch.coef.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(egarch.coef.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(egarch.coef.ret.cg)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(egarch.coef.ret.cg)[i]<-paste("gamma1",sep="")
		}
	}
rownames(egarch.coef.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
		{
		if (is.na(coef(get(paste("egarch.ret.",colnames(base.cg[i]),sep="")))[j])!=TRUE)
			{
		egarch.coef.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),attributes(coef(get(paste("egarch.ret.",colnames(base.cg[i]),sep=""))))$names[j]]<-coef(get(paste("egarch.ret.",colnames(base.cg[i]),sep="")))[j]
			}
		}
	}

egarch.coef.ret.ind<-c()
egarch.coef.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(egarch.coef.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(egarch.coef.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(egarch.coef.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(egarch.coef.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(egarch.coef.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(egarch.coef.ret.ind)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(egarch.coef.ret.ind)[i]<-paste("gamma1",sep="")
		}
	}
rownames(egarch.coef.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
		{
		if (is.na(coef(get(paste("egarch.ret.",colnames(base.ind[i]),sep="")))[j])!=TRUE)
			{
		egarch.coef.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),attributes(coef(get(paste("egarch.ret.",colnames(base.ind[i]),sep=""))))$names[j]]<-coef(get(paste("egarch.ret.",colnames(base.ind[i]),sep="")))[j]
			}
		}
	}

#####################################################################################
# Nyblom Stability Test

egarch.nyblom.test.ret.fin<-c()
egarch.nyblom.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(egarch.nyblom.test.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(egarch.nyblom.test.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(egarch.nyblom.test.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(egarch.nyblom.test.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(egarch.nyblom.test.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(egarch.nyblom.test.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(egarch.nyblom.test.ret.fin)[i]<-paste("gamma1",sep="")
		}	
	}
rownames(egarch.nyblom.test.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		egarch.nyblom.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),rownames(nyblom(get(paste("egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j]
			}
		}
	}

egarch.nyblom.test.ret.cg<-c()
egarch.nyblom.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(egarch.nyblom.test.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(egarch.nyblom.test.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(egarch.nyblom.test.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(egarch.nyblom.test.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(egarch.nyblom.test.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(egarch.nyblom.test.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(egarch.nyblom.test.ret.cg)[i]<-paste("gamma1",sep="")
		}	
	}
rownames(egarch.nyblom.test.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		egarch.nyblom.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),rownames(nyblom(get(paste("egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
egarch.nyblom.test.ret.ind<-c()
egarch.nyblom.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(egarch.nyblom.test.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(egarch.nyblom.test.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(egarch.nyblom.test.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(egarch.nyblom.test.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(egarch.nyblom.test.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(egarch.nyblom.test.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(egarch.nyblom.test.ret.ind)[i]<-paste("gamma1",sep="")
		}	
	}
rownames(egarch.nyblom.test.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		egarch.nyblom.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),rownames(nyblom(get(paste("egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j]
			}
		}
	}
#####################################################################################
# SIGN BIAS TEST

egarch.signbias.test.ret.fin<-c()
egarch.signbias.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(egarch.signbias.test.ret.fin)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(egarch.signbias.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		egarch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(egarch.signbias.test.ret.fin)[j]]<-signbias(get(paste("egarch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
egarch.signbias.test.ret.cg<-c()
egarch.signbias.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(egarch.signbias.test.ret.cg)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(egarch.signbias.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		egarch.signbias.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(egarch.signbias.test.ret.cg)[j]]<-signbias(get(paste("egarch.ret.",colnames(base.cg[i]),sep="")))$prob[j]
		}
	}
	
egarch.signbias.test.ret.ind<-c()
egarch.signbias.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(egarch.signbias.test.ret.ind)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(egarch.signbias.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		egarch.signbias.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(egarch.signbias.test.ret.ind)[j]]<-signbias(get(paste("egarch.ret.",colnames(base.ind[i]),sep="")))$prob[j]
		}
	}

for (i in 1:10)
	{
	for (j in 1:4)
		{
		egarch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(egarch.signbias.test.ret.fin)[j]]<-signbias(get(paste("egarch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
	
# Goodness of Fit Test
egarch.gof.test.ret.fin<-c()
egarch.gof.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(egarch.gof.test.ret.fin)<-list("20", "30", "40", "50")
rownames(egarch.gof.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		egarch.gof.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(egarch.gof.test.ret.fin)[j]]<-rugarch::gof(get(paste("egarch.ret.",colnames(base.fin[i]),sep="")),groups=as.numeric(paste(colnames(egarch.gof.test.ret.fin)[j])))[3]
		}
	}
	
egarch.gof.test.ret.cg<-c()
egarch.gof.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(egarch.gof.test.ret.cg)<-list("20", "30", "40", "50")
rownames(egarch.gof.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		egarch.gof.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(egarch.gof.test.ret.cg)[j]]<-rugarch::gof(get(paste("egarch.ret.",colnames(base.cg[i]),sep="")),groups=as.numeric(paste(colnames(egarch.gof.test.ret.cg)[j])))[3]
		}
	}
	
egarch.gof.test.ret.ind<-c()
egarch.gof.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))


colnames(egarch.gof.test.ret.ind)<-list("20", "30", "40", "50")
rownames(egarch.gof.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		egarch.gof.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(egarch.gof.test.ret.ind)[j]]<-rugarch::gof(get(paste("egarch.ret.",colnames(base.ind[i]),sep="")),groups=as.numeric(paste(colnames(egarch.gof.test.ret.ind)[j])))[3]
		}
	}


#################################################################################	
# ST EGARCH
# specify ugarchspec
for(i in 1:15)
	{
	assign(paste("spec.st.egarch.ret.fin.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "std", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.st.egarch.ret.cg.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "std", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.st.egarch.ret.ind.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "std", start.pars = list(), fixed.pars = list()))
	}
	
# fit univariate garch
for (i in 1:15)
	{
	assign(paste("st.egarch.ret.fin.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.st.egarch.ret.fin.",colnames(base.name[i]),sep="")), get(paste("ret.fin.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("st.egarch.ret.cg.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.st.egarch.ret.cg.",colnames(base.name[i]),sep="")), get(paste("ret.cg.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("st.egarch.ret.ind.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.st.egarch.ret.ind.",colnames(base.name[i]),sep="")), get(paste("ret.ind.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	}


#######################################################################################COEFFICIENT
st.egarch.coef.ret.fin<-c()
st.egarch.coef.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("shape",sep="")
		}			
	}
rownames(st.egarch.coef.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))[j])!=TRUE)
			{
		st.egarch.coef.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),attributes(coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep=""))))$names[j]]<-coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))[j]
			}
		}
	}

st.egarch.coef.ret.fin<-c()
st.egarch.coef.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("shape",sep="")
		}			
	}
rownames(st.egarch.coef.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))[j])!=TRUE)
			{
		st.egarch.coef.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),attributes(coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep=""))))$names[j]]<-coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))[j]
			}
		}
	}
	
st.egarch.coef.ret.cg<-c()
st.egarch.coef.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("shape",sep="")
		}			
	}
rownames(st.egarch.coef.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))[j])!=TRUE)
			{
		st.egarch.coef.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),attributes(coef(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep=""))))$names[j]]<-coef(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))[j]
			}
		}
	}
	
st.egarch.coef.ret.ind<-c()
st.egarch.coef.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("shape",sep="")
		}			
	}
rownames(st.egarch.coef.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))[j])!=TRUE)
			{
		st.egarch.coef.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),attributes(coef(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep=""))))$names[j]]<-coef(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))[j]
			}
		}
	}
	
	
# Nyblom Stability Test
st.egarch.nyblom.test.ret.fin<-c()
st.egarch.nyblom.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("shape",sep="")
		}
	}
rownames(st.egarch.nyblom.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		st.egarch.nyblom.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),rownames(nyblom(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j]
			}
		}
	}

st.egarch.nyblom.test.ret.cg<-c()
st.egarch.nyblom.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("shape",sep="")
		}
	}
rownames(st.egarch.nyblom.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		st.egarch.nyblom.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),rownames(nyblom(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j]
			}
		}
	}

st.egarch.nyblom.test.ret.ind<-c()
st.egarch.nyblom.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("shape",sep="")
		}
	}
rownames(st.egarch.nyblom.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		st.egarch.nyblom.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),rownames(nyblom(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
#####################################################################################
# SIGN BIAS TEST


st.egarch.signbias.test.ret.fin<-c()
st.egarch.signbias.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.egarch.signbias.test.ret.fin)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(st.egarch.signbias.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(st.egarch.signbias.test.ret.fin)[j]]<-signbias(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
st.egarch.signbias.test.ret.cg<-c()
st.egarch.signbias.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.egarch.signbias.test.ret.cg)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(st.egarch.signbias.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.signbias.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(st.egarch.signbias.test.ret.cg)[j]]<-signbias(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))$prob[j]
		}
	}
	
st.egarch.signbias.test.ret.ind<-c()
st.egarch.signbias.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.egarch.signbias.test.ret.ind)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(st.egarch.signbias.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.signbias.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(st.egarch.signbias.test.ret.ind)[j]]<-signbias(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))$prob[j]
		}
	}

	
# Goodness of Fit Test
st.egarch.gof.test.ret.fin<-c()
st.egarch.gof.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.egarch.gof.test.ret.fin)<-list("20", "30", "40", "50")
rownames(st.egarch.gof.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.gof.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(st.egarch.gof.test.ret.fin)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")),groups=as.numeric(paste(colnames(st.egarch.gof.test.ret.fin)[j])))[3]
		}
	}
	
st.egarch.gof.test.ret.cg<-c()
st.egarch.gof.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.egarch.gof.test.ret.cg)<-list("20", "30", "40", "50")
rownames(st.egarch.gof.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.gof.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(st.egarch.gof.test.ret.cg)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")),groups=as.numeric(paste(colnames(st.egarch.gof.test.ret.cg)[j])))[3]
		}
	}
	
st.egarch.gof.test.ret.ind<-c()
st.egarch.gof.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))


colnames(st.egarch.gof.test.ret.ind)<-list("20", "30", "40", "50")
rownames(st.egarch.gof.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.gof.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(st.egarch.gof.test.ret.ind)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")),groups=as.numeric(paste(colnames(st.egarch.gof.test.ret.ind)[j])))[3]
		}
	}


#################################################################################	
# ST EGARCH
# specify ugarchspec
for(i in 1:15)
	{
	assign(paste("spec.st.egarch.ret.fin.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "std", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.st.egarch.ret.cg.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "std", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.st.egarch.ret.ind.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "std", start.pars = list(), fixed.pars = list()))
	}
	
# fit univariate garch
for (i in 1:15)
	{
	assign(paste("st.egarch.ret.fin.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.st.egarch.ret.fin.",colnames(base.name[i]),sep="")), get(paste("ret.fin.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("st.egarch.ret.cg.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.st.egarch.ret.cg.",colnames(base.name[i]),sep="")), get(paste("ret.cg.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("st.egarch.ret.ind.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.st.egarch.ret.ind.",colnames(base.name[i]),sep="")), get(paste("ret.ind.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	}


#######################################################################################COEFFICIENT
st.egarch.coef.ret.fin<-c()
st.egarch.coef.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("shape",sep="")
		}			
	}
rownames(st.egarch.coef.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))[j])!=TRUE)
			{
		st.egarch.coef.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),attributes(coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep=""))))$names[j]]<-coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))[j]
			}
		}
	}

st.egarch.coef.ret.fin<-c()
st.egarch.coef.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.fin)[i]<-paste("shape",sep="")
		}			
	}
rownames(st.egarch.coef.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))[j])!=TRUE)
			{
		st.egarch.coef.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),attributes(coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep=""))))$names[j]]<-coef(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))[j]
			}
		}
	}
	
st.egarch.coef.ret.cg<-c()
st.egarch.coef.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.cg)[i]<-paste("shape",sep="")
		}			
	}
rownames(st.egarch.coef.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))[j])!=TRUE)
			{
		st.egarch.coef.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),attributes(coef(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep=""))))$names[j]]<-coef(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))[j]
			}
		}
	}
	
st.egarch.coef.ret.ind<-c()
st.egarch.coef.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.coef.ret.ind)[i]<-paste("shape",sep="")
		}			
	}
rownames(st.egarch.coef.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))[j])!=TRUE)
			{
		st.egarch.coef.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),attributes(coef(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep=""))))$names[j]]<-coef(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))[j]
			}
		}
	}
	
	
# Nyblom Stability Test
st.egarch.nyblom.test.ret.fin<-c()
st.egarch.nyblom.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.fin)[i]<-paste("shape",sep="")
		}
	}
rownames(st.egarch.nyblom.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		st.egarch.nyblom.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),rownames(nyblom(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j]
			}
		}
	}

st.egarch.nyblom.test.ret.cg<-c()
st.egarch.nyblom.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.cg)[i]<-paste("shape",sep="")
		}
	}
rownames(st.egarch.nyblom.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		st.egarch.nyblom.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),rownames(nyblom(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j]
			}
		}
	}

st.egarch.nyblom.test.ret.ind<-c()
st.egarch.nyblom.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1)
		{
		colnames(st.egarch.nyblom.test.ret.ind)[i]<-paste("shape",sep="")
		}
	}
rownames(st.egarch.nyblom.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		st.egarch.nyblom.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),rownames(nyblom(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
#####################################################################################
# SIGN BIAS TEST


st.egarch.signbias.test.ret.fin<-c()
st.egarch.signbias.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.egarch.signbias.test.ret.fin)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(st.egarch.signbias.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(st.egarch.signbias.test.ret.fin)[j]]<-signbias(get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
st.egarch.signbias.test.ret.cg<-c()
st.egarch.signbias.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.egarch.signbias.test.ret.cg)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(st.egarch.signbias.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.signbias.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(st.egarch.signbias.test.ret.cg)[j]]<-signbias(get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))$prob[j]
		}
	}
	
st.egarch.signbias.test.ret.ind<-c()
st.egarch.signbias.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.egarch.signbias.test.ret.ind)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(st.egarch.signbias.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.signbias.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(st.egarch.signbias.test.ret.ind)[j]]<-signbias(get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))$prob[j]
		}
	}

	
# Goodness of Fit Test
st.egarch.gof.test.ret.fin<-c()
st.egarch.gof.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.egarch.gof.test.ret.fin)<-list("20", "30", "40", "50")
rownames(st.egarch.gof.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.gof.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(st.egarch.gof.test.ret.fin)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")),groups=as.numeric(paste(colnames(st.egarch.gof.test.ret.fin)[j])))[3]
		}
	}
	
st.egarch.gof.test.ret.cg<-c()
st.egarch.gof.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.egarch.gof.test.ret.cg)<-list("20", "30", "40", "50")
rownames(st.egarch.gof.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.gof.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(st.egarch.gof.test.ret.cg)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")),groups=as.numeric(paste(colnames(st.egarch.gof.test.ret.cg)[j])))[3]
		}
	}
	
st.egarch.gof.test.ret.ind<-c()
st.egarch.gof.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))


colnames(st.egarch.gof.test.ret.ind)<-list("20", "30", "40", "50")
rownames(st.egarch.gof.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.egarch.gof.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(st.egarch.gof.test.ret.ind)[j]]<-rugarch::gof(get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")),groups=as.numeric(paste(colnames(st.egarch.gof.test.ret.ind)[j])))[3]
		}
	}

 
#################################################################################	
# SST EGARCH
# specify ugarchspec
for(i in 1:15)
	{
	assign(paste("spec.sst.egarch.ret.fin.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "sstd", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.sst.egarch.ret.cg.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "sstd", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.sst.egarch.ret.ind.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "sstd", start.pars = list(), fixed.pars = list()))
	}
	
# fit univariate garch
for (i in 1:15)
	{
	assign(paste("sst.egarch.ret.fin.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.sst.egarch.ret.fin.",colnames(base.name[i]),sep="")), get(paste("ret.fin.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("sst.egarch.ret.cg.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.sst.egarch.ret.cg.",colnames(base.name[i]),sep="")), get(paste("ret.cg.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("sst.egarch.ret.ind.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.sst.egarch.ret.ind.",colnames(base.name[i]),sep="")), get(paste("ret.ind.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	}
	
#####################################################################################
#COEFFICIENT
sst.egarch.coef.ret.fin<-c()
sst.egarch.coef.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.egarch.coef.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(sst.egarch.coef.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(sst.egarch.coef.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(sst.egarch.coef.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(sst.egarch.coef.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(sst.egarch.coef.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(sst.egarch.coef.ret.fin)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(sst.egarch.coef.ret.fin)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.egarch.coef.ret.fin)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.egarch.coef.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep="")))[j])!=TRUE)
			{
		sst.egarch.coef.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),attributes(coef(get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep=""))))$names[j]]<-coef(get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep="")))[j]
			}
		}
	}

sst.egarch.coef.ret.cg<-c()
sst.egarch.coef.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.egarch.coef.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(sst.egarch.coef.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(sst.egarch.coef.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(sst.egarch.coef.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(sst.egarch.coef.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(sst.egarch.coef.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(sst.egarch.coef.ret.cg)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1)
		{
		colnames(sst.egarch.coef.ret.cg)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.egarch.coef.ret.cg)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.egarch.coef.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep="")))[j])!=TRUE)
			{
		sst.egarch.coef.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),attributes(coef(get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep=""))))$names[j]]<-coef(get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep="")))[j]
			}
		}
	}
	
sst.egarch.coef.ret.ind<-c()
sst.egarch.coef.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.egarch.coef.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(sst.egarch.coef.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(sst.egarch.coef.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(sst.egarch.coef.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(sst.egarch.coef.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(sst.egarch.coef.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(sst.egarch.coef.ret.ind)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1)
		{
		colnames(sst.egarch.coef.ret.ind)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.egarch.coef.ret.ind)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.egarch.coef.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep="")))[j])!=TRUE)
			{
		sst.egarch.coef.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),attributes(coef(get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep=""))))$names[j]]<-coef(get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep="")))[j]
			}
		}
	}

	
# Nyblom Stability Test
sst.egarch.nyblom.test.ret.fin<-c()
sst.egarch.nyblom.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.egarch.nyblom.test.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(sst.egarch.nyblom.test.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(sst.egarch.nyblom.test.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(sst.egarch.nyblom.test.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.fin)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.fin)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.fin)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.egarch.nyblom.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		sst.egarch.nyblom.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),rownames(nyblom(get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j]
			}
		}
	}

sst.egarch.nyblom.test.ret.cg<-c()
sst.egarch.nyblom.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.egarch.nyblom.test.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(sst.egarch.nyblom.test.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(sst.egarch.nyblom.test.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(sst.egarch.nyblom.test.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.cg)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.cg)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.cg)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.egarch.nyblom.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		sst.egarch.nyblom.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),rownames(nyblom(get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
sst.egarch.nyblom.test.ret.ind<-c()
sst.egarch.nyblom.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.egarch.nyblom.test.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(sst.egarch.nyblom.test.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(sst.egarch.nyblom.test.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(sst.egarch.nyblom.test.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.ind)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.ind)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.egarch.nyblom.test.ret.ind)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.egarch.nyblom.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		sst.egarch.nyblom.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),rownames(nyblom(get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
#####################################################################################
# SIGN BIAS TEST


sst.egarch.signbias.test.ret.fin<-c()
sst.egarch.signbias.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.egarch.signbias.test.ret.fin)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(sst.egarch.signbias.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.egarch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(sst.egarch.signbias.test.ret.fin)[j]]<-signbias(get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
sst.egarch.signbias.test.ret.cg<-c()
sst.egarch.signbias.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.egarch.signbias.test.ret.cg)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(sst.egarch.signbias.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.egarch.signbias.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(sst.egarch.signbias.test.ret.cg)[j]]<-signbias(get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep="")))$prob[j]
		}
	}
	
sst.egarch.signbias.test.ret.ind<-c()
sst.egarch.signbias.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.egarch.signbias.test.ret.ind)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(sst.egarch.signbias.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.egarch.signbias.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(sst.egarch.signbias.test.ret.ind)[j]]<-signbias(get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep="")))$prob[j]
		}
	}

for (i in 1:10)
	{
	for (j in 1:4)
		{
		sst.egarch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(sst.egarch.signbias.test.ret.fin)[j]]<-signbias(get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
	
# Goodness of Fit Test
sst.egarch.gof.test.ret.fin<-c()
sst.egarch.gof.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.egarch.gof.test.ret.fin)<-list("20", "30", "40", "50")
rownames(sst.egarch.gof.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.egarch.gof.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(sst.egarch.gof.test.ret.fin)[j]]<-rugarch::gof(get(paste("egarch.ret.",colnames(base.fin[i]),sep="")),groups=as.numeric(paste(colnames(sst.egarch.gof.test.ret.fin)[j])))[3]
		}
	}
	
sst.egarch.gof.test.ret.cg<-c()
sst.egarch.gof.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.egarch.gof.test.ret.cg)<-list("20", "30", "40", "50")
rownames(sst.egarch.gof.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.egarch.gof.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(sst.egarch.gof.test.ret.cg)[j]]<-rugarch::gof(get(paste("egarch.ret.",colnames(base.cg[i]),sep="")),groups=as.numeric(paste(colnames(sst.egarch.gof.test.ret.cg)[j])))[3]
		}
	}
	
sst.egarch.gof.test.ret.ind<-c()
sst.egarch.gof.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))


colnames(sst.egarch.gof.test.ret.ind)<-list("20", "30", "40", "50")
rownames(sst.egarch.gof.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.egarch.gof.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(sst.egarch.gof.test.ret.ind)[j]]<-rugarch::gof(get(paste("egarch.ret.",colnames(base.ind[i]),sep="")),groups=as.numeric(paste(colnames(sst.egarch.gof.test.ret.ind)[j])))[3]
		}
	}

#################################################################################	
# GJRGARCH
# specify ugarchspec
for(i in 1:15)
	{
	assign(paste("spec.gjr.garch.ret.fin.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "norm", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.gjr.garch.ret.cg.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "norm", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.gjr.garch.ret.ind.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "norm", start.pars = list(), fixed.pars = list()))
	}
	
# fit univariate garch
for (i in 1:15)
	{
	assign(paste("gjr.garch.ret.fin.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.gjr.garch.ret.fin.",colnames(base.name[i]),sep="")), get(paste("ret.fin.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("gjr.garch.ret.cg.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.gjr.garch.ret.cg.",colnames(base.name[i]),sep="")), get(paste("ret.cg.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("gjr.garch.ret.ind.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.gjr.garch.ret.ind.",colnames(base.name[i]),sep="")), get(paste("ret.ind.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	}
	

#######################################################################################COEFFICIENT
gjr.garch.coef.ret.fin<-c()
gjr.garch.coef.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(gjr.garch.coef.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(gjr.garch.coef.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(gjr.garch.coef.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(gjr.garch.coef.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(gjr.garch.coef.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(gjr.garch.coef.ret.fin)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(gjr.garch.coef.ret.fin)[i]<-paste("gamma1",sep="")
		}
	}
rownames(gjr.garch.coef.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
		{
		if (is.na(coef(get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j])!=TRUE)
			{
		gjr.garch.coef.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),attributes(coef(get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep=""))))$names[j]]<-coef(get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
			}
		}
	}

gjr.garch.coef.ret.cg<-c()
gjr.garch.coef.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(gjr.garch.coef.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(gjr.garch.coef.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(gjr.garch.coef.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(gjr.garch.coef.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(gjr.garch.coef.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(gjr.garch.coef.ret.cg)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(gjr.garch.coef.ret.cg)[i]<-paste("gamma1",sep="")
		}
	}
rownames(gjr.garch.coef.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
		{
		if (is.na(coef(get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j])!=TRUE)
			{
		gjr.garch.coef.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),attributes(coef(get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep=""))))$names[j]]<-coef(get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
			}
		}
	}

gjr.garch.coef.ret.ind<-c()
gjr.garch.coef.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(gjr.garch.coef.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(gjr.garch.coef.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(gjr.garch.coef.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(gjr.garch.coef.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(gjr.garch.coef.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(gjr.garch.coef.ret.ind)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(gjr.garch.coef.ret.ind)[i]<-paste("gamma1",sep="")
		}
	}
rownames(gjr.garch.coef.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
		{
		if (is.na(coef(get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j])!=TRUE)
			{
		gjr.garch.coef.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),attributes(coef(get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep=""))))$names[j]]<-coef(get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
			}
		}
	}
	

#####################################################################################
# Nyblom Stability Test

gjr.garch.nyblom.test.ret.fin<-c()
gjr.garch.nyblom.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(gjr.garch.nyblom.test.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(gjr.garch.nyblom.test.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(gjr.garch.nyblom.test.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(gjr.garch.nyblom.test.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(gjr.garch.nyblom.test.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(gjr.garch.nyblom.test.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(gjr.garch.nyblom.test.ret.fin)[i]<-paste("gamma1",sep="")
		}	
	}
rownames(gjr.garch.nyblom.test.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		gjr.garch.nyblom.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),rownames(nyblom(get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j]
			}
		}
	}

gjr.garch.nyblom.test.ret.cg<-c()
gjr.garch.nyblom.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(gjr.garch.nyblom.test.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(gjr.garch.nyblom.test.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(gjr.garch.nyblom.test.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(gjr.garch.nyblom.test.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(gjr.garch.nyblom.test.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(gjr.garch.nyblom.test.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(gjr.garch.nyblom.test.ret.cg)[i]<-paste("gamma1",sep="")
		}	
	}
rownames(gjr.garch.nyblom.test.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		gjr.garch.nyblom.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),rownames(nyblom(get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
gjr.garch.nyblom.test.ret.ind<-c()
gjr.garch.nyblom.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(gjr.garch.nyblom.test.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(gjr.garch.nyblom.test.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(gjr.garch.nyblom.test.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(gjr.garch.nyblom.test.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(gjr.garch.nyblom.test.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(gjr.garch.nyblom.test.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(gjr.garch.nyblom.test.ret.ind)[i]<-paste("gamma1",sep="")
		}	
	}
rownames(gjr.garch.nyblom.test.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		gjr.garch.nyblom.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),rownames(nyblom(get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	

#####################################################################################
# SIGN BIAS TEST

gjr.garch.signbias.test.ret.fin<-c()
gjr.garch.signbias.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(gjr.garch.signbias.test.ret.fin)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(gjr.garch.signbias.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		gjr.garch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(gjr.garch.signbias.test.ret.fin)[j]]<-signbias(get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
gjr.garch.signbias.test.ret.cg<-c()
gjr.garch.signbias.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(gjr.garch.signbias.test.ret.cg)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(gjr.garch.signbias.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		gjr.garch.signbias.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(gjr.garch.signbias.test.ret.cg)[j]]<-signbias(get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")))$prob[j]
		}
	}
	
gjr.garch.signbias.test.ret.ind<-c()
gjr.garch.signbias.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(gjr.garch.signbias.test.ret.ind)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(gjr.garch.signbias.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		gjr.garch.signbias.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(gjr.garch.signbias.test.ret.ind)[j]]<-signbias(get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")))$prob[j]
		}
	}

for (i in 1:10)
	{
	for (j in 1:4)
		{
		gjr.garch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(gjr.garch.signbias.test.ret.fin)[j]]<-signbias(get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
	
# Goodness of Fit Test
gjr.garch.gof.test.ret.fin<-c()
gjr.garch.gof.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(gjr.garch.gof.test.ret.fin)<-list("20", "30", "40", "50")
rownames(gjr.garch.gof.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		gjr.garch.gof.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(gjr.garch.gof.test.ret.fin)[j]]<-rugarch::gof(get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")),groups=as.numeric(paste(colnames(gjr.garch.gof.test.ret.fin)[j])))[3]
		}
	}
	
gjr.garch.gof.test.ret.cg<-c()
gjr.garch.gof.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(gjr.garch.gof.test.ret.cg)<-list("20", "30", "40", "50")
rownames(gjr.garch.gof.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		gjr.garch.gof.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(gjr.garch.gof.test.ret.cg)[j]]<-rugarch::gof(get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")),groups=as.numeric(paste(colnames(gjr.garch.gof.test.ret.cg)[j])))[3]
		}
	}
	
gjr.garch.gof.test.ret.ind<-c()
gjr.garch.gof.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))


colnames(gjr.garch.gof.test.ret.ind)<-list("20", "30", "40", "50")
rownames(gjr.garch.gof.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		gjr.garch.gof.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(gjr.garch.gof.test.ret.ind)[j]]<-rugarch::gof(get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")),groups=as.numeric(paste(colnames(gjr.garch.gof.test.ret.ind)[j])))[3]
		}
	}

#################################################################################	
# ST.GJRGARCH
# specify ugarchspec
for(i in 1:15)
	{
	assign(paste("spec.st.gjr.garch.ret.fin.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "std", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.st.gjr.garch.ret.cg.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "std", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.st.gjr.garch.ret.ind.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "std", start.pars = list(), fixed.pars = list()))
	}
	
# fit univariate garch
for (i in 1:15)
	{
	assign(paste("st.gjr.garch.ret.fin.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.st.gjr.garch.ret.fin.",colnames(base.name[i]),sep="")), get(paste("ret.fin.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("st.gjr.garch.ret.cg.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.st.gjr.garch.ret.cg.",colnames(base.name[i]),sep="")), get(paste("ret.cg.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("st.gjr.garch.ret.ind.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.st.gjr.garch.ret.ind.",colnames(base.name[i]),sep="")), get(paste("ret.ind.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	}
	
	
#######################################################################################COEFFICIENT
st.gjr.garch.coef.ret.fin<-c()
st.gjr.garch.coef.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.gjr.garch.coef.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(st.gjr.garch.coef.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(st.gjr.garch.coef.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(st.gjr.garch.coef.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(st.gjr.garch.coef.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(st.gjr.garch.coef.ret.fin)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(st.gjr.garch.coef.ret.fin)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(st.gjr.garch.coef.ret.fin)[i]<-paste("shape",sep="")
		}
	}
rownames(st.gjr.garch.coef.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j])!=TRUE)
			{
		st.gjr.garch.coef.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),attributes(coef(get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep=""))))$names[j]]<-coef(get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
			}
		}
	}

st.gjr.garch.coef.ret.cg<-c()
st.gjr.garch.coef.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.gjr.garch.coef.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(st.gjr.garch.coef.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(st.gjr.garch.coef.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(st.gjr.garch.coef.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(st.gjr.garch.coef.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(st.gjr.garch.coef.ret.cg)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(st.gjr.garch.coef.ret.cg)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1)
		{
		colnames(st.gjr.garch.coef.ret.cg)[i]<-paste("shape",sep="")
		}
	}
rownames(st.gjr.garch.coef.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j])!=TRUE)
			{
		st.gjr.garch.coef.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),attributes(coef(get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep=""))))$names[j]]<-coef(get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
			}
		}
	}
	
st.gjr.garch.coef.ret.ind<-c()
st.gjr.garch.coef.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.gjr.garch.coef.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(st.gjr.garch.coef.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(st.gjr.garch.coef.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(st.gjr.garch.coef.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(st.gjr.garch.coef.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(st.gjr.garch.coef.ret.ind)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(st.gjr.garch.coef.ret.ind)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1)
		{
		colnames(st.gjr.garch.coef.ret.ind)[i]<-paste("shape",sep="")
		}
	}
rownames(st.gjr.garch.coef.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j])!=TRUE)
			{
		st.gjr.garch.coef.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),attributes(coef(get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep=""))))$names[j]]<-coef(get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
			}
		}
	}
	

#####################################################################################
# Nyblom Stability Test

st.gjr.garch.nyblom.test.ret.fin<-c()
st.gjr.garch.nyblom.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(st.gjr.garch.nyblom.test.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(st.gjr.garch.nyblom.test.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.fin)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.fin)[i]<-paste("shape",sep="")
		}
	}
rownames(st.gjr.garch.nyblom.test.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		st.gjr.garch.nyblom.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),rownames(nyblom(get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
st.gjr.garch.nyblom.test.ret.cg<-c()
st.gjr.garch.nyblom.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(st.gjr.garch.nyblom.test.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(st.gjr.garch.nyblom.test.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.cg)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.cg)[i]<-paste("shape",sep="")
		}
	}
rownames(st.gjr.garch.nyblom.test.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		st.gjr.garch.nyblom.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),rownames(nyblom(get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	
st.gjr.garch.nyblom.test.ret.ind<-c()
st.gjr.garch.nyblom.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(st.gjr.garch.nyblom.test.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(st.gjr.garch.nyblom.test.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.ind)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1)
		{
		colnames(st.gjr.garch.nyblom.test.ret.ind)[i]<-paste("shape",sep="")
		}
	}
rownames(st.gjr.garch.nyblom.test.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		st.gjr.garch.nyblom.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),rownames(nyblom(get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j]
			}
		}
	}
	

#####################################################################################
# SIGN BIAS TEST

st.gjr.garch.signbias.test.ret.fin<-c()
st.gjr.garch.signbias.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.gjr.garch.signbias.test.ret.fin)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(st.gjr.garch.signbias.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.gjr.garch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(st.gjr.garch.signbias.test.ret.fin)[j]]<-signbias(get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
st.gjr.garch.signbias.test.ret.cg<-c()
st.gjr.garch.signbias.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.gjr.garch.signbias.test.ret.cg)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(st.gjr.garch.signbias.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.gjr.garch.signbias.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(st.gjr.garch.signbias.test.ret.cg)[j]]<-signbias(get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")))$prob[j]
		}
	}
	
st.gjr.garch.signbias.test.ret.ind<-c()
st.gjr.garch.signbias.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.gjr.garch.signbias.test.ret.ind)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(st.gjr.garch.signbias.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.gjr.garch.signbias.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(st.gjr.garch.signbias.test.ret.ind)[j]]<-signbias(get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")))$prob[j]
		}
	}



# Goodness of Fit Test
st.gjr.garch.gof.test.ret.fin<-c()
st.gjr.garch.gof.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.gjr.garch.gof.test.ret.fin)<-list("20", "30", "40", "50")
rownames(st.gjr.garch.gof.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.gjr.garch.gof.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(st.gjr.garch.gof.test.ret.fin)[j]]<-rugarch::gof(get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")),groups=as.numeric(paste(colnames(st.gjr.garch.gof.test.ret.fin)[j])))[3]
		}
	}
	
st.gjr.garch.gof.test.ret.cg<-c()
st.gjr.garch.gof.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(st.gjr.garch.gof.test.ret.cg)<-list("20", "30", "40", "50")
rownames(st.gjr.garch.gof.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.gjr.garch.gof.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(st.gjr.garch.gof.test.ret.cg)[j]]<-rugarch::gof(get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")),groups=as.numeric(paste(colnames(st.gjr.garch.gof.test.ret.cg)[j])))[3]
		}
	}
	
st.gjr.garch.gof.test.ret.ind<-c()
st.gjr.garch.gof.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))


colnames(st.gjr.garch.gof.test.ret.ind)<-list("20", "30", "40", "50")
rownames(st.gjr.garch.gof.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		st.gjr.garch.gof.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(st.gjr.garch.gof.test.ret.ind)[j]]<-rugarch::gof(get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")),groups=as.numeric(paste(colnames(st.gjr.garch.gof.test.ret.ind)[j])))[3]
		}
	}

#################################################################################	
# SST.GJRGARCH
# specify ugarchspec
for(i in 1:15)
	{
	assign(paste("spec.sst.gjr.garch.ret.fin.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.fin.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "sstd", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.sst.gjr.garch.ret.cg.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.cg.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "sstd", start.pars = list(), fixed.pars = list()))
	
	assign(paste("spec.sst.gjr.garch.ret.ind.",colnames(base.name[i]),sep=""),
	ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), 
	submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
	mean.model = list(armaOrder = c(get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[1], get(paste("arima.ret.ind.",colnames(base.name[i]),sep=""))$arma[2]), include.mean = TRUE, archm = FALSE, 
	archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
	distribution.model = "sstd", start.pars = list(), fixed.pars = list()))
	}
	
# fit univariate garch
for (i in 1:15)
	{
	assign(paste("sst.gjr.garch.ret.fin.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.sst.gjr.garch.ret.fin.",colnames(base.name[i]),sep="")), get(paste("ret.fin.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("sst.gjr.garch.ret.cg.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.sst.gjr.garch.ret.cg.",colnames(base.name[i]),sep="")), get(paste("ret.cg.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	
	assign(paste("sst.gjr.garch.ret.ind.",colnames(base.name[i]),sep=""), ugarchfit(get(paste("spec.sst.gjr.garch.ret.ind.",colnames(base.name[i]),sep="")), get(paste("ret.ind.",colnames(base.name[i]),sep="")), out.sample = 30, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')))
	}
	

	
#####################################################################################
#COEFFICIENT
sst.gjr.garch.coef.ret.fin<-c()
sst.gjr.garch.coef.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.gjr.garch.coef.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(sst.gjr.garch.coef.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(sst.gjr.garch.coef.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(sst.gjr.garch.coef.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.fin)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.fin)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.fin)[i]<-paste("skew",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.fin)[i]<-paste("shape",sep="")
		}
	}
rownames(sst.gjr.garch.coef.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j])!=TRUE)
			{
		sst.gjr.garch.coef.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),attributes(coef(get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep=""))))$names[j]]<-coef(get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
			}
		}
	}

#COEFFICIENT
sst.gjr.garch.coef.ret.cg<-c()
sst.gjr.garch.coef.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.gjr.garch.coef.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(sst.gjr.garch.coef.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(sst.gjr.garch.coef.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(sst.gjr.garch.coef.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.cg)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.cg)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.cg)[i]<-paste("skew",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.cg)[i]<-paste("shape",sep="")
		}
	}
rownames(sst.gjr.garch.coef.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j])!=TRUE)
			{
		sst.gjr.garch.coef.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),attributes(coef(get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep=""))))$names[j]]<-coef(get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
			}
		}
	}

#COEFFICIENT
sst.gjr.garch.coef.ret.ind<-c()
sst.gjr.garch.coef.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.gjr.garch.coef.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(sst.gjr.garch.coef.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(sst.gjr.garch.coef.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(sst.gjr.garch.coef.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.ind)[i]<-paste("beta1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.ind)[i]<-paste("gamma1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.ind)[i]<-paste("skew",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.coef.ret.ind)[i]<-paste("shape",sep="")
		}
	}
rownames(sst.gjr.garch.coef.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1))
		{
		if (is.na(coef(get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j])!=TRUE)
			{
		sst.gjr.garch.coef.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),attributes(coef(get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep=""))))$names[j]]<-coef(get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
			}
		}
	}

#####################################################################################
# Nyblom Stability Test
sst.gjr.garch.nyblom.test.ret.fin<-c()
sst.gjr.garch.nyblom.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(sst.gjr.garch.nyblom.test.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(sst.gjr.garch.nyblom.test.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.fin)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.fin)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.fin)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.gjr.garch.nyblom.test.ret.fin)<-colnames(ret.fin)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		sst.gjr.garch.nyblom.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),rownames(nyblom(get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j]
			}
		}
	}

sst.gjr.garch.nyblom.test.ret.cg<-c()
sst.gjr.garch.nyblom.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(sst.gjr.garch.nyblom.test.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(sst.gjr.garch.nyblom.test.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.cg)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.cg)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.cg)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.gjr.garch.nyblom.test.ret.cg)<-colnames(ret.cg)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		sst.gjr.garch.nyblom.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),rownames(nyblom(get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j]
			}
		}
	}

sst.gjr.garch.nyblom.test.ret.ind<-c()
sst.gjr.garch.nyblom.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(sst.gjr.garch.nyblom.test.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(sst.gjr.garch.nyblom.test.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.ind)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.ind)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1)
		{
		colnames(sst.gjr.garch.nyblom.test.ret.ind)[i]<-paste("shape",sep="")
		}	
	}
rownames(sst.gjr.garch.nyblom.test.ret.ind)<-colnames(ret.ind)

for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1))
		{
		if (is.na(nyblom(get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j])!=TRUE)
			{
		sst.gjr.garch.nyblom.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),rownames(nyblom(get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j]
			}
		}
	}

#####################################################################################
# SIGN BIAS TEST

sst.gjr.garch.signbias.test.ret.fin<-c()
sst.gjr.garch.signbias.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.gjr.garch.signbias.test.ret.fin)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(sst.gjr.garch.signbias.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.gjr.garch.signbias.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(sst.gjr.garch.signbias.test.ret.fin)[j]]<-signbias(get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")))$prob[j]
		}
	}
	
sst.gjr.garch.signbias.test.ret.cg<-c()
sst.gjr.garch.signbias.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.gjr.garch.signbias.test.ret.cg)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(sst.gjr.garch.signbias.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.gjr.garch.signbias.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(sst.gjr.garch.signbias.test.ret.cg)[j]]<-signbias(get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")))$prob[j]
		}
	}
	
sst.gjr.garch.signbias.test.ret.ind<-c()
sst.gjr.garch.signbias.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.gjr.garch.signbias.test.ret.ind)<-list("Sign Bias", "Negative Sign Bias", "Positive Sign Bias", "Joint Effect")
rownames(sst.gjr.garch.signbias.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.gjr.garch.signbias.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(sst.gjr.garch.signbias.test.ret.ind)[j]]<-signbias(get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")))$prob[j]
		}
	}

	
# Goodness of Fit Test
sst.gjr.garch.gof.test.ret.fin<-c()
sst.gjr.garch.gof.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.gjr.garch.gof.test.ret.fin)<-list("20", "30", "40", "50")
rownames(sst.gjr.garch.gof.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.gjr.garch.gof.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),colnames(sst.gjr.garch.gof.test.ret.fin)[j]]<-rugarch::gof(get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")),groups=as.numeric(paste(colnames(sst.gjr.garch.gof.test.ret.fin)[j])))[3]
		}
	}
	
sst.gjr.garch.gof.test.ret.cg<-c()
sst.gjr.garch.gof.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 4))

colnames(sst.gjr.garch.gof.test.ret.cg)<-list("20", "30", "40", "50")
rownames(sst.gjr.garch.gof.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.gjr.garch.gof.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),colnames(sst.gjr.garch.gof.test.ret.cg)[j]]<-rugarch::gof(get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")),groups=as.numeric(paste(colnames(sst.gjr.garch.gof.test.ret.cg)[j])))[3]
		}
	}
	
sst.gjr.garch.gof.test.ret.ind<-c()
sst.gjr.garch.gof.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 4))


colnames(sst.gjr.garch.gof.test.ret.ind)<-list("20", "30", "40", "50")
rownames(sst.gjr.garch.gof.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:4)
		{
		sst.gjr.garch.gof.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),colnames(sst.gjr.garch.gof.test.ret.ind)[j]]<-rugarch::gof(get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")),groups=as.numeric(paste(colnames(sst.gjr.garch.gof.test.ret.ind)[j])))[3]
		}
	}

# INFOCRIT
#Model Selection using Information Criteria
num.infocrit.ret.fin<-data.frame(matrix(NA, nrow=15, ncol=5 ))
num.infocrit.ret.cg<-data.frame(matrix(NA, nrow=15, ncol=5 ))
num.infocrit.ret.ind<-data.frame(matrix(NA, nrow=15, ncol=5 ))

colnames(num.infocrit.ret.fin)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")
colnames(num.infocrit.ret.cg)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")
colnames(num.infocrit.ret.ind)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")

for (i in 1:15)
	{
	rownames(num.infocrit.ret.fin)[i]<-paste("ret.",colnames(base.fin[i]),sep="")
	rownames(num.infocrit.ret.cg)[i]<-paste("ret.",colnames(base.cg[i]),sep="")
	rownames(num.infocrit.ret.ind)[i]<-paste("ret.",colnames(base.ind[i]),sep="")
	}
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
		num.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")[j]]<-min(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("egarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
))
	if (j==5)
	{
	num.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min","Model")[j]]<-min(num.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),1:4])
	}
		}
	}
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
		num.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")[j]]<-min(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("egarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
))
	if (j==5)
	{
	num.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min","Model")[j]]<-min(num.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),1:4])
	}
		
	}
	}	
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
		num.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")[j]]<-min(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("egarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
))
	if (j==5)
	{
	num.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min","Model")[j]]<-min(num.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),1:4])
	}
		
	}
	}

####################################################################################
infocrit.ret.fin<-data.frame(matrix(NA, nrow=15, ncol=5 ))
infocrit.ret.cg<-data.frame(matrix(NA, nrow=15, ncol=5 ))
infocrit.ret.ind<-data.frame(matrix(NA, nrow=15, ncol=5 ))

colnames(infocrit.ret.fin)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")
colnames(infocrit.ret.cg)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")
colnames(infocrit.ret.ind)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")

for (i in 1:15)
	{
	rownames(infocrit.ret.fin)[i]<-paste("ret.",colnames(base.fin[i]),sep="")
	rownames(infocrit.ret.cg)[i]<-paste("ret.",colnames(base.cg[i]),sep="")
	rownames(infocrit.ret.ind)[i]<-paste("ret.",colnames(base.ind[i]),sep="")
	}
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
			if (j<=4)
	{
		infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","egarch","gjr.garch")[which(c(infocriteria(get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("egarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
)==min(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("egarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
)), arr.ind=TRUE)]}
	
	if (j==5)
	{
	infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","ugarch","ugarch","ugarch","egarch","egarch","egarch","egarch","gjr.garch","gjr.garch","gjr.garch","gjr.garch")[which(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("egarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))
)==min(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("egarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("gjr.garch.ret.",colnames(base.fin[i]),sep="")))
)), arr.ind=TRUE)]
	}	}
	}

for (i in 1:15)
	{
	for (j in 1:5)
		{
			if (j<=4)
	{
		infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","egarch","gjr.garch")[which(c(infocriteria(get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("egarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
)==min(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("egarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
)), arr.ind=TRUE)]}
	
	if (j==5)
	{
	infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","ugarch","ugarch","ugarch","egarch","egarch","egarch","egarch","gjr.garch","gjr.garch","gjr.garch","gjr.garch")[which(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("egarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")))
)==min(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("egarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("gjr.garch.ret.",colnames(base.cg[i]),sep="")))
)), arr.ind=TRUE)]
	}	}
	}


	
for (i in 1:15)
	{
	for (j in 1:5)
		{
			if (j<=4)
	{
		infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","egarch","gjr.garch")[which(c(infocriteria(get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("egarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
)==min(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("egarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
)), arr.ind=TRUE)]}
	
	if (j==5)
	{
	infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","ugarch","ugarch","ugarch","egarch","egarch","egarch","egarch","gjr.garch","gjr.garch","gjr.garch","gjr.garch")[which(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("egarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")))
)==min(c(
infocriteria (get(paste("ugarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("egarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("gjr.garch.ret.",colnames(base.ind[i]),sep="")))
)), arr.ind=TRUE)]
	}	}
	}
	
# INFOCRIT.ST

num.st.infocrit.ret.fin<-data.frame(matrix(NA, nrow=15, ncol=5 ))
num.st.infocrit.ret.cg<-data.frame(matrix(NA, nrow=15, ncol=5 ))
num.st.infocrit.ret.ind<-data.frame(matrix(NA, nrow=15, ncol=5 ))

colnames(num.st.infocrit.ret.fin)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")
colnames(num.st.infocrit.ret.cg)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")
colnames(num.st.infocrit.ret.ind)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")

for (i in 1:15)
	{
	rownames(num.st.infocrit.ret.fin)[i]<-paste("ret.",colnames(base.fin[i]),sep="")
	rownames(num.st.infocrit.ret.cg)[i]<-paste("ret.",colnames(base.cg[i]),sep="")
	rownames(num.st.infocrit.ret.ind)[i]<-paste("ret.",colnames(base.ind[i]),sep="")
	}
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
		num.st.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")[j]]<-min(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
))
	if (j==5)
	{
	num.st.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min","Model")[j]]<-min(num.st.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),1:4])
	}
		}
	}
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
		num.st.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")[j]]<-min(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
))
	if (j==5)
	{
	num.st.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min","Model")[j]]<-min(num.st.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),1:4])
	}
		
	}
	}	
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
		num.st.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")[j]]<-min(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
))
	if (j==5)
	{
	num.st.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min","Model")[j]]<-min(num.st.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),1:4])
	}
		
	}
	}
	

####################################################################################
st.infocrit.ret.fin<-data.frame(matrix(NA, nrow=15, ncol=5 ))
st.infocrit.ret.cg<-data.frame(matrix(NA, nrow=15, ncol=5 ))
st.infocrit.ret.ind<-data.frame(matrix(NA, nrow=15, ncol=5 ))

colnames(st.infocrit.ret.fin)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")
colnames(st.infocrit.ret.cg)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")
colnames(st.infocrit.ret.ind)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")

for (i in 1:15)
	{
	rownames(st.infocrit.ret.fin)[i]<-paste("ret.",colnames(base.fin[i]),sep="")
	rownames(st.infocrit.ret.cg)[i]<-paste("ret.",colnames(base.cg[i]),sep="")
	rownames(st.infocrit.ret.ind)[i]<-paste("ret.",colnames(base.ind[i]),sep="")
	}
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
			if (j<=4)
	{
		st.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","egarch","gjr.garch")[which(c(infocriteria(get(paste("st.ugarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
)==min(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("st.egarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
)), arr.ind=TRUE)]}
	
	if (j==5)
	{
	st.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","ugarch","ugarch","ugarch","egarch","egarch","egarch","egarch","gjr.garch","gjr.garch","gjr.garch","gjr.garch")[which(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("st.egarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")))
)==min(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("st.egarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.fin[i]),sep="")))
)), arr.ind=TRUE)]
	}	}
	}

for (i in 1:15)
	{
	for (j in 1:5)
		{
			if (j<=4)
	{
		st.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","egarch","gjr.garch")[which(c(infocriteria(get(paste("st.ugarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
)==min(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("st.egarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
)), arr.ind=TRUE)]}
	
	if (j==5)
	{
	st.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","ugarch","ugarch","ugarch","egarch","egarch","egarch","egarch","gjr.garch","gjr.garch","gjr.garch","gjr.garch")[which(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("st.egarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")))
)==min(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("st.egarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.cg[i]),sep="")))
)), arr.ind=TRUE)]
	}	}
	}


	
for (i in 1:15)
	{
	for (j in 1:5)
		{
			if (j<=4)
	{
		st.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","egarch","gjr.garch")[which(c(infocriteria(get(paste("st.ugarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
)==min(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("st.egarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
)), arr.ind=TRUE)]}
	
	if (j==5)
	{
	st.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","ugarch","ugarch","ugarch","egarch","egarch","egarch","egarch","gjr.garch","gjr.garch","gjr.garch","gjr.garch")[which(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("st.egarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")))
)==min(c(
infocriteria (get(paste("st.ugarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("st.egarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("st.gjr.garch.ret.",colnames(base.ind[i]),sep="")))
)), arr.ind=TRUE)]
	}	}
	}
	
# INFOCRIT.SST

num.sst.infocrit.ret.fin<-data.frame(matrix(NA, nrow=15, ncol=5 ))
num.sst.infocrit.ret.cg<-data.frame(matrix(NA, nrow=15, ncol=5 ))
num.sst.infocrit.ret.ind<-data.frame(matrix(NA, nrow=15, ncol=5 ))

colnames(num.sst.infocrit.ret.fin)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")
colnames(num.sst.infocrit.ret.cg)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")
colnames(num.sst.infocrit.ret.ind)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")

for (i in 1:15)
	{
	rownames(num.sst.infocrit.ret.fin)[i]<-paste("ret.",colnames(base.fin[i]),sep="")
	rownames(num.sst.infocrit.ret.cg)[i]<-paste("ret.",colnames(base.cg[i]),sep="")
	rownames(num.sst.infocrit.ret.ind)[i]<-paste("ret.",colnames(base.ind[i]),sep="")
	}
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
		num.sst.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")[j]]<-min(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
))
	if (j==5)
	{
	num.sst.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min","Model")[j]]<-min(num.sst.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),1:4])
	}
		}
	}
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
		num.sst.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")[j]]<-min(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
))
	if (j==5)
	{
	num.sst.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min","Model")[j]]<-min(num.sst.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),1:4])
	}
		
	}
	}	
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
		num.sst.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min")[j]]<-min(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
))
	if (j==5)
	{
	num.sst.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Min","Model")[j]]<-min(num.sst.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),1:4])
	}
		
	}
	}
	

####################################################################################
sst.infocrit.ret.fin<-data.frame(matrix(NA, nrow=15, ncol=5 ))
sst.infocrit.ret.cg<-data.frame(matrix(NA, nrow=15, ncol=5 ))
sst.infocrit.ret.ind<-data.frame(matrix(NA, nrow=15, ncol=5 ))

colnames(sst.infocrit.ret.fin)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")
colnames(sst.infocrit.ret.cg)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")
colnames(sst.infocrit.ret.ind)<-c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")

for (i in 1:15)
	{
	rownames(sst.infocrit.ret.fin)[i]<-paste("ret.",colnames(base.fin[i]),sep="")
	rownames(sst.infocrit.ret.cg)[i]<-paste("ret.",colnames(base.cg[i]),sep="")
	rownames(sst.infocrit.ret.ind)[i]<-paste("ret.",colnames(base.ind[i]),sep="")
	}
	
for (i in 1:15)
	{
	for (j in 1:5)
		{
			if (j<=4)
	{
		sst.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","egarch","gjr.garch")[which(c(infocriteria(get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
)==min(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep="")))[j],
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")))[j]
)), arr.ind=TRUE)]}
	
	if (j==5)
	{
	sst.infocrit.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","ugarch","ugarch","ugarch","egarch","egarch","egarch","egarch","gjr.garch","gjr.garch","gjr.garch","gjr.garch")[which(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")))
)==min(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("sst.egarch.ret.",colnames(base.fin[i]),sep=""))),
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.fin[i]),sep="")))
)), arr.ind=TRUE)]
	}	}
	}

for (i in 1:15)
	{
	for (j in 1:5)
		{
			if (j<=4)
	{
		sst.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","egarch","gjr.garch")[which(c(infocriteria(get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
)==min(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep="")))[j],
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")))[j]
)), arr.ind=TRUE)]}
	
	if (j==5)
	{
	sst.infocrit.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","ugarch","ugarch","ugarch","egarch","egarch","egarch","egarch","gjr.garch","gjr.garch","gjr.garch","gjr.garch")[which(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")))
)==min(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("sst.egarch.ret.",colnames(base.cg[i]),sep=""))),
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.cg[i]),sep="")))
)), arr.ind=TRUE)]
	}	}
	}


	
for (i in 1:15)
	{
	for (j in 1:5)
		{
			if (j<=4)
	{
		sst.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","egarch","gjr.garch")[which(c(infocriteria(get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
)==min(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep="")))[j],
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")))[j]
)), arr.ind=TRUE)]}
	
	if (j==5)
	{
	sst.infocrit.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),c("Akaike","Bayes","Shibata","Hannan-Quinn","Model")[j]]<-c("ugarch","ugarch","ugarch","ugarch","egarch","egarch","egarch","egarch","gjr.garch","gjr.garch","gjr.garch","gjr.garch")[which(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")))
)==min(c(
infocriteria (get(paste("sst.ugarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("sst.egarch.ret.",colnames(base.ind[i]),sep=""))),
infocriteria (get(paste("sst.gjr.garch.ret.",colnames(base.ind[i]),sep="")))
)), arr.ind=TRUE)]
	}	}
	}
	
###################################################################################### ARCH/GARCH TEST
#testing for arch effect
for (i in 1:15)
	{
	assign(paste("arch.lm.ret.fin.",colnames(base.name[i]),sep=""), ArchTest(get(paste("resid.ret.fin.",colnames(base.name[i]),sep="")), lags=1))
	assign(paste("arch.lm.ret.cg.",colnames(base.name[i]),sep=""), ArchTest(get(paste("resid.ret.cg.",colnames(base.name[i]),sep="")), lags=1))
	assign(paste("arch.lm.ret.ind.",colnames(base.name[i]),sep=""), ArchTest(get(paste("resid.ret.ind.",colnames(base.name[i]),sep="")), lags=1))
	}

for (i in 1:10)
	{
	assign(paste("arch.lm.ret.",colnames(base.exch[i]),sep=""), ArchTest(get(paste("resid.ret.",colnames(base.exch[i]),sep="")), lags=1))
	}
	

num.arch.lm.ret.fin<-c()
num.arch.lm.ret.cg<-c()
num.arch.lm.ret.ind<-c()
num.arch.lm.ret.fin<-data.frame(num.arch.lm.ret.fin)
num.arch.lm.ret.cg<-data.frame(num.arch.lm.ret.cg)
num.arch.lm.ret.ind<-data.frame(num.arch.lm.ret.ind)
for (i in 1:15)
	{
	num.arch.lm.ret.fin[paste("ret.fin.",colnames(base.name[i]),sep=""),"pval"]<-get(paste("arch.lm.ret.fin.",colnames(base.name[i]),sep=""))$p.value[1]
	num.arch.lm.ret.cg[paste("ret.cg.",colnames(base.name[i]),sep=""),"pval"]<-get(paste("arch.lm.ret.cg.",colnames(base.name[i]),sep=""))$p.value[1]
	num.arch.lm.ret.ind[paste("ret.ind.",colnames(base.name[i]),sep=""),"pval"]<-get(paste("arch.lm.ret.ind.",colnames(base.name[i]),sep=""))$p.value[1]
	}

arch.lm.ret.fin<-num.arch.lm.ret.fin>0.1
arch.lm.ret.cg<-num.arch.lm.ret.cg>0.1
arch.lm.ret.ind<-num.arch.lm.ret.ind>0.1

num.arch.lm.ret.exch<-c()
num.arch.lm.ret.exch<-data.frame(num.arch.lm.ret.exch)
for (i in 1:10)
	{
	num.arch.lm.ret.exch[paste("ret.",colnames(base.exch[i]),sep=""),"pval"]<-get(paste("arch.lm.ret.",colnames(base.exch[i]),sep=""))$p.value[1]
	}
################################################################################################################
# When failed to converge, clean the data
for (i in 1:15)
	{
	assign(paste("clean.ret.",colnames(base.fin)[i],sep=""),Return.clean(get(paste("ret.",colnames(base.fin)[i],sep="")),method="boudt"))
	assign(paste("clean.ret.",colnames(base.cg)[i],sep=""),Return.clean(get(paste("ret.",colnames(base.cg)[i],sep="")),method="boudt"))
	assign(paste("clean.ret.",colnames(base.ind)[i],sep=""),Return.clean(get(paste("ret.",colnames(base.ind)[i],sep="")),method="boudt"))	
	}

for (i in 1:10)
	{
	assign(paste("clean.ret.",colnames(base.exch)[i],sep=""),Return.clean(get(paste("ret.",colnames(base.exch)[i],sep="")),method="boudt"))
	}
	
############################################################################################
#INFOCRIT COMPILATION

comp.infocrit.ret.fin<-data.frame(matrix(NA, nrow=15, ncol=3))
colnames(comp.infocrit.ret.fin)<-c("Distribution","Model","Infocrit")
for (i in 1:15)
	{
	rownames(comp.infocrit.ret.fin)[i]<-paste("ret.",colnames(base.fin[i]),sep="")
	}

for (i in 1:15)
	{
	comp.infocrit.ret.fin[i,"Distribution"]<-c("norm","st","sst")[which(c(num.infocrit.ret.fin[i,5],num.st.infocrit.ret.fin[i,5],num.sst.infocrit.ret.fin[i,5])==min(num.infocrit.ret.fin[i,5],num.st.infocrit.ret.fin[i,5],num.sst.infocrit.ret.fin[i,5]))]
	comp.infocrit.ret.fin[i,"Model"]<-get(paste(c("infocrit.ret.fin","st.infocrit.ret.fin","sst.infocrit.ret.fin")[which(c(num.infocrit.ret.fin[i,5],num.st.infocrit.ret.fin[i,5],num.sst.infocrit.ret.fin[i,5])==min(num.infocrit.ret.fin[i,5],num.st.infocrit.ret.fin[i,5],num.sst.infocrit.ret.fin[i,5]))]))[i,5]
	comp.infocrit.ret.fin[i,"Infocrit"]<-get(paste(c("num.infocrit.ret.fin","num.st.infocrit.ret.fin","num.sst.infocrit.ret.fin")[which(c(num.infocrit.ret.fin[i,5],num.st.infocrit.ret.fin[i,5],num.sst.infocrit.ret.fin[i,5])==min(num.infocrit.ret.fin[i,5],num.st.infocrit.ret.fin[i,5],num.sst.infocrit.ret.fin[i,5]))]))[i,5]
	}

comp.infocrit.ret.cg<-data.frame(matrix(NA, nrow=15, ncol=3))
colnames(comp.infocrit.ret.cg)<-c("Distribution","Model","Infocrit")
for (i in 1:15)
	{
	rownames(comp.infocrit.ret.cg)[i]<-paste("ret.",colnames(base.cg[i]),sep="")
	}

for (i in 1:15)
	{
	comp.infocrit.ret.cg[i,"Distribution"]<-c("norm","st","sst")[which(c(num.infocrit.ret.cg[i,5],num.st.infocrit.ret.cg[i,5],num.sst.infocrit.ret.cg[i,5])==min(num.infocrit.ret.cg[i,5],num.st.infocrit.ret.cg[i,5],num.sst.infocrit.ret.cg[i,5]))]
	comp.infocrit.ret.cg[i,"Model"]<-get(paste(c("infocrit.ret.cg","st.infocrit.ret.cg","sst.infocrit.ret.cg")[which(c(num.infocrit.ret.cg[i,5],num.st.infocrit.ret.cg[i,5],num.sst.infocrit.ret.cg[i,5])==min(num.infocrit.ret.cg[i,5],num.st.infocrit.ret.cg[i,5],num.sst.infocrit.ret.cg[i,5]))]))[i,5]
	comp.infocrit.ret.cg[i,"Infocrit"]<-get(paste(c("num.infocrit.ret.cg","num.st.infocrit.ret.cg","num.sst.infocrit.ret.cg")[which(c(num.infocrit.ret.cg[i,5],num.st.infocrit.ret.cg[i,5],num.sst.infocrit.ret.cg[i,5])==min(num.infocrit.ret.cg[i,5],num.st.infocrit.ret.cg[i,5],num.sst.infocrit.ret.cg[i,5]))]))[i,5]
	}
	
comp.infocrit.ret.ind<-data.frame(matrix(NA, nrow=15, ncol=3))
colnames(comp.infocrit.ret.ind)<-c("Distribution","Model","Infocrit")
for (i in 1:15)
	{
	rownames(comp.infocrit.ret.ind)[i]<-paste("ret.",colnames(base.ind[i]),sep="")
	}

for (i in 1:15)
	{
	comp.infocrit.ret.ind[i,"Distribution"]<-c("norm","st","sst")[which(c(num.infocrit.ret.ind[i,5],num.st.infocrit.ret.ind[i,5],num.sst.infocrit.ret.ind[i,5])==min(num.infocrit.ret.ind[i,5],num.st.infocrit.ret.ind[i,5],num.sst.infocrit.ret.ind[i,5]))]
	comp.infocrit.ret.ind[i,"Model"]<-get(paste(c("infocrit.ret.ind","st.infocrit.ret.ind","sst.infocrit.ret.ind")[which(c(num.infocrit.ret.ind[i,5],num.st.infocrit.ret.ind[i,5],num.sst.infocrit.ret.ind[i,5])==min(num.infocrit.ret.ind[i,5],num.st.infocrit.ret.ind[i,5],num.sst.infocrit.ret.ind[i,5]))]))[i,5]
	comp.infocrit.ret.ind[i,"Infocrit"]<-get(paste(c("num.infocrit.ret.ind","num.st.infocrit.ret.ind","num.sst.infocrit.ret.ind")[which(c(num.infocrit.ret.ind[i,5],num.st.infocrit.ret.ind[i,5],num.sst.infocrit.ret.ind[i,5])==min(num.infocrit.ret.ind[i,5],num.st.infocrit.ret.ind[i,5],num.sst.infocrit.ret.ind[i,5]))]))[i,5]
	}
	
comp.infocrit.ret.exch<-data.frame(matrix(NA, nrow=10, ncol=3))
colnames(comp.infocrit.ret.exch)<-c("Distribution","Model","Infocrit")
for (i in 1:10)
	{
	rownames(comp.infocrit.ret.exch)[i]<-paste("ret.",colnames(base.exch[i]),sep="")
	}

for (i in 1:10)
	{
	comp.infocrit.ret.exch[i,"Distribution"]<-c("norm","st","sst")[which(c(num.infocrit.ret.exch[i,5],num.st.infocrit.ret.exch[i,5],num.sst.infocrit.ret.exch[i,5])==min(num.infocrit.ret.exch[i,5],num.st.infocrit.ret.exch[i,5],num.sst.infocrit.ret.exch[i,5]))]
	comp.infocrit.ret.exch[i,"Model"]<-get(paste(c("infocrit.ret.exch","st.infocrit.ret.exch","sst.infocrit.ret.exch")[which(c(num.infocrit.ret.exch[i,5],num.st.infocrit.ret.exch[i,5],num.sst.infocrit.ret.exch[i,5])==min(num.infocrit.ret.exch[i,5],num.st.infocrit.ret.exch[i,5],num.sst.infocrit.ret.exch[i,5]))]))[i,5]
	comp.infocrit.ret.exch[i,"Infocrit"]<-get(paste(c("num.infocrit.ret.exch","num.st.infocrit.ret.exch","num.sst.infocrit.ret.exch")[which(c(num.infocrit.ret.exch[i,5],num.st.infocrit.ret.exch[i,5],num.sst.infocrit.ret.exch[i,5])==min(num.infocrit.ret.exch[i,5],num.st.infocrit.ret.exch[i,5],num.sst.infocrit.ret.exch[i,5]))]))[i,5]
	}
	
	comp.infocrit.ret.fin
	comp.infocrit.ret.cg
	comp.infocrit.ret.ind
	comp.infocrit.ret.exch
	
	

###############################################################################################
# Combined Nyblom Stability Test	

comp.nyblom.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(comp.nyblom.test.ret.fin)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.fin[,1])))
		{
		colnames(comp.nyblom.test.ret.fin)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.fin[,1])) & i<=1+max(arima.ret.fin[,1])+(max(arima.ret.fin[,2])))
		{
		colnames(comp.nyblom.test.ret.fin)[i]<-paste("ma",i-1-max(arima.ret.fin[,1]),sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2])) & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1)
		{
		colnames(comp.nyblom.test.ret.fin)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1)
		{
		colnames(comp.nyblom.test.ret.fin)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1)
		{
		colnames(comp.nyblom.test.ret.fin)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.fin)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.fin)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.fin)[i]<-paste("shape",sep="")
		}	
	}
rownames(comp.nyblom.test.ret.fin)<-colnames(ret.fin)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.fin[,1]))+(max(arima.ret.fin[,2]))+1+1+1+1+1+1))
		{
		if (comp.infocrit.ret.fin[i,1]=="norm")
			{
			if (is.na(nyblom(get(paste(comp.infocrit.ret.fin[i,2],".ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j])!=TRUE)
				{
				comp.nyblom.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),rownames(nyblom(get(paste(comp.infocrit.ret.fin[i,2],".ret.",colnames(base.fin[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste(comp.infocrit.ret.fin[i,2],".ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j]
				}
			}
		else if (comp.infocrit.ret.fin[i,1]!="norm")
			{
			if (is.na(nyblom(get(paste(comp.infocrit.ret.fin[i,1],".",comp.infocrit.ret.fin[i,2],".ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j])!=TRUE)
				{
				comp.nyblom.test.ret.fin[paste("ret.",colnames(base.fin[i]),sep=""),rownames(nyblom(get(paste(comp.infocrit.ret.fin[i,1],".",comp.infocrit.ret.fin[i,2],".ret.",colnames(base.fin[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste(comp.infocrit.ret.fin[i,1],".",comp.infocrit.ret.fin[i,2],".ret.",colnames(base.fin[i]),sep="")))$IndividualStat[j]
				}
			}
		}
	}

comp.nyblom.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(comp.nyblom.test.ret.cg)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.cg[,1])))
		{
		colnames(comp.nyblom.test.ret.cg)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.cg[,1])) & i<=1+max(arima.ret.cg[,1])+(max(arima.ret.cg[,2])))
		{
		colnames(comp.nyblom.test.ret.cg)[i]<-paste("ma",i-1-max(arima.ret.cg[,1]),sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2])) & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1)
		{
		colnames(comp.nyblom.test.ret.cg)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1)
		{
		colnames(comp.nyblom.test.ret.cg)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1)
		{
		colnames(comp.nyblom.test.ret.cg)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.cg)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.cg)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.cg)[i]<-paste("shape",sep="")
		}	
	}
rownames(comp.nyblom.test.ret.cg)<-colnames(ret.cg)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.cg[,1]))+(max(arima.ret.cg[,2]))+1+1+1+1+1+1))
		{
		if (comp.infocrit.ret.cg[i,1]=="norm")
			{
			if (is.na(nyblom(get(paste(comp.infocrit.ret.cg[i,2],".ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j])!=TRUE)
				{
				comp.nyblom.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),rownames(nyblom(get(paste(comp.infocrit.ret.cg[i,2],".ret.",colnames(base.cg[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste(comp.infocrit.ret.cg[i,2],".ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j]
				}
			}
		else if (comp.infocrit.ret.cg[i,1]!="norm")
			{
			if (is.na(nyblom(get(paste(comp.infocrit.ret.cg[i,1],".",comp.infocrit.ret.cg[i,2],".ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j])!=TRUE)
				{
				comp.nyblom.test.ret.cg[paste("ret.",colnames(base.cg[i]),sep=""),rownames(nyblom(get(paste(comp.infocrit.ret.cg[i,1],".",comp.infocrit.ret.cg[i,2],".ret.",colnames(base.cg[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste(comp.infocrit.ret.cg[i,1],".",comp.infocrit.ret.cg[i,2],".ret.",colnames(base.cg[i]),sep="")))$IndividualStat[j]
				}
			}
		}
	}


comp.nyblom.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = (1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(comp.nyblom.test.ret.ind)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.ind[,1])))
		{
		colnames(comp.nyblom.test.ret.ind)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.ind[,1])) & i<=1+max(arima.ret.ind[,1])+(max(arima.ret.ind[,2])))
		{
		colnames(comp.nyblom.test.ret.ind)[i]<-paste("ma",i-1-max(arima.ret.ind[,1]),sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2])) & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1)
		{
		colnames(comp.nyblom.test.ret.ind)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1)
		{
		colnames(comp.nyblom.test.ret.ind)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1)
		{
		colnames(comp.nyblom.test.ret.ind)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.ind)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.ind)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.ind)[i]<-paste("shape",sep="")
		}	
	}
rownames(comp.nyblom.test.ret.ind)<-colnames(ret.ind)


for (i in 1:15)
	{
	for (j in 1:(1+(max(arima.ret.ind[,1]))+(max(arima.ret.ind[,2]))+1+1+1+1+1+1))
		{
		if (comp.infocrit.ret.ind[i,1]=="norm")
			{
			if (is.na(nyblom(get(paste(comp.infocrit.ret.ind[i,2],".ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j])!=TRUE)
				{
				comp.nyblom.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),rownames(nyblom(get(paste(comp.infocrit.ret.ind[i,2],".ret.",colnames(base.ind[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste(comp.infocrit.ret.ind[i,2],".ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j]
				}
			}
		else if (comp.infocrit.ret.ind[i,1]!="norm")
			{
			if (is.na(nyblom(get(paste(comp.infocrit.ret.ind[i,1],".",comp.infocrit.ret.ind[i,2],".ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j])!=TRUE)
				{
				comp.nyblom.test.ret.ind[paste("ret.",colnames(base.ind[i]),sep=""),rownames(nyblom(get(paste(comp.infocrit.ret.ind[i,1],".",comp.infocrit.ret.ind[i,2],".ret.",colnames(base.ind[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste(comp.infocrit.ret.ind[i,1],".",comp.infocrit.ret.ind[i,2],".ret.",colnames(base.ind[i]),sep="")))$IndividualStat[j]
				}
			}
		}
	}


comp.nyblom.test.ret.exch<-data.frame(matrix(NA, nrow = 10, ncol = (1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]+1+1+1+1+1+1))))

for (i in 1:(1+(max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1+1+1+1+1+1))
	{
	if (i==1)
		{
		colnames(comp.nyblom.test.ret.exch)[i]<-paste("mu")
		}
	if (i>1 & i<=(1+max(arima.ret.exch[,1])))
		{
		colnames(comp.nyblom.test.ret.exch)[i]<-paste("ar",i-1,sep="")
		}
	if (i>(1+max(arima.ret.exch[,1])) & i<=1+max(arima.ret.exch[,1])+(max(arima.ret.exch[,2])))
		{
		colnames(comp.nyblom.test.ret.exch)[i]<-paste("ma",i-1-max(arima.ret.exch[,1]),sep="")
		}
	if (i>(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2])) & i<=(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1)
		{
		colnames(comp.nyblom.test.ret.exch)[i]<-paste("omega",sep="")
		}
	if (i>(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1 & i<=(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1+1)
		{
		colnames(comp.nyblom.test.ret.exch)[i]<-paste("alpha1",sep="")
		}
	if (i>(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1+1 & i<=(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1+1+1)
		{
		colnames(comp.nyblom.test.ret.exch)[i]<-paste("beta1",sep="")
		}	
	if (i>(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1+1+1 & i<=(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.exch)[i]<-paste("gamma1",sep="")
		}	
	if (i>(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1+1+1+1 & i<=(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.exch)[i]<-paste("skew",sep="")
		}	
	if (i>(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1+1+1+1+1 & i<=(1+max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1+1+1+1+1+1)
		{
		colnames(comp.nyblom.test.ret.exch)[i]<-paste("shape",sep="")
		}	
	}
rownames(comp.nyblom.test.ret.exch)<-colnames(ret.exch)

for (i in 1:10)
	{
	for (j in 1:(1+(max(arima.ret.exch[,1]))+(max(arima.ret.exch[,2]))+1+1+1+1+1+1))
		{
		if (comp.infocrit.ret.exch[i,1]=="norm")
			{
			if (is.na(nyblom(get(paste(comp.infocrit.ret.exch[i,2],".ret.",colnames(base.exch[i]),sep="")))$IndividualStat[j])!=TRUE)
				{
				comp.nyblom.test.ret.exch[paste("ret.",colnames(base.exch[i]),sep=""),rownames(nyblom(get(paste(comp.infocrit.ret.exch[i,2],".ret.",colnames(base.exch[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste(comp.infocrit.ret.exch[i,2],".ret.",colnames(base.exch[i]),sep="")))$IndividualStat[j]
				}
			}
		else if (comp.infocrit.ret.exch[i,1]!="norm")
			{
			if (is.na(nyblom(get(paste(comp.infocrit.ret.exch[i,1],".",comp.infocrit.ret.exch[i,2],".ret.",colnames(base.exch[i]),sep="")))$IndividualStat[j])!=TRUE)
				{
				comp.nyblom.test.ret.exch[paste("ret.",colnames(base.exch[i]),sep=""),rownames(nyblom(get(paste(comp.infocrit.ret.exch[i,1],".",comp.infocrit.ret.exch[i,2],".ret.",colnames(base.exch[i]),sep="")))$IndividualStat)[j]]<-nyblom(get(paste(comp.infocrit.ret.exch[i,1],".",comp.infocrit.ret.exch[i,2],".ret.",colnames(base.exch[i]),sep="")))$IndividualStat[j]
				}
			}
		}
	}
	
		
comp.nyblom.test.ret.fin
comp.nyblom.test.ret.cg
comp.nyblom.test.ret.ind
comp.nyblom.test.ret.exch

		

		

	
################################################################################################
# Persistence of GARCH model
comp.persistence.ret.fin<-c()
comp.persistence.ret.fin<-data.frame(comp.persistence.ret.fin)
for (i in 1:15)
	{
	if (comp.infocrit.ret.fin[i,1]=="norm")
		{
		comp.persistence.ret.fin[colnames(ret.fin[,i]),"alpha"]<-get(paste("coef.ret.fin",sep=""))[i,"alpha1"]
		comp.persistence.ret.fin[colnames(ret.fin[,i]),"beta"]<-get(paste("coef.ret.fin",sep=""))[i,"beta1"]
		comp.persistence.ret.fin[colnames(ret.fin[,i]),"persistence"]<-comp.persistence.ret.fin[colnames(ret.fin[,i]),"alpha"]+comp.persistence.ret.fin[colnames(ret.fin[,i]),"beta"]
		}
	
	else if (comp.infocrit.ret.fin[i,1]!="norm")
		{
		comp.persistence.ret.fin[colnames(ret.fin[,i]),"alpha"]<-get(paste((comp.infocrit.ret.fin[i,1]),".",(comp.infocrit.ret.fin[i,2]),".coef.ret.fin",sep=""))[i,"alpha1"]
		comp.persistence.ret.fin[colnames(ret.fin[,i]),"beta"]<-get(paste((comp.infocrit.ret.fin[i,1]),".",(comp.infocrit.ret.fin[i,2]),".coef.ret.fin",sep=""))[i,"beta1"]
		comp.persistence.ret.fin[colnames(ret.fin[,i]),"persistence"]<-comp.persistence.ret.fin[colnames(ret.fin[,i]),"alpha"]+comp.persistence.ret.fin[colnames(ret.fin[,i]),"beta"]
		}
	}
	
comp.persistence.ret.cg<-c()
comp.persistence.ret.cg<-data.frame(comp.persistence.ret.cg)
for (i in 1:15)
	{
	if (comp.infocrit.ret.cg[i,1]=="norm")
		{
		comp.persistence.ret.cg[colnames(ret.cg[,i]),"alpha"]<-get(paste("coef.ret.cg",sep=""))[i,"alpha1"]
		comp.persistence.ret.cg[colnames(ret.cg[,i]),"beta"]<-get(paste("coef.ret.cg",sep=""))[i,"beta1"]
		comp.persistence.ret.cg[colnames(ret.cg[,i]),"persistence"]<-comp.persistence.ret.cg[colnames(ret.cg[,i]),"alpha"]+comp.persistence.ret.cg[colnames(ret.cg[,i]),"beta"]
		}
	
	else if (comp.infocrit.ret.cg[i,1]!="norm")
		{
		comp.persistence.ret.cg[colnames(ret.cg[,i]),"alpha"]<-get(paste((comp.infocrit.ret.cg[i,1]),".",(comp.infocrit.ret.cg[i,2]),".coef.ret.cg",sep=""))[i,"alpha1"]
		comp.persistence.ret.cg[colnames(ret.cg[,i]),"beta"]<-get(paste((comp.infocrit.ret.cg[i,1]),".",(comp.infocrit.ret.cg[i,2]),".coef.ret.cg",sep=""))[i,"beta1"]
		comp.persistence.ret.cg[colnames(ret.cg[,i]),"persistence"]<-comp.persistence.ret.cg[colnames(ret.cg[,i]),"alpha"]+comp.persistence.ret.cg[colnames(ret.cg[,i]),"beta"]
		}
	}
	
comp.persistence.ret.ind<-c()
comp.persistence.ret.ind<-data.frame(comp.persistence.ret.ind)
for (i in 1:15)
	{
	if (comp.infocrit.ret.ind[i,1]=="norm")
		{
		comp.persistence.ret.ind[colnames(ret.ind[,i]),"alpha"]<-get(paste("coef.ret.ind",sep=""))[i,"alpha1"]
		comp.persistence.ret.ind[colnames(ret.ind[,i]),"beta"]<-get(paste("coef.ret.ind",sep=""))[i,"beta1"]
		comp.persistence.ret.ind[colnames(ret.ind[,i]),"persistence"]<-comp.persistence.ret.ind[colnames(ret.ind[,i]),"alpha"]+comp.persistence.ret.ind[colnames(ret.ind[,i]),"beta"]
		}
	
	else if (comp.infocrit.ret.ind[i,1]!="norm")
		{
		comp.persistence.ret.ind[colnames(ret.ind[,i]),"alpha"]<-get(paste((comp.infocrit.ret.ind[i,1]),".",(comp.infocrit.ret.ind[i,2]),".coef.ret.ind",sep=""))[i,"alpha1"]
		comp.persistence.ret.ind[colnames(ret.ind[,i]),"beta"]<-get(paste((comp.infocrit.ret.ind[i,1]),".",(comp.infocrit.ret.ind[i,2]),".coef.ret.ind",sep=""))[i,"beta1"]
		comp.persistence.ret.ind[colnames(ret.ind[,i]),"persistence"]<-comp.persistence.ret.ind[colnames(ret.ind[,i]),"alpha"]+comp.persistence.ret.ind[colnames(ret.ind[,i]),"beta"]
		}
	}
	
comp.persistence.ret.exch<-c()
comp.persistence.ret.exch<-data.frame(comp.persistence.ret.exch)
for (i in 1:10)
	{
	if (comp.infocrit.ret.exch[i,1]=="norm")
		{
		comp.persistence.ret.exch[colnames(ret.exch[,i]),"alpha"]<-get(paste("coef.ret.exch",sep=""))[i,"alpha1"]
		comp.persistence.ret.exch[colnames(ret.exch[,i]),"beta"]<-get(paste("coef.ret.exch",sep=""))[i,"beta1"]
		comp.persistence.ret.exch[colnames(ret.exch[,i]),"persistence"]<-comp.persistence.ret.exch[colnames(ret.exch[,i]),"alpha"]+comp.persistence.ret.exch[colnames(ret.exch[,i]),"beta"]
		}
	
	else if (comp.infocrit.ret.exch[i,1]!="norm")
		{
		comp.persistence.ret.exch[colnames(ret.exch[,i]),"alpha"]<-get(paste((comp.infocrit.ret.exch[i,1]),".",(comp.infocrit.ret.exch[i,2]),".coef.ret.exch",sep=""))[i,"alpha1"]
		comp.persistence.ret.exch[colnames(ret.exch[,i]),"beta"]<-get(paste((comp.infocrit.ret.exch[i,1]),".",(comp.infocrit.ret.exch[i,2]),".coef.ret.exch",sep=""))[i,"beta1"]
		comp.persistence.ret.exch[colnames(ret.exch[,i]),"persistence"]<-comp.persistence.ret.exch[colnames(ret.exch[,i]),"alpha"]+comp.persistence.ret.exch[colnames(ret.exch[,i]),"beta"]
		}
	}
	
	comp.persistence.ret.fin
	comp.persistence.ret.cg
	comp.persistence.ret.ind
	comp.persistence.ret.exch
	
	

#################################################################################################################	
# NEWS IMPACT CURVE of Univariate GARCH Models
	

news.impact.ret.fin.zx<- matrix(nrow = 100)
news.impact.ret.fin.zx<-data.frame(news.impact.ret.fin.zx)
for (i in 1:15)
	{
	if (comp.infocrit.ret.fin[i,1]=="norm")
	{
	news.impact.ret.fin.zx[,colnames(ret.fin[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.fin[i,2],".",colnames(ret.fin[,i]),sep="")))$zx)
	}
else if (comp.infocrit.ret.fin[i,1]!="norm")
	{
	news.impact.ret.fin.zx[,colnames(ret.fin[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.fin[i,1],".",comp.infocrit.ret.fin[i,2],".",colnames(ret.fin[,i]),sep="")))$zx)
	}
	}
news.impact.ret.fin.zx<-news.impact.ret.fin.zx[-1]
news.impact.ret.fin.zx<-news.impact.ret.fin.zx[,1]

news.impact.ret.fin.zy<- matrix(nrow = 100)
news.impact.ret.fin.zy<-data.frame(news.impact.ret.fin.zy)
for (i in 1:15)
	{
	if (comp.infocrit.ret.fin[i,1]=="norm")
	{
	news.impact.ret.fin.zy[,colnames(ret.fin[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.fin[i,2],".",colnames(ret.fin[,i]),sep="")))$zy)
	}
else if (comp.infocrit.ret.fin[i,1]!="norm")
	{
	news.impact.ret.fin.zy[,colnames(ret.fin[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.fin[i,1],".",comp.infocrit.ret.fin[i,2],".",colnames(ret.fin[,i]),sep="")))$zy)
	}
	}
news.impact.ret.fin.zy<-news.impact.ret.fin.zy[-1]


news.impact.ret.cg.zx<- matrix(nrow = 100)
news.impact.ret.cg.zx<-data.frame(news.impact.ret.cg.zx)
for (i in 1:15)
	{
	if (comp.infocrit.ret.cg[i,1]=="norm")
	{
	news.impact.ret.cg.zx[,colnames(ret.cg[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.cg[i,2],".",colnames(ret.cg[,i]),sep="")))$zx)
	}
else if (comp.infocrit.ret.cg[i,1]!="norm")
	{
	news.impact.ret.cg.zx[,colnames(ret.cg[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.cg[i,1],".",comp.infocrit.ret.cg[i,2],".",colnames(ret.cg[,i]),sep="")))$zx)
	}
	}
news.impact.ret.cg.zx<-news.impact.ret.cg.zx[-1]
news.impact.ret.cg.zx<-news.impact.ret.cg.zx[,1]

news.impact.ret.cg.zy<- matrix(nrow = 100)
news.impact.ret.cg.zy<-data.frame(news.impact.ret.cg.zy)
for (i in 1:15)
	{
	if (comp.infocrit.ret.cg[i,1]=="norm")
	{
	news.impact.ret.cg.zy[,colnames(ret.cg[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.cg[i,2],".",colnames(ret.cg[,i]),sep="")))$zy)
	}
else if (comp.infocrit.ret.cg[i,1]!="norm")
	{
	news.impact.ret.cg.zy[,colnames(ret.cg[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.cg[i,1],".",comp.infocrit.ret.cg[i,2],".",colnames(ret.cg[,i]),sep="")))$zy)
	}
	}
news.impact.ret.cg.zy<-news.impact.ret.cg.zy[-1]


news.impact.ret.ind.zx<- matrix(nrow = 100)
news.impact.ret.ind.zx<-data.frame(news.impact.ret.ind.zx)
for (i in 1:15)
	{
	if (comp.infocrit.ret.ind[i,1]=="norm")
	{
	news.impact.ret.ind.zx[,colnames(ret.ind[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.ind[i,2],".",colnames(ret.ind[,i]),sep="")))$zx)
	}
else if (comp.infocrit.ret.ind[i,1]!="norm")
	{
	news.impact.ret.ind.zx[,colnames(ret.ind[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.ind[i,1],".",comp.infocrit.ret.ind[i,2],".",colnames(ret.ind[,i]),sep="")))$zx)
	}
	}
news.impact.ret.ind.zx<-news.impact.ret.ind.zx[-1]
news.impact.ret.ind.zx<-news.impact.ret.ind.zx[,1]

news.impact.ret.ind.zy<- matrix(nrow = 100)
news.impact.ret.ind.zy<-data.frame(news.impact.ret.ind.zy)
for (i in 1:15)
	{
	if (comp.infocrit.ret.ind[i,1]=="norm")
	{
	news.impact.ret.ind.zy[,colnames(ret.ind[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.ind[i,2],".",colnames(ret.ind[,i]),sep="")))$zy)
	}
else if (comp.infocrit.ret.ind[i,1]!="norm")
	{
	news.impact.ret.ind.zy[,colnames(ret.ind[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.ind[i,1],".",comp.infocrit.ret.ind[i,2],".",colnames(ret.ind[,i]),sep="")))$zy)
	}
	}
news.impact.ret.ind.zy<-news.impact.ret.ind.zy[-1]

news.impact.ret.exch.zx<- matrix(nrow = 100)
news.impact.ret.exch.zx<-data.frame(news.impact.ret.exch.zx)
for (i in 1:10)
	{
	if (comp.infocrit.ret.exch[i,1]=="norm")
	{
	news.impact.ret.exch.zx[,colnames(ret.exch[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.exch[i,2],".",colnames(ret.exch[,i]),sep="")))$zx)
	}
else if (comp.infocrit.ret.exch[i,1]!="norm")
	{
	news.impact.ret.exch.zx[,colnames(ret.exch[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.exch[i,1],".",comp.infocrit.ret.exch[i,2],".",colnames(ret.exch[,i]),sep="")))$zx)
	}
	}
news.impact.ret.exch.zx<-news.impact.ret.exch.zx[-1]
news.impact.ret.exch.zx<-news.impact.ret.exch.zx[,1]

news.impact.ret.exch.zy<- matrix(nrow = 100)
news.impact.ret.exch.zy<-data.frame(news.impact.ret.exch.zy)
for (i in 1:10)
	{
	if (comp.infocrit.ret.exch[i,1]=="norm")
	{
	news.impact.ret.exch.zy[,colnames(ret.exch[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.exch[i,2],".",colnames(ret.exch[,i]),sep="")))$zy)
	}
else if (comp.infocrit.ret.exch[i,1]!="norm")
	{
	news.impact.ret.exch.zy[,colnames(ret.exch[,i])]<-data.frame(newsimpact(get(paste(comp.infocrit.ret.exch[i,1],".",comp.infocrit.ret.exch[i,2],".",colnames(ret.exch[,i]),sep="")))$zy)
	}
	}
news.impact.ret.exch.zy<-news.impact.ret.exch.zy[-1]
# plot news impact curve
pdf("news impact.pdf",paper="a4r",width=11.69,height=8.27)

ggplot(melt(cbind(news.impact.ret.fin.zx,news.impact.ret.fin.zy),id="news.impact.ret.fin.zx"),aes(x=news.impact.ret.fin.zx,y=value,colour=variable,group=variable))+geom_line()+xlab(expression(z[t - 1])) +  ylab(expression(sigma[t]^2)) +ggtitle("News Impact Curve")

ggplot(melt(cbind(news.impact.ret.cg.zx,news.impact.ret.cg.zy),id="news.impact.ret.cg.zx"),aes(x=news.impact.ret.cg.zx,y=value,colour=variable,group=variable))+geom_line()+xlab(expression(z[t - 1])) +  ylab(expression(sigma[t]^2)) +ggtitle("News Impact Curve")

ggplot(melt(cbind(news.impact.ret.ind.zx,news.impact.ret.ind.zy),id="news.impact.ret.ind.zx"),aes(x=news.impact.ret.ind.zx,y=value,colour=variable,group=variable))+geom_line()+xlab(expression(z[t - 1])) +  ylab(expression(sigma[t]^2)) +ggtitle("News Impact Curve")

ggplot(melt(cbind(news.impact.ret.exch.zx,news.impact.ret.exch.zy),id="news.impact.ret.exch.zx"),aes(x=news.impact.ret.exch.zx,y=value,colour=variable,group=variable))+geom_line()+xlab(expression(z[t - 1])) +  ylab(expression(sigma[t]^2)) +ggtitle("News Impact Curve")

dev.off()	

##############################################################
# DCC TEST
dcc.test.ret.fin<-data.frame(matrix(NA, nrow = 15, ncol = 1))
colnames(dcc.test.ret.fin)<-paste("p.value")
for (i in 1:15)
	{
	rownames(dcc.test.ret.fin)[i]<-paste("ret.",colnames(base.fin)[i],sep="")
	}

for (i in 1:15)
	{
	if (dim(get(paste("ret.",colnames(base.fin)[i],sep="")))[1]==2749)
		{
		temp<-cbind(as.matrix(ret.fin.id[2:2749]),as.matrix(get(paste("ret.",colnames(base.fin)[i],sep=""))[1:2748]))
		dcc.test.ret.fin[paste("ret.",colnames(base.fin)[i],sep=""),"p.value"]<-DCCtest(temp,garchOrder=c(1,1), n.lags=1, solver="gosolnp")$p.value
		}
	else if (dim(get(paste("ret.",colnames(base.fin)[i],sep="")))[1]==2748)
		{
		temp<-cbind(as.matrix(ret.fin.id[3:2749]),as.matrix(get(paste("ret.",colnames(base.fin)[i],sep=""))[1:2747]))
		dcc.test.ret.fin[paste("ret.",colnames(base.fin)[i],sep=""),"p.value"]<-DCCtest(temp,garchOrder=c(1,1), n.lags=1, solver="gosolnp")$p.value
		}
  	}
	
dcc.test.ret.cg<-data.frame(matrix(NA, nrow = 15, ncol = 1))
colnames(dcc.test.ret.cg)<-paste("p.value")
for (i in 1:15)
	{
	rownames(dcc.test.ret.cg)[i]<-paste("ret.",colnames(base.cg)[i],sep="")
	}

for (i in 1:15)
	{
	if (dim(get(paste("ret.",colnames(base.cg)[i],sep="")))[1]==2749)
		{
		temp<-cbind(as.matrix(ret.cg.id[2:2749]),as.matrix(get(paste("ret.",colnames(base.cg)[i],sep=""))[1:2748]))
		dcc.test.ret.cg[paste("ret.",colnames(base.cg)[i],sep=""),"p.value"]<-DCCtest(temp,garchOrder=c(1,1), n.lags=1, solver="gosolnp")$p.value
		}
	else if (dim(get(paste("ret.",colnames(base.cg)[i],sep="")))[1]==2748)
		{
		temp<-cbind(as.matrix(ret.cg.id[3:2749]),as.matrix(get(paste("ret.",colnames(base.cg)[i],sep=""))[1:2747]))
		dcc.test.ret.cg[paste("ret.",colnames(base.cg)[i],sep=""),"p.value"]<-DCCtest(temp,garchOrder=c(1,1), n.lags=1, solver="gosolnp")$p.value
		}
  	}
	
dcc.test.ret.ind<-data.frame(matrix(NA, nrow = 15, ncol = 1))
colnames(dcc.test.ret.ind)<-paste("p.value")
for (i in 1:15)
	{
	rownames(dcc.test.ret.ind)[i]<-paste("ret.",colnames(base.ind)[i],sep="")
	}

for (i in 1:15)
	{
	if (dim(get(paste("ret.",colnames(base.ind)[i],sep="")))[1]==2749)
		{
		temp<-cbind(as.matrix(ret.ind.id[2:2749]),as.matrix(get(paste("ret.",colnames(base.ind)[i],sep=""))[1:2748]))
		dcc.test.ret.ind[paste("ret.",colnames(base.ind)[i],sep=""),"p.value"]<-DCCtest(temp,garchOrder=c(1,1), n.lags=1, solver="gosolnp")$p.value
		}
	else if (dim(get(paste("ret.",colnames(base.ind)[i],sep="")))[1]==2748)
		{
		temp<-cbind(as.matrix(ret.ind.id[3:2749]),as.matrix(get(paste("ret.",colnames(base.ind)[i],sep=""))[1:2747]))
		dcc.test.ret.ind[paste("ret.",colnames(base.ind)[i],sep=""),"p.value"]<-DCCtest(temp,garchOrder=c(1,1), n.lags=1, solver="gosolnp")$p.value
		}
  	}	
	
# DCC Model
#USA, Japan, China, and Singapore, Germnay and Greece

 # [1] "ret.fin.chn"  "ret.fin.fra"  "ret.fin.ger"  "ret.fin.gre" 
 # [5] "ret.fin.hk"   "ret.fin.id"   "ret.fin.ita"  "ret.fin.jp"  
 # [9] "ret.fin.my"   "ret.fin.sing" "ret.fin.skor" "ret.fin.sp"  
# [13] "ret.fin.th"   "ret.fin.uk"   "ret.fin.us" 

#CHN,GER,JP,MY,SING,US 	1,3,8,9,10,15
#ONlY on FIN: GRE					4


for(i in c(1,3,4,8,9,10,15))
	{
	mspec.fin<-c()
	spec1<-c()
	spec2<-c()
	spec1<-get(paste("spec.",comp.infocrit.ret.fin[6,1],".",comp.infocrit.ret.fin[6,2],".ret.",colnames(base.fin[6]),sep=""))
	spec2<-get(paste("spec.",comp.infocrit.ret.fin[i,1],".",comp.infocrit.ret.fin[i,2],".ret.",colnames(base.fin[i]),sep=""))
	mspec.fin=multispec(c(spec1,spec2))
	
	
	assign(paste("dcc.spec.fin"),dccspec(get(paste("mspec.fin")), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL, robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), dccOrder = c(1,1), model = c("DCC"), groups = rep(1, length(uspec@spec)), distribution = c("mvnorm"), start.pars = list(), fixed.pars = list()))
	
	if (dim(get(paste("ret.",colnames(base.fin)[i],sep="")))[1]==2749)
		{
		temp.dcc.data<-cbind(as.matrix(ret.fin.id[2:2749]),as.matrix(get(paste("ret.",colnames(base.fin)[i],sep=""))[1:2748]))
		}
	else if (dim(get(paste("ret.",colnames(base.fin)[i],sep="")))[1]==2748)
		{
		temp.dcc.data<-cbind(as.matrix(ret.fin.id[3:2749]),as.matrix(get(paste("ret.",colnames(base.fin)[i],sep=""))[1:2747]))
		}
	
	colnames(temp.dcc.data)<-c("ret.fin.id",paste("ret.",colnames(base.fin)[i],sep=""))
		
	assign(paste("dcc.fit.fin.id.",colnames(base.fin[i]),sep=""),dccfit(dcc.spec.fin, temp.dcc.data, out.sample = 30, solver = "gosolnp", solver.control = list(),fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), cluster = NULL, fit = NULL, VAR.fit = NULL))
	}


for(i in c(1,3,4,8,9,10,15))
	{
	mspec.cg<-c()
	spec1<-c()
	spec2<-c()
	spec1<-get(paste("spec.",comp.infocrit.ret.cg[6,1],".",comp.infocrit.ret.cg[6,2],".ret.",colnames(base.cg[6]),sep=""))
	spec2<-get(paste("spec.",comp.infocrit.ret.cg[i,1],".",comp.infocrit.ret.cg[i,2],".ret.",colnames(base.cg[i]),sep=""))
	mspec.cg=multispec(c(spec1,spec2))
	
	
	assign(paste("dcc.spec.cg"),dccspec(get(paste("mspec.cg")), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL, robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), dccOrder = c(1,1), model = c("DCC"), groups = rep(1, length(uspec@spec)), distribution = c("mvnorm"), start.pars = list(), fixed.pars = list()))
	
	if (dim(get(paste("ret.",colnames(base.cg)[i],sep="")))[1]==2749)
		{
		temp.dcc.data<-cbind(as.matrix(ret.cg.id[2:2749]),as.matrix(get(paste("ret.",colnames(base.cg)[i],sep=""))[1:2748]))
		}
	else if (dim(get(paste("ret.",colnames(base.cg)[i],sep="")))[1]==2748)
		{
		temp.dcc.data<-cbind(as.matrix(ret.cg.id[3:2749]),as.matrix(get(paste("ret.",colnames(base.cg)[i],sep=""))[1:2747]))
		}
	
	colnames(temp.dcc.data)<-c("ret.cg.id",paste("ret.",colnames(base.cg)[i],sep=""))
		
	assign(paste("dcc.fit.cg.id.",colnames(base.cg[i]),sep=""),dccfit(dcc.spec.cg, temp.dcc.data, out.sample = 30, solver = "gosolnp", solver.control = list(),fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), cluster = NULL, fit = NULL, VAR.fit = NULL))
	}


for(i in c(1,3,4,8,9,10,15))
	{
	mspec.ind<-c()
	spec1<-c()
	spec2<-c()
	spec1<-get(paste("spec.",comp.infocrit.ret.ind[6,1],".",comp.infocrit.ret.ind[6,2],".ret.",colnames(base.ind[6]),sep=""))
	spec2<-get(paste("spec.",comp.infocrit.ret.ind[i,1],".",comp.infocrit.ret.ind[i,2],".ret.",colnames(base.ind[i]),sep=""))
	mspec.ind=multispec(c(spec1,spec2))
	
	
	assign(paste("dcc.spec.ind"),dccspec(get(paste("mspec.ind")), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL, robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), dccOrder = c(1,1), model = c("DCC"), groups = rep(1, length(uspec@spec)), distribution = c("mvnorm"), start.pars = list(), fixed.pars = list()))
	
	if (dim(get(paste("ret.",colnames(base.ind)[i],sep="")))[1]==2749)
		{
		temp.dcc.data<-cbind(as.matrix(ret.ind.id[2:2749]),as.matrix(get(paste("ret.",colnames(base.ind)[i],sep=""))[1:2748]))
		}
	else if (dim(get(paste("ret.",colnames(base.ind)[i],sep="")))[1]==2748)
		{
		temp.dcc.data<-cbind(as.matrix(ret.ind.id[3:2749]),as.matrix(get(paste("ret.",colnames(base.ind)[i],sep=""))[1:2747]))
		}
	
	colnames(temp.dcc.data)<-c("ret.ind.id",paste("ret.",colnames(base.ind)[i],sep=""))
		
	assign(paste("dcc.fit.ind.id.",colnames(base.ind[i]),sep=""),dccfit(dcc.spec.ind, temp.dcc.data, out.sample = 30, solver = "gosolnp", solver.control = list(),fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), cluster = NULL, fit = NULL, VAR.fit = NULL))
	}


pdf("rcov.dcc.fit.fin.pdf",paper="a4r",width=11.69,height=8.27)
for(i in c(1,3,4,8,9,10,15))
	{
	plot(as.xts(rcov(get(paste("dcc.fit.fin.id.",colnames(base.fin[i]),sep="")))[1,2,]),main=paste("DCC Conditional Covariance","\n ret.fin.id","-","ret.fin.",colnames(base.name[i]),sep=""))
	}
dev.off()
	
pdf("rcov.dcc.fit.cg.pdf",paper="a4r",width=11.69,height=8.27)
for(i in c(1,3,4,8,9,10,15))
	{
	plot(as.xts(rcov(get(paste("dcc.fit.cg.id.",colnames(base.cg[i]),sep="")))[1,2,]),main=paste("DCC Conditional Covariance","\n ret.cg.id","-","ret.cg.",colnames(base.name[i]),sep=""))
	}
dev.off()

pdf("rcov.dcc.fit.ind.pdf",paper="a4r",width=11.69,height=8.27)
for(i in c(1,3,4,8,9,10,15))
	{
	plot(as.xts(rcov(get(paste("dcc.fit.ind.id.",colnames(base.ind[i]),sep="")))[1,2,]),main=paste("DCC Conditional Covariance","\n ret.ind.id","-","ret.ind.",colnames(base.name[i]),sep=""))
	}
dev.off()


pdf("rcor.dcc.fit.fin.pdf",paper="a4r",width=11.69,height=8.27)
for(i in c(1,3,4,8,9,10,15))
	{
	plot(as.xts(rcor(get(paste("dcc.fit.fin.id.",colnames(base.fin[i]),sep="")))[1,2,]),main=paste("DCC Conditional Correlation","\n ret.fin.id","-","ret.fin.",colnames(base.name[i]),sep=""))
	}
dev.off()
	
pdf("rcor.dcc.fit.cg.pdf",paper="a4r",width=11.69,height=8.27)
for(i in c(1,3,4,8,9,10,15))
	{
	plot(as.xts(rcor(get(paste("dcc.fit.cg.id.",colnames(base.cg[i]),sep="")))[1,2,]),main=paste("DCC Conditional Correlation","\n ret.cg.id","-","ret.cg.",colnames(base.name[i]),sep=""))
	}
dev.off()

pdf("rcor.dcc.fit.ind.pdf",paper="a4r",width=11.69,height=8.27)
for(i in c(1,3,4,8,9,10,15))
	{
	plot(as.xts(rcor(get(paste("dcc.fit.ind.id.",colnames(base.ind[i]),sep="")))[1,2,]),main=paste("DCC Conditional Correlation","\n ret.ind.id","-","ret.ind.",colnames(base.name[i]),sep=""))
	}
dev.off()

#############################################

pdf("rcor.dcc.fit.fin.pdf",paper="a4r",width=11.69,height=8.27)
for(i in c(1,3,4,8,9,10,15))
	{
	plot(as.xts(rcor(get(paste("dcc.fit.fin.id.",colnames(base.fin[i]),sep="")))[1,2,]),main=paste("DCC Conditional Correlation","\n ret.fin.id","-","ret.fin.",colnames(base.name[i]),sep=""))
abline(v=as.POSIXct(as.Date("2007-02-27")))
abline(v=as.POSIXct(as.Date("2010-08-30")))
abline(v=as.POSIXct(as.Date("2010-02-02")))
	}
dev.off()
	
pdf("rcor.dcc.fit.cg.pdf",paper="a4r",width=11.69,height=8.27)
for(i in c(1,3,4,8,9,10,15))
	{
	plot(as.xts(rcor(get(paste("dcc.fit.cg.id.",colnames(base.cg[i]),sep="")))[1,2,]),main=paste("DCC Conditional Correlation","\n ret.cg.id","-","ret.cg.",colnames(base.name[i]),sep=""))
abline(v=as.POSIXct(as.Date("2007-02-27")))
abline(v=as.POSIXct(as.Date("2010-08-30")))
abline(v=as.POSIXct(as.Date("2010-02-02")))
	}
dev.off()

pdf("rcor.dcc.fit.ind.pdf",paper="a4r",width=11.69,height=8.27)
for(i in c(1,3,4,8,9,10,15))
	{
	plot(as.xts(rcor(get(paste("dcc.fit.ind.id.",colnames(base.ind[i]),sep="")))[1,2,]),main=paste("DCC Conditional Correlation","\n ret.ind.id","-","ret.ind.",colnames(base.name[i]),sep=""))
abline(v=as.POSIXct(as.Date("2007-02-27")))
abline(v=as.POSIXct(as.Date("2010-08-30")))
abline(v=as.POSIXct(as.Date("2010-02-02")))
	}
dev.off()

#############################################
#Testing for Contagion
#Mean Difference t-test
t.test.rcor.fin<-c()
t.test.rcor.fin<-data.frame(matrix(NA, nrow=length(c(1,3,4,8,9,10,15)), ncol=5))
for (j in 1:length(c(1,3,4,8,9,10,15)))
	{
		rownames(t.test.rcor.fin)[j]<-paste("ret.",colnames(base.fin[c(1,3,4,8,9,10,15)[j]]),sep="")
	}
colnames(t.test.rcor.fin)<-c("p.val1<2","p.val1<3","p.val2=3","p.val2<3","p.val2>3")
	
for (i in c(1,3,4,8,9,10,15))
{
temp.period1.fin<-(rcor(get(paste("dcc.fit.fin.id.",colnames(base.fin[i]),sep="")))[1,2,])[1:1082]
temp.period2.fin<-(rcor(get(paste("dcc.fit.fin.id.",colnames(base.fin[i]),sep="")))[1,2,])[1083:1997]
temp.period3.fin<-(rcor(get(paste("dcc.fit.fin.id.",colnames(base.fin[i]),sep="")))[1,2,])[1848:2718]
t.test.rcor.fin[paste("ret.",colnames(base.fin[i]),sep=""),"p.val1<2"]<-t.test(get(paste("temp.period1.fin")),get(paste("temp.period2.fin")),alternative = c("less"))$p.value
t.test.rcor.fin[paste("ret.",colnames(base.fin[i]),sep=""),"p.val1<3"]<-t.test(get(paste("temp.period1.fin")),get(paste("temp.period3.fin")),alternative = c("less"))$p.value
t.test.rcor.fin[paste("ret.",colnames(base.fin[i]),sep=""),"p.val2=3"]<-t.test(get(paste("temp.period2.fin")),get(paste("temp.period3.fin")))$p.value
t.test.rcor.fin[paste("ret.",colnames(base.fin[i]),sep=""),"p.val2<3"]<-t.test(get(paste("temp.period2.fin")),get(paste("temp.period3.fin")),,alternative = c("less"))$p.value
t.test.rcor.fin[paste("ret.",colnames(base.fin[i]),sep=""),"p.val2>3"]<- t.test(get(paste("temp.period2.fin")),get(paste("temp.period3.fin")),,alternative = c("greater"))$p.value
}

t.test.rcor.cg<-c()
t.test.rcor.cg<-data.frame(matrix(NA, nrow=length(c(1,3,4,8,9,10,15)), ncol=5))
for (j in 1:length(c(1,3,4,8,9,10,15)))
	{
		rownames(t.test.rcor.cg)[j]<-paste("ret.",colnames(base.cg[c(1,3,4,8,9,10,15)[j]]),sep="")
	}
colnames(t.test.rcor.cg)<-c("p.val1<2","p.val1<3","p.val2=3","p.val2<3","p.val2>3")
	
for (i in c(1,3,4,8,9,10,15))
{
temp.period1.cg<-(rcor(get(paste("dcc.fit.cg.id.",colnames(base.cg[i]),sep="")))[1,2,])[1:1082]
temp.period2.cg<-(rcor(get(paste("dcc.fit.cg.id.",colnames(base.cg[i]),sep="")))[1,2,])[1083:1997]
temp.period3.cg<-(rcor(get(paste("dcc.fit.cg.id.",colnames(base.cg[i]),sep="")))[1,2,])[1848:2718]
t.test.rcor.cg[paste("ret.",colnames(base.cg[i]),sep=""),"p.val1<2"]<-t.test(get(paste("temp.period1.cg")),get(paste("temp.period2.cg")),alternative = c("less"))$p.value
t.test.rcor.cg[paste("ret.",colnames(base.cg[i]),sep=""),"p.val1<3"]<-t.test(get(paste("temp.period1.cg")),get(paste("temp.period3.cg")),alternative = c("less"))$p.value
t.test.rcor.cg[paste("ret.",colnames(base.cg[i]),sep=""),"p.val2=3"]<-t.test(get(paste("temp.period2.cg")),get(paste("temp.period3.cg")))$p.value
t.test.rcor.cg[paste("ret.",colnames(base.cg[i]),sep=""),"p.val2<3"]<-t.test(get(paste("temp.period2.cg")),get(paste("temp.period3.cg")),,alternative = c("less"))$p.value
t.test.rcor.cg[paste("ret.",colnames(base.cg[i]),sep=""),"p.val2>3"]<- t.test(get(paste("temp.period2.cg")),get(paste("temp.period3.cg")),,alternative = c("greater"))$p.value
}

t.test.rcor.ind<-c()
t.test.rcor.ind<-data.frame(matrix(NA, nrow=length(c(1,3,4,8,9,10,15)), ncol=5))
for (j in 1:length(c(1,3,4,8,9,10,15)))
	{
		rownames(t.test.rcor.ind)[j]<-paste("ret.",colnames(base.ind[c(1,3,4,8,9,10,15)[j]]),sep="")
	}
colnames(t.test.rcor.ind)<-c("p.val1<2","p.val1<3","p.val2=3","p.val2<3","p.val2>3")
	
for (i in c(1,3,4,8,9,10,15))
{
temp.period1.ind<-(rcor(get(paste("dcc.fit.ind.id.",colnames(base.ind[i]),sep="")))[1,2,])[1:1082]
temp.period2.ind<-(rcor(get(paste("dcc.fit.ind.id.",colnames(base.ind[i]),sep="")))[1,2,])[1083:1997]
temp.period3.ind<-(rcor(get(paste("dcc.fit.ind.id.",colnames(base.ind[i]),sep="")))[1,2,])[1848:2718]
t.test.rcor.ind[paste("ret.",colnames(base.ind[i]),sep=""),"p.val1<2"]<-t.test(get(paste("temp.period1.ind")),get(paste("temp.period2.ind")),alternative = c("less"))$p.value
t.test.rcor.ind[paste("ret.",colnames(base.ind[i]),sep=""),"p.val1<3"]<-t.test(get(paste("temp.period1.ind")),get(paste("temp.period3.ind")),alternative = c("less"))$p.value
t.test.rcor.ind[paste("ret.",colnames(base.ind[i]),sep=""),"p.val2=3"]<-t.test(get(paste("temp.period2.ind")),get(paste("temp.period3.ind")))$p.value
t.test.rcor.ind[paste("ret.",colnames(base.ind[i]),sep=""),"p.val2<3"]<-t.test(get(paste("temp.period2.ind")),get(paste("temp.period3.ind")),,alternative = c("less"))$p.value
t.test.rcor.ind[paste("ret.",colnames(base.ind[i]),sep=""),"p.val2>3"]<- t.test(get(paste("temp.period2.ind")),get(paste("temp.period3.ind")),,alternative = c("greater"))$p.value
}

t.test.rcor.fin>0.05
t.test.rcor.cg>0.05
t.test.rcor.ind>0.05

# WILCOX TEST
wilcox.test.rcor.fin<-c()
wilcox.test.rcor.fin<-data.frame(matrix(NA, nrow=length(c(1,3,4,8,9,10,15)), ncol=3))
for (j in 1:length(c(1,3,4,8,9,10,15)))
	{
		rownames(wilcox.test.rcor.fin)[j]<-paste("ret.",colnames(base.fin[c(1,3,4,8,9,10,15)[j]]),sep="")
	}
colnames(wilcox.test.rcor.fin)<-c("p.val1=2","p.val1=3","p.val2=3")
	
for (i in c(1,3,4,8,9,10,15))
{
temp.period1.fin<-(rcor(get(paste("dcc.fit.fin.id.",colnames(base.fin[i]),sep="")))[1,2,])[1:1082]
temp.period2.fin<-(rcor(get(paste("dcc.fit.fin.id.",colnames(base.fin[i]),sep="")))[1,2,])[1083:1997]
temp.period3.fin<-(rcor(get(paste("dcc.fit.fin.id.",colnames(base.fin[i]),sep="")))[1,2,])[1848:2718]
wilcox.test.rcor.fin[paste("ret.",colnames(base.fin[i]),sep=""),"p.val1=2"]<-wilcox.test(get(paste("temp.period1.fin")),get(paste("temp.period2.fin")),conf.int=TRUE)$p.value

wilcox.test.rcor.fin[paste("ret.",colnames(base.fin[i]),sep=""),"p.val1=3"]<-wilcox.test(get(paste("temp.period1.fin")),get(paste("temp.period3.fin")),conf.int=TRUE)$p.value

wilcox.test.rcor.fin[paste("ret.",colnames(base.fin[i]),sep=""),"p.val2=3"]<-wilcox.test(get(paste("temp.period2.fin")),get(paste("temp.period3.fin")),conf.int=TRUE)$p.value
}

wilcox.test.rcor.cg<-c()
wilcox.test.rcor.cg<-data.frame(matrix(NA, nrow=length(c(1,3,4,8,9,10,15)), ncol=3))
for (j in 1:length(c(1,3,4,8,9,10,15)))
	{
		rownames(wilcox.test.rcor.cg)[j]<-paste("ret.",colnames(base.cg[c(1,3,4,8,9,10,15)[j]]),sep="")
	}
colnames(wilcox.test.rcor.cg)<-c("p.val1=2","p.val1=3","p.val2=3")
	
for (i in c(1,3,4,8,9,10,15))
{
temp.period1.cg<-(rcor(get(paste("dcc.fit.cg.id.",colnames(base.cg[i]),sep="")))[1,2,])[1:1082]
temp.period2.cg<-(rcor(get(paste("dcc.fit.cg.id.",colnames(base.cg[i]),sep="")))[1,2,])[1083:1997]
temp.period3.cg<-(rcor(get(paste("dcc.fit.cg.id.",colnames(base.cg[i]),sep="")))[1,2,])[1848:2718]
wilcox.test.rcor.cg[paste("ret.",colnames(base.cg[i]),sep=""),"p.val1=2"]<-wilcox.test(get(paste("temp.period1.cg")),get(paste("temp.period2.cg")),conf.int=TRUE)$p.value

wilcox.test.rcor.cg[paste("ret.",colnames(base.cg[i]),sep=""),"p.val1=3"]<-wilcox.test(get(paste("temp.period1.cg")),get(paste("temp.period3.cg")),conf.int=TRUE)$p.value

wilcox.test.rcor.cg[paste("ret.",colnames(base.cg[i]),sep=""),"p.val2=3"]<-wilcox.test(get(paste("temp.period2.cg")),get(paste("temp.period3.cg")),conf.int=TRUE)$p.value
}

wilcox.test.rcor.ind<-c()
wilcox.test.rcor.ind<-data.frame(matrix(NA, nrow=length(c(1,3,4,8,9,10,15)), ncol=3))
for (j in 1:length(c(1,3,4,8,9,10,15)))
	{
		rownames(wilcox.test.rcor.ind)[j]<-paste("ret.",colnames(base.ind[c(1,3,4,8,9,10,15)[j]]),sep="")
	}
colnames(wilcox.test.rcor.ind)<-c("p.val1=2","p.val1=3","p.val2=3")
	
for (i in c(1,3,4,8,9,10,15))
{
temp.period1.ind<-(rcor(get(paste("dcc.fit.ind.id.",colnames(base.ind[i]),sep="")))[1,2,])[1:1082]
temp.period2.ind<-(rcor(get(paste("dcc.fit.ind.id.",colnames(base.ind[i]),sep="")))[1,2,])[1083:1997]
temp.period3.ind<-(rcor(get(paste("dcc.fit.ind.id.",colnames(base.ind[i]),sep="")))[1,2,])[1848:2718]
wilcox.test.rcor.ind[paste("ret.",colnames(base.ind[i]),sep=""),"p.val1=2"]<-wilcox.test(get(paste("temp.period1.ind")),get(paste("temp.period2.ind")),conf.int=TRUE)$p.value

wilcox.test.rcor.ind[paste("ret.",colnames(base.ind[i]),sep=""),"p.val1=3"]<-wilcox.test(get(paste("temp.period1.ind")),get(paste("temp.period3.ind")),conf.int=TRUE)$p.value

wilcox.test.rcor.ind[paste("ret.",colnames(base.ind[i]),sep=""),"p.val2=3"]<-wilcox.test(get(paste("temp.period2.ind")),get(paste("temp.period3.ind")),conf.int=TRUE)$p.value
}

wilcox.test.rcor.fin
wilcox.test.rcor.cg
wilcox.test.rcor.ind

wilcox.test.rcor.fin>0.05
wilcox.test.rcor.cg>0.05
wilcox.test.rcor.ind>0.05
	
