# Jarque-Bera Test
# Testing Normality
jb.test.ret.fin<-c()
jb.test.ret.fin<-data.frame(jb.test.ret.fin)
jb.test.ret.fin<-jb.test(ret.fin)
colnames(jb.test.ret.fin)<-paste("ret.",colnames(base.fin),sep="")
jb.test.ret.fin

jb.test.ret.exch<-c()
jb.test.ret.exch<-data.frame(jb.test.ret.exch)
jb.test.ret.exch<-jb.test(ret.exch)
colnames(jb.test.ret.exch)<-paste("ret.",colnames(base.exch),sep="")
jb.test.ret.exch

jb.test.ret.int<-c()
jb.test.ret.int<-data.frame(jb.test.ret.int)
jb.test.ret.int<-jb.test(ret.int)
colnames(jb.test.ret.int)<-paste("ret.",colnames(base.int),sep="")
jb.test.ret.int

#################################################################################################################
# DECOMPOSITION
# assign series to ts class to be used in decomposition
for (i in 1:15)
	{
	assign(paste("ts.fin.",colnames(base.name[i]),sep=""),ts(data=get(paste("fin.",colnames(base.name[i]),sep="")),frequency=22))
	assign(paste("ts.cg.",colnames(base.name[i]),sep=""),ts(data=get(paste("cg.",colnames(base.name[i]),sep="")),frequency=22))
	assign(paste("ts.ind.",colnames(base.name[i]),sep=""),ts(data=get(paste("ind.",colnames(base.name[i]),sep="")),frequency=22))
	}

for (i in 1:11)
	{
	assign(paste("ts.",colnames(base.exch[i]),sep=""),ts(data=get(paste(colnames(base.exch[i]),sep="")),frequency=22))
	}
	
for (i in 1:10)
	{
	assign(paste("ts.int.",colnames(base.int[i]),sep=""),ts(data=get(paste("int.",colnames(base.int[i]),sep="")),frequency=22))
	}
	
for (i in 1:15)
	{
	assign(paste("ts.ret.fin.",colnames(base.name[i]),sep=""),ts(data=get(paste("ret.fin.",colnames(base.name[i]),sep="")),frequency=22))
	assign(paste("ts.ret.cg.",colnames(base.name[i]),sep=""),ts(data=get(paste("ret.cg.",colnames(base.name[i]),sep="")),frequency=22))
	assign(paste("ts.ret.ind.",colnames(base.name[i]),sep=""),ts(data=get(paste("ret.ind.",colnames(base.name[i]),sep="")),frequency=22))
	}
	
for (i in 1:11)
	{
	assign(paste("ts.ret.",colnames(base.exch[i]),sep=""),ts(data=get(paste("ret.",colnames(base.exch[i]),sep="")),frequency=22))
	}
	
for (i in 1:10)
	{
	assign(paste("ts.ret.",colnames(base.int[i]),sep=""),ts(data=get(paste("ret.",colnames(base.int[i]),sep="")),frequency=22))
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

pdf("base.int decomp.pdf",paper="a4r",width=11.69,height=8.27)
for (i in 1:10)
	{
	decomp.plot(decompose(get(paste("ts.",colnames(base.int[i]),sep="")),type = c("additive")),main=paste("Decomposition of additive time series","-",colnames(base.int[i]),sep=""))
	}
dev.off()

pdf("base.exch decomp.pdf",paper="a4r",width=11.69,height=8.27)
for (i in 1:11)
	{
	decomp.plot(decompose(get(paste("ts.",colnames(base.exch[i]),sep="")),type = c("additive")),main=paste("Decomposition of additive time series","-",colnames(base.exch[i]),sep=""))
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

pdf("ret.int decomp.pdf",paper="a4r",width=11.69,height=8.27)
for (i in 1:10)
	{
	decomp.plot(decompose(get(paste("ts.ret.",colnames(base.int[i]),sep="")),type = c("additive")),main=paste("Decomposition of additive time series","- ret.",colnames(base.int[i]),sep=""))
	}
dev.off()

pdf("ret.exch decomp.pdf",paper="a4r",width=11.69,height=8.27)
for (i in 1:11)
	{
	decomp.plot(decompose(get(paste("ts.ret.",colnames(base.exch[i]),sep="")),type = c("additive")),main=paste("Decomposition of additive time series","- ret.",colnames(base.exch[i]),sep=""))
	}
dev.off()

####################################################################################################################
# ACF & PACF
# plot acf and pacf
pdf("ret acf-pacf.pdf",paper="a4r",width=11.69,height=8.27)
	for(i in 1:15)
		{
		Acf(get(paste("ret.",colnames(base.name[i]),sep="")),main=paste("ret.",colnames(base.name[i]),sep=""))
		Acf(get(paste("ret.",colnames(base.name[i]),sep="")),main=paste("ret.",colnames(base.name[i]),sep=""))
		Acf(get(paste("ret.",colnames(base.name[i]),sep="")),main=paste("ret.",colnames(base.name[i]),sep=""))
		Pacf(get(paste("ret.",colnames(base.name[i]),sep="")),main=paste("ret.",colnames(base.name[i]),sep=""))
		Pacf(get(paste("ret.",colnames(base.name[i]),sep="")),main=paste("ret.",colnames(base.name[i]),sep=""))
		Pacf(get(paste("ret.",colnames(base.name[i]),sep="")),main=paste("ret.",colnames(base.name[i]),sep=""))
		}
dev.off()

pdf("ret.exch acf-pacf.pdf",paper="a4r",width=11.69,height=8.27)
	for(i in 1:11)
		{
		Acf(get(paste("ret.",colnames(base.exch[i]),sep="")),main=paste("ret.",colnames(base.exch[i]),sep=""))
		Pacf(get(paste("ret.",colnames(base.exch[i]),sep="")),main=paste("ret.",colnames(base.exch[i]),sep=""))
		}
dev.off()

pdf("ret.int acf-pacf.pdf",paper="a4r",width=11.69,height=8.27)
for(i in 1:10)
		{
		Acf(get(paste("ret.",colnames(base.int[i]),sep="")),main=paste("ret.",colnames(base.int[i]),sep=""))
		Pacf(get(paste("ret.",colnames(base.int[i]),sep="")),main=paste("ret.",colnames(base.int[i]),sep=""))
		}
dev.off()

##################################################################################################################
# DISTRIBUTION FIT
# distribution test of ret.
for (i in 1:15)
	{
	assign(paste("dist.ret.fin.",colnames(base.name[i]),sep=""),fitdistrplus::fitdist(as.numeric(get(paste("ret.fin.",colnames(base.name[i]),sep=""))), "norm"))
	assign(paste("dist.ret.cg.",colnames(base.name[i]),sep=""),fitdistrplus::fitdist(as.numeric(get(paste("ret.cg.",colnames(base.name[i]),sep=""))), "norm"))
	assign(paste("dist.ret.ind.",colnames(base.name[i]),sep=""),fitdistrplus::fitdist(as.numeric(get(paste("ret.ind.",colnames(base.name[i]),sep=""))), "norm"))
	}
	
for (i in 1:10)
	{
	assign(paste("dist.ret.",colnames(base.int[i]),sep=""),fitdistrplus::fitdist(as.numeric(get(paste("ret.",colnames(base.int[i]),sep=""))), "norm"))
	}

for (i in 1:11)
	{
	assign(paste("dist.ret.",colnames(base.exch[i]),sep=""),fitdistrplus::fitdist(as.numeric(get(paste("ret.",colnames(base.exch[i]),sep=""))),method=c("mme"),"norm"))
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

pdf("dist.int.pdf",paper="a4r",width=11.69,height=8.27)	
for(i in 1:10)
	{
	plot(get(paste("dist.ret.",colnames(base.int[i]),sep="")),sub=paste("ret.",colnames(base.int[i]),sep=""))
	}
dev.off()

pdf("dist.exch.pdf",paper="a4r",width=11.69,height=8.27)
for(i in 1:11)
	{
	plot(get(paste("dist.ret.",colnames(base.exch[i]),sep="")),sub=paste("ret.",colnames(base.exch[i]),sep=""))
	}
dev.off()
####################################################################################################################
# STATIONARITY TEST
# testing for stationarity: pp test
for(i in 1:15)
	{
	assign(paste("ur.pp.ret.fin.",colnames(base.name[i]),sep=""),urppTest(ts(get(paste("ret.fin.",colnames(base.name[i]),sep=""))),type="Z-tau"))
	assign(paste("ur.pp.ret.cg.",colnames(base.name[i]),sep=""),urppTest(ts(get(paste("ret.cg.",colnames(base.name[i]),sep=""))),type="Z-tau"))
	assign(paste("ur.pp.ret.ind.",colnames(base.name[i]),sep=""),urppTest(ts(get(paste("ret.ind.",colnames(base.name[i]),sep=""))),type="Z-tau"))
	}

for(i in 1:11)
	{
	assign(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""),urppTest(ts(get(paste("ret.",colnames(base.exch[i]),sep=""))),type="Z-tau"))
	}

for(i in 1:10)
	{
	assign(paste("ur.pp.ret.",colnames(base.int[i]),sep=""),urppTest(ts(get(paste("ret.",colnames(base.int[i]),sep=""))),type="Z-tau"))
	}		


# testing for stationarity: kpss test
for(i in 1:15)
	{
	assign(paste("ur.kpss.ret.fin.",colnames(base.name[i]),sep=""),urkpssTest(ts(get(paste("ret.fin.",colnames(base.name[i]),sep="")))))
	assign(paste("ur.kpss.ret.cg.",colnames(base.name[i]),sep=""),urkpssTest(ts(get(paste("ret.cg.",colnames(base.name[i]),sep="")))))
	assign(paste("ur.kpss.ret.ind.",colnames(base.name[i]),sep=""),urkpssTest(ts(get(paste("ret.ind.",colnames(base.name[i]),sep="")))))
	}

for(i in 1:11)
	{
	assign(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""),urkpssTest(ts(get(paste("ret.",colnames(base.exch[i]),sep="")))))
	}
	
for(i in 1:10)
	{
	assign(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""),urkpssTest(ts(get(paste("ret.",colnames(base.int[i]),sep="")))))
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

num.pp.ret.exch<-c()
num.pp.ret.exch<-data.frame(num.pp.ret.exch)
for (i in 1:11)	
	{
	num.pp.ret.exch["num.pp.ret.exch.1pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])
	num.pp.ret.exch["num.pp.ret.exch.1pct.cval",i]<-abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[1])
	num.pp.ret.exch["num.pp.ret.exch.5pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])
	num.pp.ret.exch["num.pp.ret.exch.5pct.cval",i]<-abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[2])
	num.pp.ret.exch["num.pp.ret.exch.10pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])
	num.pp.ret.exch["num.pp.ret.exch.10pct.cval",i]<-abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[3])
	}			
colnames(num.pp.ret.exch)<-paste("ret.",colnames(base.exch),sep="")

pp.ret.exch<-c()
pp.ret.exch<-data.frame(pp.ret.exch)
for (i in 1:11)	
	{
	pp.ret.exch["pp.ret.exch.1pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[1])
	pp.ret.exch["pp.ret.exch.5pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[2])
	pp.ret.exch["pp.ret.exch.10pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[3])
	}			
colnames(pp.ret.exch)<-paste("ret.",colnames(base.exch),sep="")

num.pp.ret.int<-c()
num.pp.ret.int<-data.frame(num.pp.ret.int)
for (i in 1:10)	
	{
	num.pp.ret.int["num.pp.ret.int.1pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])
	num.pp.ret.int["num.pp.ret.int.1pct.cval",i]<-abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@cval[1])
	num.pp.ret.int["num.pp.ret.int.5pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])
	num.pp.ret.int["num.pp.ret.int.5pct.cval",i]<-abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@cval[2])
	num.pp.ret.int["num.pp.ret.int.10pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])
	num.pp.ret.int["num.pp.ret.int.10pct.cval",i]<-abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@cval[3])
	}			
colnames(num.pp.ret.int)<-paste("ret.",colnames(base.int),sep="")

pp.ret.int<-c()
pp.ret.int<-data.frame(pp.ret.int)
for (i in 1:10)	
	{
	pp.ret.int["pp.ret.int.1pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@cval[1])
	pp.ret.int["pp.ret.int.5pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@cval[2])
	pp.ret.int["pp.ret.int.10pct",i]<-abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.pp.ret.",colnames(base.int[i]),sep=""))@test$test@cval[3])
	}			
colnames(pp.ret.int)<-paste("ret.",colnames(base.int),sep="")

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

num.kpss.ret.exch<-c()
num.kpss.ret.exch<-data.frame(num.kpss.ret.exch)
for (i in 1:11)	
	{
	num.kpss.ret.exch["num.kpss.ret.exch.1pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret.exch["num.kpss.ret.exch.1pct.cval",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[1])
	num.kpss.ret.exch["num.kpss.ret.exch.5pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret.exch["num.kpss.ret.exch.5pct.cval",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[2])
	num.kpss.ret.exch["num.kpss.ret.exch.10pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret.exch["num.kpss.ret.exch.10pct.cval",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[3])
	}			
colnames(num.kpss.ret.exch)<-paste("ret.",colnames(base.exch),sep="")

kpss.ret.exch<-c()
kpss.ret.exch<-data.frame(kpss.ret.exch)
for (i in 1:11)	
	{
	kpss.ret.exch["kpss.ret.exch.1pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[1])
	kpss.ret.exch["kpss.ret.exch.5pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[2])
	kpss.ret.exch["kpss.ret.exch.10pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[3])
	}			
colnames(kpss.ret.exch)<-paste("ret.",colnames(base.exch),sep="")

num.kpss.ret.int<-c()
num.kpss.ret.int<-data.frame(num.kpss.ret.int)
for (i in 1:10)	
	{
	num.kpss.ret.int["num.kpss.ret.int.1pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret.int["num.kpss.ret.int.1pct.cval",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@cval[1])
	num.kpss.ret.int["num.kpss.ret.int.5pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret.int["num.kpss.ret.int.5pct.cval",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@cval[2])
	num.kpss.ret.int["num.kpss.ret.int.10pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])
	num.kpss.ret.int["num.kpss.ret.int.10pct.cval",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@cval[3])
	}			
colnames(num.kpss.ret.int)<-paste("ret.",colnames(base.int),sep="")

kpss.ret.int<-c()
kpss.ret.int<-data.frame(kpss.ret.int)
for (i in 1:10)	
	{
	kpss.ret.int["kpss.ret.int.1pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@cval[1])
	kpss.ret.int["kpss.ret.int.5pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@cval[2])
	kpss.ret.int["kpss.ret.int.10pct",i]<-abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.kpss.ret.",colnames(base.int[i]),sep=""))@test$test@cval[3])
	}			
colnames(kpss.ret.int)<-paste("ret.",colnames(base.int),sep="")

#################################################################################################################
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

arima.ret.exch<-c()
arima.ret.exch<-data.frame(arima.ret.exch)
for (i in 1:11)
	{
	arima.ret.exch[paste(colnames(ret.exch[,i])),"ARp"]<-get(paste("arima.",colnames(ret.exch[,i]),sep=""))$arma[1]
	arima.ret.exch[paste(colnames(ret.exch[,i])),"MAq"]<-get(paste("arima.",colnames(ret.exch[,i]),sep=""))$arma[2]
	}
	
arima.ret.int<-c()
arima.ret.int<-data.frame(arima.ret.int)
for (i in 1:10)
	{
	arima.ret.int[paste(colnames(ret.int[,i])),"ARp"]<-get(paste("arima.",colnames(ret.int[,i]),sep=""))$arma[1]
	arima.ret.int[paste(colnames(ret.int[,i])),"MAq"]<-get(paste("arima.",colnames(ret.int[,i]),sep=""))$arma[2]
	}
		

#################################################################################################################
# ARCH/GARCH TEST
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
	

for (i in 1:10)
	{
	assign(paste("arch.lm.ret.",colnames(base.int[i]),sep=""), ArchTest(get(paste("resid.ret.",colnames(base.int[i]),sep="")), lags=1))
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
for (i in 1:11)
	{
	num.arch.lm.ret.exch[paste("ret.",colnames(base.exch[i]),sep=""),"pval"]<-get(paste("arch.lm.ret.",colnames(base.exch[i]),sep=""))$p.value[1]
	}

arch.lm.ret.exch<-num.arch.lm.ret.exch>0.1

num.arch.lm.ret.int<-c()
num.arch.lm.ret.int<-data.frame(num.arch.lm.ret.int)
for (i in 1:10)
	{
	num.arch.lm.ret.int[paste("ret.",colnames(base.int[i]),sep=""),"pval"]<-get(paste("arch.lm.ret.",colnames(base.int[i]),sep=""))$p.value[1]
	}

arch.lm.ret.int<-num.arch.lm.ret.int>0.1
	
#################################################################################################################	
# Persistence of GARCH model
persistence.ret.fin<-c()
persistence.ret.fin<-data.frame(persistence.ret.fin)
for (i in 1:15)
	{
	persistence.ret.fin[colnames(ret.fin[,i]),"alpha"]<-coef(get(paste("ugarch.",colnames(ret.fin[,i]),sep="")))[arima.ret.fin[i,1]+arima.ret.fin[i,2]+3]
	persistence.ret.fin[colnames(ret.fin[,i]),"beta"]<-coef(get(paste("ugarch.",colnames(ret.fin[,i]),sep="")))[arima.ret.fin[i,1]+arima.ret.fin[i,2]+4]	
	persistence.ret.fin[colnames(ret.fin[,i]),"persistence"]<-persistence(get(paste("ugarch.",colnames(ret.fin[,i]),sep="")))	
	}

persistence.ret.cg<-c()
persistence.ret.cg<-data.frame(persistence.ret.cg)
for (i in 1:15)
	{
	persistence.ret.cg[colnames(ret.cg[,i]),"alpha"]<-coef(get(paste("ugarch.",colnames(ret.cg[,i]),sep="")))[arima.ret.cg[i,1]+arima.ret.cg[i,2]+3]
	persistence.ret.cg[colnames(ret.cg[,i]),"beta"]<-coef(get(paste("ugarch.",colnames(ret.cg[,i]),sep="")))[arima.ret.cg[i,1]+arima.ret.cg[i,2]+4]	
	persistence.ret.cg[colnames(ret.cg[,i]),"persistence"]<-persistence(get(paste("ugarch.",colnames(ret.cg[,i]),sep="")))	
	}
	
persistence.ret.ind<-c()
persistence.ret.ind<-data.frame(persistence.ret.ind)
for (i in 1:15)
	{
	persistence.ret.ind[colnames(ret.ind[,i]),"alpha"]<-coef(get(paste("ugarch.",colnames(ret.ind[,i]),sep="")))[arima.ret.ind[i,1]+arima.ret.ind[i,2]+3]
	persistence.ret.ind[colnames(ret.ind[,i]),"beta"]<-coef(get(paste("ugarch.",colnames(ret.ind[,i]),sep="")))[arima.ret.ind[i,1]+arima.ret.ind[i,2]+4]	
	persistence.ret.ind[colnames(ret.ind[,i]),"persistence"]<-persistence(get(paste("ugarch.",colnames(ret.ind[,i]),sep="")))	
	}
	
persistence.ret.exch<-c()
persistence.ret.exch<-data.frame(persistence.ret.exch)
for (i in 1:11)
	{
	persistence.ret.exch[colnames(ret.exch[,i]),"alpha"]<-coef(get(paste("ugarch.",colnames(ret.exch[,i]),sep="")))[arima.ret.exch[i,1]+arima.ret.exch[i,2]+3]
	persistence.ret.exch[colnames(ret.exch[,i]),"beta"]<-coef(get(paste("ugarch.",colnames(ret.exch[,i]),sep="")))[arima.ret.exch[i,1]+arima.ret.exch[i,2]+4]	
	persistence.ret.exch[colnames(ret.exch[,i]),"persistence"]<-persistence(get(paste("ugarch.",colnames(ret.exch[,i]),sep="")))	
	}

persistence.ret.int<-c()
persistence.ret.int<-data.frame(persistence.ret.int)
for (i in 1:10)
	{
	persistence.ret.int[colnames(ret.int[,i]),"alpha"]<-coef(get(paste("ugarch.",colnames(ret.int[,i]),sep="")))[arima.ret.int[i,1]+arima.ret.int[i,2]+3]
	persistence.ret.int[colnames(ret.int[,i]),"beta"]<-coef(get(paste("ugarch.",colnames(ret.int[,i]),sep="")))[arima.ret.int[i,1]+arima.ret.int[i,2]+4]	
	persistence.ret.int[colnames(ret.int[,i]),"persistence"]<-persistence(get(paste("ugarch.",colnames(ret.int[,i]),sep="")))	
	}
	
#################################################################################################################	
# NEWS IMPACT CURVE of Univariate GARCH Models
	
#news impact curve
ugarch.ret.fin.zx<- matrix(nrow = 100)
ugarch.ret.fin.zx<-data.frame(ugarch.ret.fin.zx)
for (i in 1:15)
	{
	ugarch.ret.fin.zx[,colnames(ret.fin[,i])]<-data.frame(newsimpact(get(paste("ugarch.",colnames(ret.fin[,i]),sep="")))$zx)
	}
ugarch.ret.fin.zx<-ugarch.ret.fin.zx[-1]
xnewsimpact<-ugarch.ret.fin.zx[,1]


ugarch.ret.fin.zy<- matrix(nrow = 100)
ugarch.ret.fin.zy<-data.frame(ugarch.ret.fin.zy)
for (i in 1:15)
	{
	ugarch.ret.fin.zy[,colnames(ret.fin[,i])]<-data.frame(newsimpact(get(paste("ugarch.",colnames(ret.fin[,i]),sep="")))$zy)
	}
ugarch.ret.fin.zy<-ugarch.ret.fin.zy[-1]
	
ugarch.ret.cg.zy<- matrix(nrow = 100)
ugarch.ret.cg.zy<-data.frame(ugarch.ret.cg.zy)
for (i in 1:15)
	{
	ugarch.ret.cg.zy[,colnames(ret.cg[,i])]<-data.frame(newsimpact(get(paste("ugarch.",colnames(ret.cg[,i]),sep="")))$zy)
	}
ugarch.ret.cg.zy<-ugarch.ret.cg.zy[-1]

ugarch.ret.ind.zy<- matrix(nrow = 100)
ugarch.ret.ind.zy<-data.frame(ugarch.ret.ind.zy)
for (i in 1:15)
	{
	ugarch.ret.ind.zy[,colnames(ret.ind[,i])]<-data.frame(newsimpact(get(paste("ugarch.",colnames(ret.ind[,i]),sep="")))$zy)
	}
ugarch.ret.ind.zy<-ugarch.ret.ind.zy[-1]

ugarch.ret.exch.zy<- matrix(nrow = 100)
ugarch.ret.exch.zy<-data.frame(ugarch.ret.exch.zy)
for (i in 1:10)
	{
	ugarch.ret.exch.zy[,colnames(ret.exch[,i])]<-data.frame(newsimpact(get(paste("ugarch.",colnames(ret.exch[,i]),sep="")))$zy)
	}
ugarch.ret.exch.zy<-ugarch.ret.exch.zy[-1]
	
# plot news impact curve
pdf("news impact.pdf",paper="a4r",width=11.69,height=8.27)

ggplot(melt(cbind(xnewsimpact,ugarch.ret.fin.zy),id="xnewsimpact"),aes(x=xnewsimpact,y=value,colour=variable,group=variable))+geom_line()+xlab(expression(z[t - 1])) +  ylab(expression(sigma[t]^2)) +ggtitle("News Impact Curve")

ggplot(melt(cbind(xnewsimpact,ugarch.ret.cg.zy),id="xnewsimpact"),aes(x=xnewsimpact,y=value,colour=variable,group=variable))+geom_line()+xlab(expression(z[t - 1])) +  ylab(expression(sigma[t]^2)) +ggtitle("News Impact Curve")

ggplot(melt(cbind(xnewsimpact,ugarch.ret.ind.zy),id="xnewsimpact"),aes(x=xnewsimpact,y=value,colour=variable,group=variable))+geom_line()+xlab(expression(z[t - 1])) +  ylab(expression(sigma[t]^2)) +ggtitle("News Impact Curve")

ggplot(melt(cbind(xnewsimpact,ugarch.ret.exch.zy),id="xnewsimpact"),aes(x=xnewsimpact,y=value,colour=variable,group=variable))+geom_line()+xlab(expression(z[t - 1])) +  ylab(expression(sigma[t]^2)) +ggtitle("News Impact Curve")

ggplot(melt(cbind(xnewsimpact,ugarch.ret.int.zy),id="xnewsimpact"),aes(x=xnewsimpact,y=value,colour=variable,group=variable))+geom_line()+xlab(expression(z[t - 1])) +  ylab(expression(sigma[t]^2)) +ggtitle("News Impact Curve")

dev.off()	

#################################################################################################################
# CONDITIONAL CORRELATION TEST
# dcc test for conditional correlation
dcc.test.ret.fin<-DCCtest(set.data.dcc.fit.ret.fin[1:(dim(set.data.dcc.fit.ret.fin)[1]-1),],solver="gosolnp")
dcc.test.ret.cg<-DCCtest(set.data.dcc.fit.ret.cg[1:(dim(set.data.dcc.fit.ret.fin)[1]-1),],solver="gosolnp")
dcc.test.ret.ind<-DCCtest(set.data.dcc.fit.ret.ind[1:(dim(set.data.dcc.fit.ret.fin)[1]-1),],solver="gosolnp")
dcc.test.ret.exch<-DCCtest(set.data.dcc.fit.ret.exch[1:(dim(set.data.dcc.fit.ret.fin)[1]-1),],solver="gosolnp")
dcc.test.ret.int<-DCCtest(set.data.dcc.fit.ret.int[1:(dim(set.data.dcc.fit.ret.int)[1]-1),],solver="gosolnp")

dcc.test.ret.fin$p.value
dcc.test.ret.cg$p.value
dcc.test.ret.ind$p.value
dcc.test.ret.exch$p.value
dcc.test.ret.int$p.value

dcc.test.ret.fin$p.value>0.05
dcc.test.ret.cg$p.value>0.05
dcc.test.ret.ind$p.value>0.05
dcc.test.ret.exch$p.value>0.05
dcc.test.ret.int$p.value>0.05

# dcc test for regional conditional correlation
#west
dcc.test.west.fin<-DCCtest(set.data.dcc.fit.ret.fin[1:(dim(set.data.dcc.fit.ret.fin)[1]-1),c(2,3,4,7,12,14,15)],solver="gosolnp")
dcc.test.west.cg<-DCCtest(set.data.dcc.fit.ret.cg[1:(dim(set.data.dcc.fit.ret.cg)[1]-1),c(2,3,4,7,12,14,15)],solver="gosolnp")
dcc.test.west.ind<-DCCtest(set.data.dcc.fit.ret.ind[1:(dim(set.data.dcc.fit.ret.ind)[1]-1),c(2,3,4,7,12,14,15)],solver="gosolnp")
dcc.test.west.exch<-DCCtest(set.data.dcc.fit.ret.exch[1:(dim(set.data.dcc.fit.ret.fin)[1]-1),c(2,10)],solver="gosolnp")
dcc.test.west.int<-DCCtest(set.data.dcc.fit.ret.int[1:(dim(set.data.dcc.fit.ret.int)[1]-1),c(2,10)],solver="gosolnp")

#aspac
dcc.test.aspac.fin<-DCCtest(set.data.dcc.fit.ret.fin[1:(dim(set.data.dcc.fit.ret.fin)[1]-1),c(1,5,6,8,9,10,11,13,15)],solver="gosolnp")
dcc.test.aspac.cg<-DCCtest(set.data.dcc.fit.ret.cg[1:(dim(set.data.dcc.fit.ret.cg)[1]-1),c(1,5,6,8,9,10,11,13,15)],solver="gosolnp")
dcc.test.aspac.ind<-DCCtest(set.data.dcc.fit.ret.ind[1:(dim(set.data.dcc.fit.ret.ind)[1]-1),c(1,5,6,8,9,10,11,13,15)],solver="gosolnp")
dcc.test.aspac.exch<-DCCtest(set.data.dcc.fit.ret.exch[1:(dim(set.data.dcc.fit.ret.exch)[1]-1),c(1,3,4,5,6,7,8,9)],solver="gosolnp")
dcc.test.aspac.int<-DCCtest(set.data.dcc.fit.ret.int[1:(dim(set.data.dcc.fit.ret.int)[1]-1),c(1,3,4,5,6,7,8,9)],solver="gosolnp")


#west
dcc.test.west.fin$p.value
dcc.test.west.cg$p.value
dcc.test.west.ind$p.value
dcc.test.west.exch$p.value
dcc.test.west.int$p.value

#aspac
dcc.test.aspac.fin$p.value
dcc.test.aspac.cg$p.value
dcc.test.aspac.ind$p.value
dcc.test.aspac.exch$p.value
dcc.test.aspac.int$p.value

#west
dcc.test.west.fin$p.value>0.05
dcc.test.west.cg$p.value>0.05
dcc.test.west.ind$p.value>0.05
dcc.test.west.exch$p.value>0.05
dcc.test.west.int$p.value>0.05

#aspac
dcc.test.aspac.fin$p.value>0.05
dcc.test.aspac.cg$p.value>0.05
dcc.test.aspac.ind$p.value>0.05
dcc.test.aspac.exch$p.value>0.05
dcc.test.aspac.int$p.value>0.05

#################################################################################################################



network.analysis <- function(x)
{
infocent.table<-data.frame(infocent(x),colnames(x))
if (exists("infocent.table")==TRUE){colnames(infocent.table)<-c("infocent","colnames")}
# if (exists("infocent.table")==TRUE){rownames(infocent.table)<-NULL}
infocent.table<-sort(infocent.table)
assign(paste("infocent.",deparse(substitute(x)),sep=""),infocent.table,envir = .GlobalEnv)

stresscent.table<-data.frame(stresscent(x),colnames(x))
if (exists("stresscent.table")==TRUE){colnames(stresscent.table)<-c("stresscent","colnames")}
# if (exists("stresscent.table")==TRUE){rownames(stresscent.table)<-NULL}
stresscent.table<-sort(stresscent.table)
assign(paste("stresscent.",deparse(substitute(x)),sep=""),stresscent.table,envir = .GlobalEnv)

bonpow.table<-data.frame(bonpow(x),colnames(x))
if (exists("bonpow.table")==TRUE){colnames(bonpow.table)<-c("bonpow","colnames")}
if (exists("bonpow.table")==TRUE){rownames(bonpow.table)<-c(1:dim(bonpow.table)[1])}
bonpow.table<-sort(bonpow.table)
assign(paste("bonpow.",deparse(substitute(x)),sep=""),bonpow.table,envir = .GlobalEnv)

print(paste("information centrality - max: ",colnames(x)[which(infocent(x)==max(infocent(x)))]))
print(paste("information centrality - min: ",colnames(x)[which(infocent(x)==min(infocent(x)))]))
print(paste("stress centrality - max: ",colnames(x)[which(stresscent(x)==max(stresscent(x)))]))
print(paste("stress centrality - min: ",colnames(x)[which(stresscent(x)==min(stresscent(x)))]))
print(paste("bonacich power - max: ",colnames(x)[which(bonpow(x)==max(bonpow(x)))]))
print(paste("bonacich power - min: ",colnames(x)[which(bonpow(x)==min(bonpow(x)))]))
print(paste("bonacich power - abs.max: ",colnames(x)[which(abs(bonpow(x))==max(abs(bonpow(x))))]))
print(paste("bonacich power - abs.min: ",colnames(x)[which(abs(bonpow(x))==min(abs(bonpow(x))))]))

print("infocent.table")
print(infocent.table)

print("stresscent.table")
print(stresscent.table)

print("bonpow.table")
print(bonpow.table)
}

network.analysis(net.bi.granger.ret.fin)
network.analysis(net.bi.granger.ret.cg)	
network.analysis(net.bi.granger.ret.ind)
network.analysis(net.bi.granger.ret.exch)
network.analysis(net.bi.granger.ret.int)

network.analysis(bi.granger.inter.fin)
network.analysis(bi.granger.inter.cg)
network.analysis(bi.granger.inter.ind)

network.analysis(bi.granger.west.fin)
network.analysis(bi.granger.west.cg)
network.analysis(bi.granger.west.ind)
network.analysis(bi.granger.west.exch)
network.analysis(bi.granger.west.int)

network.analysis(bi.granger.aspac.fin)
network.analysis(bi.granger.aspac.cg)
network.analysis(bi.granger.aspac.ind)
network.analysis(bi.granger.aspac.exch)

network.analysis(bi.granger.across)
network.analysis(bi.granger.across.west)
network.analysis(bi.granger.across.aspac)

#################################################################################################################	
# CONDITIONAL COVARIANCE
pdf("cond cov.pdf",paper="a4r",width=11.69,height=8.27)
for (i in 1:15)
	{
		for (j in 1:15)
		{
			plot(as.xts(rcov(dcc.fit.ret.fin)[i,j,1:2718]),main=paste("DCC Conditional Covariance","\n ret.fin.",colnames(base.name[i]),"-","ret.fin.",colnames(base.name[j]),sep=""))
			plot(as.xts(rcov(dcc.fit.ret.cg)[i,j,1:2718]),main=paste("DCC Conditional Covariance","\n ret.cg.",colnames(base.name[i]),"-","ret.cg.",colnames(base.name[j]),sep=""))
			plot(as.xts(rcov(dcc.fit.ret.ind)[i,j,1:2718]),main=paste("DCC Conditional Covariance","\n ret.ind.",colnames(base.name[i]),"-","ret.ind.",colnames(base.name[j]),sep=""))
		}
	}
dev.off()

#################################################################################################################
# CONDITIONAL CORRELATION
pdf("cond corr.pdf",paper="a4r",width=11.69,height=8.27)
for (i in 1:15)
{
    for (j in 1:15)
    {         
        plot(as.xts(rcor(dcc.fit.ret.fin)[i,j,1:2718]),main=paste("DCC Conditional Correlation","\n ret.fin.",colnames(base.name[i]),"-","ret.fin.",colnames(base.name[j]),sep=""))   
		plot(as.xts(rcor(dcc.fit.ret.cg)[i,j,1:2718]),main=paste("DCC Conditional Correlation","\n ret.cg.",colnames(base.name[i]),"-","ret.cg.",colnames(base.name[j]),sep=""))   
		plot(as.xts(rcor(dcc.fit.ret.ind)[i,j,1:2718]),main=paste("DCC Conditional Correlation","\n ret.ind.",colnames(base.name[i]),"-","ret.ind.",colnames(base.name[j]),sep=""))   
   }
}
dev.off()

pdf("cond corr.pdf",paper="a4r",width=11.69,height=8.27)
for (i in 1:15)
{
    for (j in 1:15)
    {         
        plot(as.xts(rcor(dcc.fit.across)[i,j,1:2718]),main=paste("DCC Conditional Correlation","\n ret.fin.",colnames(set.data.dcc.fit.across[,i]),"-","ret.fin.",colnames(set.data.dcc.fit.across[,j]),sep="")) 
	}
}
dev.off()
#################################################################################
# DCC forecast
forecast.dcc.fit.ret.fin<-dccforecast(dcc.fit.ret.fin,n.ahead=365)
forecast.dcc.fit.ret.cg<-dccforecast(dcc.fit.ret.cg,n.ahead=365)
forecast.dcc.fit.ret.ind<-dccforecast(dcc.fit.ret.ind,n.ahead=365)

pdf("forecast.pdf",paper="a4r",width=23.7,height=14)
plot(forecast.dcc.fit.ret.fin,which=2, series=c(1:15))
plot(forecast.dcc.fit.ret.cg,which=2, series=c(1:15))
plot(forecast.dcc.fit.ret.ind,which=2, series=c(1:15))
dev.off()
#################################################################################
# Structural Break Detection
for (i in 1:45)
	{
	for (j in 1:45)
		{         
		assign(paste("rcor.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep=""),rcor(dcc.fit.across)[i,j,1:2718])
		assign(paste("rcov.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep=""),rcov(dcc.fit.across)[i,j,1:2718])
		}
	}
	

for (i in 1:45)
	{
	for (j in 1:45)
		{         
		assign(paste("ts.rcor.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep=""),na.omit(decompose(ts(data=xts(get(paste("rcor.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep="")),as.Date(data.frame(rownames(data.frame(get(paste("rcor.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep="")))))[,1])),frequency=22))$random))
		assign(paste("ts.rcov.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep=""),na.omit(decompose(ts(data=xts(get(paste("rcov.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep="")),as.Date(data.frame(rownames(data.frame(get(paste("rcov.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep="")))))[,1])),frequency=22))$random))
		}
	}

for (i in 1:45)
	{
	for (j in 1:45)
		{      
	assign(paste("ts.rcor.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep=""),xts(get(paste("ts.rcor.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep="")),as.Date(data.frame(rownames(data.frame(get(paste("rcor.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep="")))))[12:2707,1])))
	assign(paste("ts.rcov.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep=""),xts(get(paste("ts.rcov.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep="")),as.Date(data.frame(rownames(data.frame(get(paste("rcov.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep="")))))[12:2707,1])))
		}
	}
	
# STRUCTURAL CHANGE
# determine the detection times and change points for return 
# note that ARL0=1/alpha where alpha is false alarm probability
	for (i in 1:45)
	{
	for (j in 1:45)
		{
		assign(paste("cp.rcor.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep=""),processStream(get(paste("ts.rcor.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep="")),"Kolmogorov-Smirnov",ARL0=10000,startup=20,lambda=NA))
		assign(paste("cp.rcov.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep=""),processStream(get(paste("ts.rcov.",colnames(residuals(dcc.fit.across)[,i]),"_",colnames(residuals(dcc.fit.across)[,j]),sep="")),"Kolmogorov-Smirnov",ARL0=10000,startup=20,lambda=NA))
		}
	}
	
	
	ilist<-c(which(colnames(residuals(dcc.fit.across))=="ret.fin.gre"),which(colnames(residuals(dcc.fit.across))=="ret.fin.us"),which(colnames(residuals(dcc.fit.across))=="ret.fin.us"),which(colnames(residuals(dcc.fit.across))=="ret.ind.chn"),which(colnames(residuals(dcc.fit.across))=="ret.ind.jp"))
	
	jlist<-c(which(colnames(residuals(dcc.fit.across))=="ret.fin.ger"),which(colnames(residuals(dcc.fit.across))=="ret.fin.ger"),which(colnames(residuals(dcc.fit.across))=="ret.fin.id"),which(colnames(residuals(dcc.fit.across))=="ret.ind.id"),which(colnames(residuals(dcc.fit.across))=="ret.ind.id"))
	
	pdf("structural change.pdf",paper="a4r",width=11.69,height=8.27)
for (i in 1:dim(as.matrix(ilist))[1])
	{
	for (j in 1:dim(as.matrix(jlist))[1])
		{
		if (i==j)
			{
		plot.xts(get(paste("ts.rcor.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep="")),type='l',
		ylab=paste("ts.rcor.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep=""),
		xlab=paste("ts.rcor.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep=""),
		main=paste("ts.rcor.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep=""));
		for (dt in (get(paste("cp.rcor.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep="")))$detectionTimes)
			{abline(v=as.POSIXct(as.Date(index(get(paste("ts.rcor.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep="")))[dt])),col="BLUE")};
		for (cp in (get(paste("cp.rcor.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep="")))$changePoints)
			{abline(v=as.POSIXct(as.Date(index(get(paste("ts.rcor.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep="")))[cp])),lty=2,col="RED")};
		
		plot.xts(get(paste("ts.rcov.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep="")),type='l',
		ylab=paste("ts.rcov.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep=""),
		xlab=paste("ts.rcov.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep=""),
		main=paste("ts.rcov.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep=""));
		for (dt in (get(paste("cp.rcov.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep="")))$detectionTimes)
			{abline(v=as.POSIXct(as.Date(index(get(paste("ts.rcov.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep="")))[dt])),col="BLUE")};
		for (cp in (get(paste("cp.rcov.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep="")))$changePoints)
			{abline(v=as.POSIXct(as.Date(index(get(paste("ts.rcov.",colnames(residuals(dcc.fit.across)[,ilist[i]]),"_",colnames(residuals(dcc.fit.across)[,jlist[j]]),sep="")))[cp])),lty=2,col="RED")};
			}
		}
	}
dev.off()
