################################################################################################
# Persistence of GARCH model
persistence.ret.fin<-c()
persistence.ret.fin<-data.frame(persistence.ret.fin)
for (i in 1:15)
	{
	persistence.ret.fin[colnames(ret.fin[,i]),"alpha"]<-coef(get(paste("infocrit.ret.fin.",colnames(ret.fin[,i]),sep="")))[arima.ret.fin[i,1]+arima.ret.fin[i,2]+3]
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

data.frame(dcc.fit.ret.fin@mfit$coef)

	# fin 4
	# cg 4, 12
	
#DCC EXCH
mspec.ret.exch<-c()
for(i in c(1:10))
	{
	mspec.ret.exch<-c(mspec.ret.exch,get(paste("spec.ugarch.ret.",colnames(base.exch[i]),sep="")))
	}
mspec.ret.exch=multispec(mspec.ret.exch)

assign(paste("dcc.spec.ret.exch"),dccspec(get(paste("mspec.ret.exch")), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL, robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), dccOrder = c(1,1), model = c("DCC"), groups = rep(1, length(uspec@spec)), distribution = c("mvnorm"), start.pars = list(), fixed.pars = list()))

merge.list.data.dcc.fit.ret.exch<-as.character(list())
for (i in c(1:10))
{
merge.list.data.dcc.fit.ret.exch<-append(merge.list.data.dcc.fit.ret.exch,paste("resid.ret.",colnames(base.exch[i]),sep=""))
}
merge.name.data.dcc.fit.ret.exch<-lapply(merge.list.data.dcc.fit.ret.exch,get)
names(merge.name.data.dcc.fit.ret.exch)<-merge.list.data.dcc.fit.ret.exch
data.dcc.fit.ret.exch<-do.call(merge,merge.name.data.dcc.fit.ret.exch)

if (exists("dcc.fit.adj.ret.exch")==TRUE)
{
if(dcc.fit.adj.ret.exch>=0)
	{
	set.data.dcc.fit.ret.exch<-data.dcc.fit.ret.exch[-c(1:dcc.fit.adj.ret.exch),]
	}
else
	{
	set.data.dcc.fit.ret.exch<-data.dcc.fit.ret.exch
	}
}

if (exists("dcc.fit.adj.ret.exch")==FALSE)
{
set.data.dcc.fit.ret.exch<-data.dcc.fit.ret.exch
}

dcc.fit.ret.exch<-dccfit(dcc.spec.ret.exch, set.data.dcc.fit.ret.exch, out.sample = 30, solver = "gosolnp", solver.control = list(),fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), cluster = NULL, fit = NULL, VAR.fit = NULL)

#DCC SEC
# specificy mspec to be used in dccspec
mspec.ret.fin<-c()
mspec.ret.cg<-c()
mspec.ret.ind<-c()
for(i in 1:15)
	{
	mspec.ret.fin<-c(mspec.ret.fin,get(paste("spec.ugarch.ret.fin.",colnames(base.name[i]),sep="")))
	mspec.ret.cg<-c(mspec.ret.cg,get(paste("spec.ugarch.ret.cg.",colnames(base.name[i]),sep="")))
	mspec.ret.ind<-c(mspec.ret.ind,get(paste("spec.ugarch.ret.ind.",colnames(base.name[i]),sep="")))
	assign(paste("mspec.ret.",colnames(base.name[i]),sep=""),c(get(paste("spec.ugarch.ret.fin.",colnames(base.name[i]),sep="")),get(paste("spec.ugarch.ret.cg.",colnames(base.name[i]),sep="")),get(paste("spec.ugarch.ret.ind.",colnames(base.name[i]),sep=""))))
	assign(paste("mspec.ret.",colnames(base.name[i]),sep=""),multispec(get(paste("mspec.ret.",colnames(base.name[i]),sep=""))))
	}
mspec.ret.fin=multispec(mspec.ret.fin)
mspec.ret.cg=multispec(mspec.ret.cg)
mspec.ret.ind=multispec(mspec.ret.ind)

# specify dccspec
assign(paste("dcc.spec.ret.fin"),dccspec(get(paste("mspec.ret.fin")), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL, robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), dccOrder = c(1,1), model = c("DCC"), groups = rep(1, length(uspec@spec)), distribution = c("mvnorm"), start.pars = list(), fixed.pars = list()))

assign(paste("dcc.spec.ret.cg"),dccspec(get(paste("mspec.ret.cg")), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL, robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), dccOrder = c(1,1), model = c("DCC"), groups = rep(1, length(uspec@spec)), distribution = c("mvnorm"), start.pars = list(), fixed.pars = list()))

assign(paste("dcc.spec.ret.ind"),dccspec(get(paste("mspec.ret.ind")), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL, robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), dccOrder = c(1,1), model = c("DCC"), groups = rep(1, length(uspec@spec)), distribution = c("mvnorm"), start.pars = list(), fixed.pars = list()))

# specify data for dcc.fit
merge.list.data.dcc.fit.ret.fin<-as.character(list())
for (i in 1:15)
{
merge.list.data.dcc.fit.ret.fin<-append(merge.list.data.dcc.fit.ret.fin,paste("resid.ret.fin.",colnames(base.name[i]),sep=""))
}
merge.name.data.dcc.fit.ret.fin<-lapply(merge.list.data.dcc.fit.ret.fin,get)
names(merge.name.data.dcc.fit.ret.fin)<-merge.list.data.dcc.fit.ret.fin
data.dcc.fit.ret.fin<-do.call(merge,merge.name.data.dcc.fit.ret.fin)

merge.list.data.dcc.fit.ret.cg<-as.character(list())
for (i in 1:15)
{
merge.list.data.dcc.fit.ret.cg<-append(merge.list.data.dcc.fit.ret.cg,paste("resid.ret.cg.",colnames(base.name[i]),sep=""))
}
merge.name.data.dcc.fit.ret.cg<-lapply(merge.list.data.dcc.fit.ret.cg,get)
names(merge.name.data.dcc.fit.ret.cg)<-merge.list.data.dcc.fit.ret.cg
data.dcc.fit.ret.cg<-do.call(merge,merge.name.data.dcc.fit.ret.cg)

merge.list.data.dcc.fit.ret.ind<-as.character(list())
for (i in 1:15)
{
merge.list.data.dcc.fit.ret.ind<-append(merge.list.data.dcc.fit.ret.ind,paste("resid.ret.ind.",colnames(base.name[i]),sep=""))
}
merge.name.data.dcc.fit.ret.ind<-lapply(merge.list.data.dcc.fit.ret.ind,get)
names(merge.name.data.dcc.fit.ret.ind)<-merge.list.data.dcc.fit.ret.ind
data.dcc.fit.ret.ind<-do.call(merge,merge.name.data.dcc.fit.ret.ind)
	
# adjust data for dcc.fit
if (exists("dcc.fit.adj.ret.fin")==TRUE)
{
if(dcc.fit.adj.ret.fin>=0)
	{
	set.data.dcc.fit.ret.fin<-data.dcc.fit.ret.fin[-c(1:dcc.fit.adj.ret.fin),]
	}
else
	{
	set.data.dcc.fit.ret.fin<-data.dcc.fit.ret.fin
	}
}
if (exists("dcc.fit.adj.ret.fin")==FALSE)
{
set.data.dcc.fit.ret.fin<-data.dcc.fit.ret.fin
}

if (exists("dcc.fit.adj.ret.cg")==TRUE)
{
if(dcc.fit.adj.ret.cg>=0)
	{
	set.data.dcc.fit.ret.cg<-data.dcc.fit.ret.cg[-c(1:dcc.fit.adj.ret.cg),]
	}
else
	{
	set.data.dcc.fit.ret.cg<-data.dcc.fit.ret.cg
	}
}
if (exists("dcc.fit.adj.ret.cg")==FALSE)
{
set.data.dcc.fit.ret.cg<-data.dcc.fit.ret.cg
}

if (exists("dcc.fit.adj.ret.ind")==TRUE)
{
if(dcc.fit.adj.ret.ind>=0)
	{
	set.data.dcc.fit.ret.ind<-data.dcc.fit.ret.ind[-c(1:dcc.fit.adj.ret.ind),]
	}
else
	{
	set.data.dcc.fit.ret.ind<-data.dcc.fit.ret.ind
	}
}
if (exists("dcc.fit.adj.ret.ind")==FALSE)
{
set.data.dcc.fit.ret.ind<-data.dcc.fit.ret.ind
}
	
# performing dcc.fit
dcc.fit.ret.fin<-dccfit(dcc.spec.ret.fin, set.data.dcc.fit.ret.fin, out.sample = 30, solver = "gosolnp", solver.control = list(),fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), cluster = NULL, fit = NULL, VAR.fit = NULL)
dcc.fit.ret.cg<-dccfit(dcc.spec.ret.cg, set.data.dcc.fit.ret.cg, out.sample = 30, solver = "gosolnp", solver.control = list(),fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), cluster = NULL, fit = NULL, VAR.fit = NULL)
dcc.fit.ret.ind<-dccfit(dcc.spec.ret.ind, set.data.dcc.fit.ret.ind, out.sample = 30, solver = "gosolnp", solver.control = list(),fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), cluster = NULL, fit = NULL, VAR.fit = NULL)
# multivariate models
# VAR
# The situation often arises in financial modelling where we have data comprising both time series and cross-sectional elements, and such a dataset would be known as a panel of data or longitudinal data. A panel of data will embody information across both time and space. Importantly, a panel keeps the same individuals or objects (henceforth we will call these ‘entities’) and measures some quantity about them over time.
# Hence, strictly, if the data are not on the same entities (for example, different firms or people) measured over time, then this would not be panel data

# adjustment in exch to account for changing regime from fixed to floating in china and malaysia
# for (i in 1:11)
	# {
	# assign(paste("unadj.ret.",colnames(base.exch[i]),sep=""),get(paste("ret.",colnames(base.exch[i]),sep="")))
	# assign(paste("ret.",colnames(base.exch[i]),sep=""),get(paste("ret.",colnames(base.exch[i]),sep=""))[-c(1:672),])
	# }

# testing for hetero scedasticity (against homoscedasticity=constant variance) ->bstats::white.test
# testing for autocorrelation -> bstats::dw.test

# make a data frame of differencing needed to check
	difference<-c()
	difference<-data.frame(difference)
	for (i in 1:15)
		{
			difference["ndiffs.ret.fin",i]<-ndiffs(ts(get(paste("ret.fin.",colnames(base.name[i]),sep=""))))
			difference["ndiffs.ret.cg",i]<-ndiffs(ts(get(paste("ret.cg.",colnames(base.name[i]),sep=""))))
			difference["ndiffs.ret.ind",i]<-ndiffs(ts(get(paste("ret.ind.",colnames(base.name[i]),sep=""))))
		}
	
	for (i in 1:11)
		{
		difference["ndiffs.ret.exch",i]<-ndiffs(ts(get(paste("ret.",colnames(base.exch[i]),sep=""))))
		}
		

	
	# adf test
	# df test
	df.ret<-c()
	df.ret<-data.frame(df.ret)
	for (i in 1:15)
		{
			df.ret["df.ret.fin.1pct",i]<-abs(get(paste("ur.df.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[1])
			df.ret["df.ret.fin.5pct",i]<-abs(get(paste("ur.df.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[2])
			df.ret["df.ret.fin.10pct",i]<-abs(get(paste("ur.df.ret.fin.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.fin.",colnames(base.name[i]),sep=""))@test$test@cval[3])
			df.ret["df.ret.cg.1pct",i]<-abs(get(paste("ur.df.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[1])
			df.ret["df.ret.cg.5pct",i]<-abs(get(paste("ur.df.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[2])
			df.ret["df.ret.cg.10pct",i]<-abs(get(paste("ur.df.ret.cg.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.cg.",colnames(base.name[i]),sep=""))@test$test@cval[3])
			df.ret["df.ret.ind.1pct",i]<-abs(get(paste("ur.df.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[1])
			df.ret["df.ret.ind.5pct",i]<-abs(get(paste("ur.df.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[2])
			df.ret["df.ret.ind.10pct",i]<-abs(get(paste("ur.df.ret.ind.",colnames(base.name[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.ind.",colnames(base.name[i]),sep=""))@test$test@cval[3])
		}
	colnames(df.ret)<-paste("ret.",colnames(base.name),sep="")
	
	df.ret.exch<-c()
	df.ret.exch<-data.frame(df.ret.exch)
	for (i in 1:11)	
		{
			df.ret.exch["df.ret.exch.1pct",i]<-abs(get(paste("ur.df.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[1])
			df.ret.exch["df.ret.exch.5pct",i]<-abs(get(paste("ur.df.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[2])
			df.ret.exch["df.ret.exch.10pct",i]<-abs(get(paste("ur.df.ret.",colnames(base.exch[i]),sep=""))@test$test@teststat[1])<abs(get(paste("ur.df.ret.",colnames(base.exch[i]),sep=""))@test$test@cval[3])
		}			
	colnames(df.ret.exch)<-paste("ret.",colnames(base.exch),sep="")
	
	
		# for (i in 1:15)
		# {
		# assign(paste("dcc.spec.ret.",colnames(base.name[i]),sep=""), dccspec(get(paste("mspec.ret.",colnames(base.name[i]),sep="")), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL, robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), dccOrder = c(1,1), model = c("DCC"), groups = rep(1, length(uspec@spec)), distribution = c("mvnorm"), start.pars = list(), fixed.pars = list()))
 		# }

# # perform multifit for ret.exch
	# multifit.ret.exch<-multifit(mspec.ret.exch, set.data.dcc.fit.ret.exch, out.sample = 0, solver = "gosolnp", solver.control = list(), fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = "all"), cluster = NULL)
	
# plotting dcc fit result
	class(dcc.fit.ret.fin)
	slotNames(dcc.fit.ret.fin)
	names(dcc.fit.ret.fin@mfit)
	names(dcc.fit.ret.fin@model)
#  plot(dcc.fit.ret.fin, which =(), series=c())
# 	1:   Conditional Mean (vs Realized Returns)
#	2:   Conditional Sigma (vs Realized Absolute Returns)
#	3:   Conditional Covariance
#	4:   Conditional Correlation
#	5:   EW Portfolio Plot with conditional density VaR limits
# conditional mean
# conditional sigma
# conditional covariance
# conditional correlation
# plot(fitted(dcc.fit.ret.fin)[1:2748,i])
# plot(sigma(dcc.fit.ret.fin)[1:2748,i])

# testing for non linearity (null hypotheses of linearity in mean)
for (i in 1:15)
	{
	assign(paste("white.neural.ret.fin.",colnames(base.name[i]),sep=""),tseries::white.test(as.ts(get(paste("ret.fin.",colnames(base.name[i]),sep="")))))
	assign(paste("white.neural.ret.cg.",colnames(base.name[i]),sep=""),tseries::white.test(as.ts(get(paste("ret.cg.",colnames(base.name[i]),sep="")))))
	assign(paste("white.neural.ret.ind.",colnames(base.name[i]),sep=""),tseries::white.test(as.ts(get(paste("ret.ind.",colnames(base.name[i]),sep="")))))
	}

	# multifit estimation
	multifit.ret.fin<-multifit(mspec.ret.fin,ret.fin,solver="gosolnp")
	multifit.ret.cg<-multifit(mspec.ret.cg,ret.cg,solver="gosolnp")
	multifit.ret.ind<-multifit(mspec.ret.ind,ret.ind,solver="gosolnp")
	multifit.ret.exch<-multifit(mspec.ret.exch,ret.exch,solver="gosolnp")
	multifit.ret.int<-multifit(mspec.ret.int,ret.int,solver="gosolnp")
	
	#st.emu
	dcc.test.st.emu.fin<-DCCtest(ret.fin[,c(2,3)],solver="gosolnp")
	dcc.test.st.emu.cg<-DCCtest(ret.cg[,c(2,3)],solver="gosolnp")
	dcc.test.st.emu.ind<-DCCtest(ret.ind[,c(2,3)],solver="gosolnp")
	dcc.test.st.emu.int<-DCCtest(ret.int[,c(2,3)],solver="gosolnp")
	#vol.emu
	dcc.test.vol.emu.fin<-DCCtest(ret.fin[,c(4,7,12)],solver="gosolnp")
	dcc.test.vol.emu.cg<-DCCtest(ret.cg[,c(4,7,12)],solver="gosolnp")
	dcc.test.vol.emu.ind<-DCCtest(ret.ind[,c(4,7,12)],solver="gosolnp")
	dcc.test.vol.emu.int<-DCCtest(ret.int[,c(4,7,12)],solver="gosolnp")
	
	#eas
	dcc.test.eas.fin<-DCCtest(ret.fin[,c(1,5,8,11)],solver="gosolnp")
	dcc.test.eas.cg<-DCCtest(ret.cg[,c(1,5,8,11)],solver="gosolnp")
	dcc.test.eas.ind<-DCCtest(ret.ind[,c(1,5,8,11)],solver="gosolnp")
	dcc.test.eas.exch<-DCCtest(ret.exch[,c(1,3,5,8)],solver="gosolnp")
	dcc.test.eas.int<-DCCtest(ret.int[,c(1,5,8,11)],solver="gosolnp")
	#asean
	dcc.test.asean.fin<-DCCtest(ret.fin[,c(6,9,10,13)],solver="gosolnp")
	dcc.test.asean.cg<-DCCtest(ret.cg[,c(6,9,10,13)],solver="gosolnp")
	dcc.test.asean.ind<-DCCtest(ret.ind[,c(6,9,10,13)],solver="gosolnp")
	dcc.test.asean.exch<-DCCtest(ret.exch[,c(4,6,7,9)],solver="gosolnp")
	dcc.test.asean.int<-DCCtest(ret.int[,c(6,9,10,13)],solver="gosolnp")
	
	#st.emu
	dcc.test.st.emu.fin$p.value>0.05
	dcc.test.st.emu.cg$p.value>0.05
	dcc.test.st.emu.ind$p.value>0.05
	dcc.test.st.emu.int$p.value>0.05
	#vol.emu
	dcc.test.vol.emu.fin$p.value>0.05
	dcc.test.vol.emu.cg$p.value>0.05
	dcc.test.vol.emu.ind$p.value>0.05
	dcc.test.vol.emu.int$p.value>0.05
	
	#eas
	dcc.test.eas.fin$p.value>0.05
	dcc.test.eas.cg$p.value>0.05
	dcc.test.eas.ind$p.value>0.05
	dcc.test.eas.exch$p.value>0.05
	dcc.test.eas.int$p.value>0.05
	#asean
	dcc.test.asean.fin$p.value>0.05
	dcc.test.asean.cg$p.value>0.05
	dcc.test.asean.ind$p.value>0.05
	dcc.test.asean.exch$p.value>0.05
	dcc.test.asean.int$p.value>0.05
	
# calculate graph adjacency of bivariate granger causality of aspac region of interbank lending rate	
	mspec.aspac.int<-c()
	for(i in c(1,5,6,8,9,10,11,13,14))
		{
			mspec.aspac.int<-c(mspec.aspac.int,get(paste("spec.ugarch.",colnames(ret.int[i]),sep="")))
		}
	mspec.aspac.int=multispec(mspec.aspac.int)
	
		assign(paste("dcc.spec.aspac.int"),dccspec(get(paste("mspec.aspac.int")), VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL, robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), dccOrder = c(1,1), model = c("DCC"), groups = rep(1, length(uspec@spec)), distribution = c("mvnorm"), start.pars = list(), fixed.pars = list()))
		
		dcc.fit.aspac.int<-dccfit(dcc.spec.aspac.int, set.data.dcc.fit.ret.int[,c(1,5,6,8,9,10,11,13,14)], out.sample = 30, solver = "gosolnp", solver.control = list(),fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), cluster = NULL, fit = NULL, VAR.fit = NULL)
		
	bi.granger.aspac.int<-c()
	bi.granger.aspac.int<-data.frame(bi.granger.aspac.int)
	for (i in c(1,5,6,8,9,10,11,13,14))
		{
		for (j in c(1,5,6,8,9,10,11,13,14))
			{
				if (i!=j)
				{
			bi.granger.aspac.int[colnames(ret.int[,i]),colnames(ret.int[,j])]<-((grangertest(residuals(dcc.fit.aspac.int)[,colnames(ret.int[,i])],residuals(dcc.fit.aspac.int)[,colnames(ret.int[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
				}
				if (i==j)
				{
			bi.granger.aspac.int[colnames(ret.int[,i]),colnames(ret.int[,j])]<-((grangertest(diff(residuals(dcc.fit.aspac.int)[,colnames(ret.int[,i])]),residuals(dcc.fit.aspac.int)[,colnames(ret.int[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
				}
			}
		}
		
		colnames(bi.granger.across[which(loadcent(bi.granger.across)==max(loadcent(bi.granger.across)))])
