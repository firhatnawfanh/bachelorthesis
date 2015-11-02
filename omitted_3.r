# BIVARIATE GRANGER CAUSALITY & GRAPH ADJACENCY PLOT
# testing granger cause (bivariate)
for (i in 1:15)
	{
	for (j in 1:15)
		{
		if (i!=j)
			{
			assign(paste("bi.granger.ret.fin.",colnames(base.name[i]),"_fin.",colnames(base.name[j]),sep=""),grangertest(get(paste("ret.fin.",colnames(base.name[i]),sep="")),get(paste("ret.fin.",colnames(base.name[j]),sep=""))))
			}
		if (i==j)
			{
			assign(paste("bi.granger.ret.fin.",colnames(base.name[i]),"_fin.",colnames(base.name[j]),sep=""),grangertest(diff(get(paste("ret.fin.",colnames(base.name[i]),sep=""))),get(paste("ret.fin.",colnames(base.name[j]),sep=""))))
			}
		}
	}
	
for (i in 1:15)
	{
	for (j in 1:15)
		{
		if (i!=j)
			{
			assign(paste("bi.granger.ret.cg.",colnames(base.name[i]),"_cg.",colnames(base.name[j]),sep=""),grangertest(get(paste("ret.cg.",colnames(base.name[i]),sep="")),get(paste("ret.cg.",colnames(base.name[j]),sep=""))))
			}
		if (i==j)
			{
			assign(paste("bi.granger.ret.cg.",colnames(base.name[i]),"_cg.",colnames(base.name[j]),sep=""),grangertest(diff(get(paste("ret.cg.",colnames(base.name[i]),sep=""))),get(paste("ret.cg.",colnames(base.name[j]),sep=""))))
			}
		}
	}	

for (i in 1:15)
{
for (j in 1:15)
	{
	if (i!=j)
		{
		assign(paste("bi.granger.ret.ind.",colnames(base.name[i]),"_ind.",colnames(base.name[j]),sep=""),grangertest(get(paste("ret.ind.",colnames(base.name[i]),sep="")),get(paste("ret.ind.",colnames(base.name[j]),sep=""))))
		}
	if (i==j)
		{
		assign(paste("bi.granger.ret.ind.",colnames(base.name[i]),"_ind.",colnames(base.name[j]),sep=""),grangertest(diff(get(paste("ret.ind.",colnames(base.name[i]),sep=""))),get(paste("ret.ind.",colnames(base.name[j]),sep=""))))
		}
	}
}	
		
for (i in 1:10)
{
for (j in 1:10)
	{
	if (i!=j)
		{
		#if(((i==2 & j==11)|(i==11 & j==2))==TRUE)
		#{
		assign(paste("bi.granger.ret.",colnames(base.exch[i]),"_",colnames(base.exch[j]),sep=""),grangertest(diff(get(paste("ret.",colnames(base.exch[i]),sep=""))),get(paste("ret.",colnames(base.exch[j]),sep=""))))
		#}
	
		#if(((i==2 & j==11)|(i==11 & j==2))==FALSE)
		#{
		#assign(paste("bi.granger.ret.",colnames(base.exch[i]),"_",colnames(base.exch[j]),sep=""),grangertest(get(paste("ret.",colnames(base.exch[i]),sep="")),get(paste("ret.",colnames(base.exch[j]),sep=""))))
		#}
	}
		
	if (i==j)
		{
		assign(paste("bi.granger.ret.",colnames(base.exch[i]),"_",colnames(base.exch[j]),sep=""),grangertest(diff(get(paste("ret.",colnames(base.exch[i]),sep=""))),get(paste("ret.",colnames(base.exch[j]),sep=""))))
		}
	}
}
	
for (i in 1:10)
{
for (j in 1:10)
	{
	if (i!=j)
		{
		assign(paste("bi.granger.ret.",colnames(base.int[i]),"_",colnames(base.int[j]),sep=""),grangertest(diff(get(paste("ret.",colnames(base.int[i]),sep=""))),get(paste("ret.",colnames(base.int[j]),sep=""))))
		}
		
	if (i==j)
		{
		assign(paste("bi.granger.ret.",colnames(base.int[i]),"_",colnames(base.int[j]),sep=""),grangertest(diff(get(paste("ret.",colnames(base.int[i]),sep=""))),get(paste("ret.",colnames(base.int[j]),sep=""))))
		}
	}
}
	
	
	
num.bi.granger.ret.fin<-c()
num.bi.granger.ret.fin<-data.frame(num.bi.granger.ret.fin)
for (i in 1:15)
	{
	for (j in 1:15)
		{
		num.bi.granger.ret.fin[paste("bi.granger.ret.fin.",colnames(base.name[i]),sep=""), paste("bi.granger.ret.fin.",colnames(base.name[j]),sep="")]<-get(paste("bi.granger.ret.fin.",colnames(base.name[i]),"_fin.",colnames(base.name[j]),sep=""))$"Pr(>F)"[2]
		}
	}

num.bi.granger.ret.cg<-c()
num.bi.granger.ret.cg<-data.frame(num.bi.granger.ret.cg)
for (i in 1:15)
	{
	for (j in 1:15)
		{
		num.bi.granger.ret.cg[paste("bi.granger.ret.cg.",colnames(base.name[i]),sep=""), paste("bi.granger.ret.cg.",colnames(base.name[j]),sep="")]<-get(paste("bi.granger.ret.cg.",colnames(base.name[i]),"_cg.",colnames(base.name[j]),sep=""))$"Pr(>F)"[2]
		}
	}

num.bi.granger.ret.ind<-c()
num.bi.granger.ret.ind<-data.frame(num.bi.granger.ret.ind)
for (i in 1:15)
	{
	for (j in 1:15)
		{
		num.bi.granger.ret.ind[paste("bi.granger.ret.ind.",colnames(base.name[i]),sep=""), paste("bi.granger.ret.ind.",colnames(base.name[j]),sep="")]<-get(paste("bi.granger.ret.ind.",colnames(base.name[i]),"_ind.",colnames(base.name[j]),sep=""))$"Pr(>F)"[2]
		}
	}

colnames(num.bi.granger.ret.fin)<-colnames(ret.fin)
rownames(num.bi.granger.ret.fin)<-colnames(ret.fin)

colnames(num.bi.granger.ret.cg)<-colnames(ret.cg)
rownames(num.bi.granger.ret.cg)<-colnames(ret.cg)

colnames(num.bi.granger.ret.ind)<-colnames(ret.ind)
rownames(num.bi.granger.ret.ind)<-colnames(ret.ind)

bi.granger.ret.fin<-num.bi.granger.ret.fin>0.05
bi.granger.ret.cg<-num.bi.granger.ret.cg>0.05
bi.granger.ret.ind<-num.bi.granger.ret.ind>0.05

colnames(bi.granger.ret.fin)<-colnames(ret.fin)
rownames(bi.granger.ret.fin)<-colnames(ret.fin)

colnames(bi.granger.ret.cg)<-colnames(ret.cg)
rownames(bi.granger.ret.cg)<-colnames(ret.cg)

colnames(bi.granger.ret.ind)<-colnames(ret.ind)
rownames(bi.granger.ret.ind)<-colnames(ret.ind)

num.bi.granger.ret.exch<-c()
num.bi.granger.ret.exch<-data.frame(num.bi.granger.ret.exch)
for (i in 1:10)
	{
	for (j in 1:10)
		{
		num.bi.granger.ret.exch[paste("bi.granger.ret.",colnames(base.exch[i]),sep=""), paste("bi.granger.ret.",colnames(base.exch[j]),sep="")]<-get(paste("bi.granger.ret.",colnames(base.exch[i]),"_",colnames(base.exch[j]),sep=""))$"Pr(>F)"[2]
		}
	}

colnames(num.bi.granger.ret.exch)<-colnames(ret.exch)
rownames(num.bi.granger.ret.exch)<-colnames(ret.exch)

bi.granger.ret.exch<-num.bi.granger.ret.exch>0.05

colnames(bi.granger.ret.exch)<-colnames(ret.exch[,1:10])
rownames(bi.granger.ret.exch)<-colnames(ret.exch[,1:10])

num.bi.granger.ret.int<-c()
num.bi.granger.ret.int<-data.frame(num.bi.granger.ret.int)
for (i in 1:10)
	{
	for (j in 1:10)
		{
		num.bi.granger.ret.int[paste("bi.granger.ret.",colnames(base.int[i]),sep=""), paste("bi.granger.ret.",colnames(base.int[j]),sep="")]<-get(paste("bi.granger.ret.",colnames(base.int[i]),"_",colnames(base.int[j]),sep=""))$"Pr(>F)"[2]
		}
	}

colnames(num.bi.granger.ret.int)<-colnames(ret.int)
rownames(num.bi.granger.ret.int)<-colnames(ret.int)

bi.granger.ret.int<-num.bi.granger.ret.int>0.05

colnames(bi.granger.ret.int)<-colnames(ret.int)
rownames(bi.granger.ret.int)<-colnames(ret.int)

net.bi.granger.ret.fin<-(bi.granger.ret.fin==FALSE)*1
net.bi.granger.ret.cg<-(bi.granger.ret.cg==FALSE)*1
net.bi.granger.ret.ind<-(bi.granger.ret.ind==FALSE)*1
net.bi.granger.ret.exch<-(bi.granger.ret.exch==FALSE)*1
net.bi.granger.ret.int<-(bi.granger.ret.int==FALSE)*1

bi.granger.ret.fin
bi.granger.ret.cg
bi.granger.ret.ind
bi.granger.ret.exch
bi.granger.ret.int

tkplot(graph.adjacency(net.bi.granger.ret.fin,mode=c("directed")))
tkplot(graph.adjacency(net.bi.granger.ret.cg,mode=c("directed")))
tkplot(graph.adjacency(net.bi.granger.ret.ind,mode=c("directed")))
tkplot(graph.adjacency(net.bi.granger.ret.exch,mode=c("directed")))
tkplot(graph.adjacency(net.bi.granger.ret.int,mode=c("directed")))

#################################################################################################################
# REGIONAL BIVARIATE GRANGER CAUSALITY - ADJACENCY GRAPH
bi.granger.west.fin<-c()
bi.granger.west.fin<-data.frame(bi.granger.west.fin)
for (i in c(2,3,4,7,12,14,15))
	{
	for (j in c(2,3,4,7,12,14,15))
		{
			if (i!=j)
			{
		bi.granger.west.fin[colnames(ret.fin[,i]),colnames(ret.fin[,j])]<-((grangertest(residuals(dcc.fit.west.fin)[,colnames(ret.fin[,i])],residuals(dcc.fit.west.fin)[,colnames(ret.fin[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.west.fin[colnames(ret.fin[,i]),colnames(ret.fin[,j])]<-((grangertest(diff(residuals(dcc.fit.west.fin)[,colnames(ret.fin[,i])]),residuals(dcc.fit.west.fin)[,colnames(ret.fin[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}
	
bi.granger.west.cg<-c()
bi.granger.west.cg<-data.frame(bi.granger.west.cg)
for (i in c(2,3,4,7,12,14,15))
	{
	for (j in c(2,3,4,7,12,14,15))
		{
			if (i!=j)
			{
		bi.granger.west.cg[colnames(ret.cg[,i]),colnames(ret.cg[,j])]<-((grangertest(residuals(dcc.fit.west.cg)[,colnames(ret.cg[,i])],residuals(dcc.fit.west.cg)[,colnames(ret.cg[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.west.cg[colnames(ret.cg[,i]),colnames(ret.cg[,j])]<-((grangertest(diff(residuals(dcc.fit.west.cg)[,colnames(ret.cg[,i])]),residuals(dcc.fit.west.cg)[,colnames(ret.cg[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}

bi.granger.west.ind<-c()
bi.granger.west.ind<-data.frame(bi.granger.west.ind)
for (i in c(2,3,4,7,12,14,15))
	{
	for (j in c(2,3,4,7,12,14,15))
		{
			if (i!=j)
			{
		bi.granger.west.ind[colnames(ret.ind[,i]),colnames(ret.ind[,j])]<-((grangertest(residuals(dcc.fit.west.ind)[,colnames(ret.ind[,i])],residuals(dcc.fit.west.ind)[,colnames(ret.ind[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.west.ind[colnames(ret.ind[,i]),colnames(ret.ind[,j])]<-((grangertest(diff(residuals(dcc.fit.west.ind)[,colnames(ret.ind[,i])]),residuals(dcc.fit.west.ind)[,colnames(ret.ind[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}
	

bi.granger.west.exch<-c()
bi.granger.west.exch<-data.frame(bi.granger.west.exch)
for (i in c(2,10,11))
	{
	for (j in c(2,10,11))
		{
			if (i!=j)
			{
		bi.granger.west.exch[colnames(ret.exch[,i]),colnames(ret.exch[,j])]<-((grangertest(residuals(dcc.fit.west.exch)[,colnames(ret.exch[,i])],residuals(dcc.fit.west.exch)[,colnames(ret.exch[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.west.exch[colnames(ret.exch[,i]),colnames(ret.exch[,j])]<-((grangertest(diff(residuals(dcc.fit.west.exch)[,colnames(ret.exch[,i])]),residuals(dcc.fit.west.exch)[,colnames(ret.exch[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}


bi.granger.west.int<-c()
bi.granger.west.int<-data.frame(bi.granger.west.int)
for (i in c(2,10))
	{
	for (j in c(2,10))
		{
			if (i!=j)
			{
		bi.granger.west.int[colnames(ret.int[,i]),colnames(ret.int[,j])]<-((grangertest(residuals(dcc.fit.west.int)[,colnames(ret.int[,i])],residuals(dcc.fit.west.int)[,colnames(ret.int[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.west.int[colnames(ret.int[,i]),colnames(ret.int[,j])]<-((grangertest(diff(residuals(dcc.fit.west.int)[,colnames(ret.int[,i])]),residuals(dcc.fit.west.int)[,colnames(ret.int[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}
	


bi.granger.aspac.fin<-c()
bi.granger.aspac.fin<-data.frame(bi.granger.aspac.fin)
for (i in c(1,5,6,8,9,10,11,13,15))
	{
	for (j in c(1,5,6,8,9,10,11,13,15))
		{
			if (i!=j)
			{
		bi.granger.aspac.fin[colnames(ret.fin[,i]),colnames(ret.fin[,j])]<-((grangertest(residuals(dcc.fit.aspac.fin)[,colnames(ret.fin[,i])],residuals(dcc.fit.aspac.fin)[,colnames(ret.fin[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.aspac.fin[colnames(ret.fin[,i]),colnames(ret.fin[,j])]<-((grangertest(diff(residuals(dcc.fit.aspac.fin)[,colnames(ret.fin[,i])]),residuals(dcc.fit.aspac.fin)[,colnames(ret.fin[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}
	
bi.granger.aspac.cg<-c()
bi.granger.aspac.cg<-data.frame(bi.granger.aspac.cg)
for (i in c(1,5,6,8,9,10,11,13,15))
	{
	for (j in c(1,5,6,8,9,10,11,13,15))
		{
			if (i!=j)
			{
		bi.granger.aspac.cg[colnames(ret.cg[,i]),colnames(ret.cg[,j])]<-((grangertest(residuals(dcc.fit.aspac.cg)[,colnames(ret.cg[,i])],residuals(dcc.fit.aspac.cg)[,colnames(ret.cg[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.aspac.cg[colnames(ret.cg[,i]),colnames(ret.cg[,j])]<-((grangertest(diff(residuals(dcc.fit.aspac.cg)[,colnames(ret.cg[,i])]),residuals(dcc.fit.aspac.cg)[,colnames(ret.cg[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}

bi.granger.aspac.ind<-c()
bi.granger.aspac.ind<-data.frame(bi.granger.aspac.ind)
for (i in c(1,5,6,8,9,10,11,13,15))
	{
	for (j in c(1,5,6,8,9,10,11,13,15))
		{
			if (i!=j)
			{
		bi.granger.aspac.ind[colnames(ret.ind[,i]),colnames(ret.ind[,j])]<-((grangertest(residuals(dcc.fit.aspac.ind)[,colnames(ret.ind[,i])],residuals(dcc.fit.aspac.ind)[,colnames(ret.ind[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.aspac.ind[colnames(ret.ind[,i]),colnames(ret.ind[,j])]<-((grangertest(diff(residuals(dcc.fit.aspac.ind)[,colnames(ret.ind[,i])]),residuals(dcc.fit.aspac.ind)[,colnames(ret.ind[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}
	
bi.granger.aspac.exch<-c()
bi.granger.aspac.exch<-data.frame(bi.granger.aspac.exch)
for (i in c(1,3,4,5,6,7,8,9,11))
	{
	for (j in c(1,3,4,5,6,7,8,9,11))
		{
			if (i!=j)
			{
		bi.granger.aspac.exch[colnames(ret.exch[,i]),colnames(ret.exch[,j])]<-((grangertest(residuals(dcc.fit.aspac.exch)[,colnames(ret.exch[,i])],residuals(dcc.fit.aspac.exch)[,colnames(ret.exch[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.aspac.exch[colnames(ret.exch[,i]),colnames(ret.exch[,j])]<-((grangertest(diff(residuals(dcc.fit.aspac.exch)[,colnames(ret.exch[,i])]),residuals(dcc.fit.aspac.exch)[,colnames(ret.exch[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}


	
tkplot(graph.adjacency(as.matrix(bi.granger.inter.fin),mode=c("directed")))
tkplot(graph.adjacency(as.matrix(bi.granger.inter.cg),mode=c("directed")))
tkplot(graph.adjacency(as.matrix(bi.granger.inter.ind),mode=c("directed")))

tkplot(graph.adjacency(as.matrix(bi.granger.west.fin),mode=c("directed")))
tkplot(graph.adjacency(as.matrix(bi.granger.west.cg),mode=c("directed")))
tkplot(graph.adjacency(as.matrix(bi.granger.west.ind),mode=c("directed")))
tkplot(graph.adjacency(as.matrix(bi.granger.west.exch),mode=c("directed")))
tkplot(graph.adjacency(as.matrix(bi.granger.west.int),mode=c("directed")))

tkplot(graph.adjacency(as.matrix(bi.granger.aspac.fin),mode=c("directed")))
tkplot(graph.adjacency(as.matrix(bi.granger.aspac.cg),mode=c("directed")))
tkplot(graph.adjacency(as.matrix(bi.granger.aspac.ind),mode=c("directed")))
tkplot(graph.adjacency(as.matrix(bi.granger.aspac.exch),mode=c("directed")))
	
#################################################################################################################

num.bi.granger.across<-c()
num.bi.granger.across<-data.frame(num.bi.granger.across)
for (i in 1:45)
	{
	for (j in 1:45)
		{
			if (i!=j)
			{
		num.bi.granger.across[colnames(residuals(dcc.fit.across)[,i]),colnames(residuals(dcc.fit.across)[,j])]<-grangertest(residuals(dcc.fit.across)[,colnames(residuals(dcc.fit.across)[,i])],residuals(dcc.fit.across)[,colnames(residuals(dcc.fit.across)[,j])])$"Pr(>F)"[2]
			}
			if (i==j)
			{
		num.bi.granger.across[colnames(residuals(dcc.fit.across)[,i]),colnames(residuals(dcc.fit.across)[,j])]<-grangertest(diff(residuals(dcc.fit.across)[,colnames(residuals(dcc.fit.across)[,i])]),residuals(dcc.fit.across)[,colnames(residuals(dcc.fit.across)[,j])])$"Pr(>F)"[2]
			}
		}
	}
	
bi.granger.across<-c()
bi.granger.across<-data.frame(bi.granger.across)
for (i in 1:45)
	{
	for (j in 1:45)
		{
			if (i!=j)
			{
		bi.granger.across[colnames(residuals(dcc.fit.across)[,i]),colnames(residuals(dcc.fit.across)[,j])]<-((grangertest(residuals(dcc.fit.across)[,colnames(residuals(dcc.fit.across)[,i])],residuals(dcc.fit.across)[,colnames(residuals(dcc.fit.across)[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.across[colnames(residuals(dcc.fit.across)[,i]),colnames(residuals(dcc.fit.across)[,j])]<-((grangertest(diff(residuals(dcc.fit.across)[,colnames(residuals(dcc.fit.across)[,i])]),residuals(dcc.fit.across)[,colnames(residuals(dcc.fit.across)[,j])])$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}

tkplot(graph.adjacency(as.matrix(bi.granger.across),mode=c("directed")))
west.region<-sort(c(c(2,3,4,7,12,14,15)*3,(c(2,3,4,7,12,14,15)*3)-1,(c(2,3,4,7,12,14,15)*3)-2))
aspac.region<-sort(c(c(1,5,6,8,9,10,11,13,15)*3,(c(1,5,6,8,9,10,11,13,15)*3)-1,(c(1,5,6,8,9,10,11,13,15)*3)-2))

bi.granger.across.west<-bi.granger.across[west.region,west.region]
bi.granger.across.aspac<-bi.granger.across[aspac.region,aspac.region]
graph.adj.across<-graph.adjacency(bi.granger.across)
graph.adj.across.west<-graph.adjacency(bi.granger.across.west)
graph.adj.across.aspac<-graph.adjacency(bi.granger.across.aspac)
tkplot(graph.adjacency(as.matrix(bi.granger.across.west),mode=c("directed")))
tkplot(graph.adjacency(as.matrix(bi.granger.across.aspac),mode=c("directed")))
########################################
bi.granger.inter.fin<-c()
bi.granger.inter.fin<-data.frame(bi.granger.inter.fin)
for (i in 1:15)
	{
	for (j in 1:15)
		{
			if (i!=j)
			{
		bi.granger.inter.fin[colnames(ret.fin[,i]), colnames(ret.fin[,j])]<-((grangertest(residuals(get(paste("ugarch.",colnames(ret.fin[,i]),sep=""))),residuals(get(paste("ugarch.",colnames(ret.fin[,j]),sep=""))))$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.inter.fin[colnames(ret.fin[,i]), colnames(ret.fin[,j])]<-((grangertest(diff(residuals(get(paste("ugarch.",colnames(ret.fin[,i]),sep="")))),residuals(get(paste("ugarch.",colnames(ret.fin[,j]),sep=""))))$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}
		
bi.granger.inter.cg<-c()
bi.granger.inter.cg<-data.frame(bi.granger.inter.cg)
for (i in 1:15)
	{
	for (j in 1:15)
		{
			if (i!=j)
			{
		bi.granger.inter.cg[colnames(ret.cg[,i]), colnames(ret.cg[,j])]<-((grangertest(residuals(get(paste("ugarch.",colnames(ret.cg[,i]),sep=""))),residuals(get(paste("ugarch.",colnames(ret.cg[,j]),sep=""))))$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.inter.cg[colnames(ret.cg[,i]), colnames(ret.cg[,j])]<-((grangertest(diff(residuals(get(paste("ugarch.",colnames(ret.cg[,i]),sep="")))),residuals(get(paste("ugarch.",colnames(ret.cg[,j]),sep=""))))$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}
	
bi.granger.inter.ind<-c()
bi.granger.inter.ind<-data.frame(bi.granger.inter.ind)
for (i in 1:15)
	{
	for (j in 1:15)
		{
			if (i!=j)
			{
		bi.granger.inter.ind[colnames(ret.ind[,i]), colnames(ret.ind[,j])]<-((grangertest(residuals(get(paste("ugarch.",colnames(ret.ind[,i]),sep=""))),residuals(get(paste("ugarch.",colnames(ret.ind[,j]),sep=""))))$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
			if (i==j)
			{
		bi.granger.inter.ind[colnames(ret.ind[,i]), colnames(ret.ind[,j])]<-((grangertest(diff(residuals(get(paste("ugarch.",colnames(ret.ind[,i]),sep="")))),residuals(get(paste("ugarch.",colnames(ret.ind[,j]),sep=""))))$"Pr(>F)"[2]>0.05)==FALSE)*1
			}
		}
	}
	
	
###################################################


###################################################

zlim <- range(nisurfacez)
zlen <- zlim[2] - zlim[1] + 1

colorlut <- terrain.colors(zlen) # height color lookup table

col <- colorlut[abs(z-zlim[1]+1)] # assign colors to heights for each point
   
open3d()
persp3d(nisurfacex,nisurfacey,nisurfacez,theta=50, phi=25, expand=0.75, color=col,back="lines",ticktype="detailed", xlab="x-axis", ylab="y-axis", zlab="z-axis",axes=TRUE)
 
##########################################      

nbcol = 100
# rev is for reverse-ing the color vector 
col <- rbind(1, cbind(matrix(col, nx-1, ny-1), 1))

colmat=as.matrix(color[zcol])


nbcol=99

color = rev(rainbow(nbcol, start = 0, end = 1))
zcol  = cut(nisurfacez, nbcol)


nbcol=99
color = as.matrix(rev(rainbow(nbcol, start = 0, end = 1)))

colmat<-matrix(nrow=nbcol,ncol=nbcol)

for(i in 1:nbcol)
{
	for (j in 1:nbcol)
	{
	if (i+j<=2*i)
		{colmat[i,j]<-(color[i,])}
	if (i+j<=2*j)
        {colmat[i,j]<-(color[j,])}
	}
}
View(colmat)

colmatp<-matrix(nrow=nbcol+1,ncol=nbcol+1)
colmatp<-rbind(1, cbind(matrix(colmatp, nbcol, nbcol), 1))

open3d()
persp3d(nisurfacex,nisurfacey,nisurfacez,theta=50, phi=25, expand=0.75, col=color[zcol],
        ticktype="detailed", xlab="x-axis", ylab="y-axis", zlab="z-axis",axes=TRUE)
 
#############################################

color <- rgb(85, 141, 85, maxColorValue=255)
open3d()
persp3d(nisurfacex,nisurfacey,nisurfacez,theta=50, phi=25, expand=0.75, col=color,
        ticktype="detailed", xlab="x-axis", ylab="y-axis", zlab="z-axis",axes=TRUE)
#############################################       
rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
heat.colors(n, alpha = 1)
terrain.colors(n, alpha = 1)
topo.colors(n, alpha = 1)
cm.colors(n, alpha = 1)


n	
the number of colors (≥ 1) to be in the palette.

s, v	
the ‘saturation’ and ‘value’ to be used to complete the HSV color descriptions.

start	
the (corrected) hue in [0,1] at which the rainbow begins.

end	
the (corrected) hue in [0,1] at which the rainbow ends.

alpha	
the alpha transparency, a number in [0,1], see argument alpha in hsv.
