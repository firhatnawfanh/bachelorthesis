#3d

nrz <- nrow(nisurfacez)
ncz <- ncol(nisurfacez)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue","green","yellow","red") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- nisurfacez[-1, -1] + nisurfacez[-1, -ncz] + nisurfacez[-nrz, -1] + nisurfacez[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp3d(nisurfacex, nisurfacey, nisurfacez, expand=0.75, col = color[facetcol], theta = (i*22.5), phi = 30, ticktype="detailed", xlab="", ylab="time", zlab="", axes=TRUE)

##############################
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)
persp3d(nisurfacex, nisurfacey, nisurfacez, theta=50, phi=25, expand=0.75, col=color[zcol],
        ticktype="detailed", xlab="", ylab="time", zlab="",axes=TRUE)



##############################

for(n in c(1,3,4,8,9,10,15))
	{
nisurfacex<-((nisurface(get(paste("adcc.fit.fin.id.",colnames(base.fin[n]),sep="")),type="cor"))$axis)
nisurfacey<-((nisurface(get(paste("adcc.fit.fin.id.",colnames(base.fin[n]),sep="")),type="cor"))$axis)
nisurfacez<-((nisurface(get(paste("adcc.fit.fin.id.",colnames(base.fin[n]),sep="")),type="cor"))$nisurface)

pdf(paste("nisurface.rcor.adcc.fit.fin.id.",colnames(base.fin[n]),".pdf",sep=""),paper="a4r",width=11.69,height=8.27)

for (i in 1:16)
{
nrz <- nrow(nisurfacez)
ncz <- ncol(nisurfacez)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue","green","yellow","red") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- nisurfacez[-1, -1] + nisurfacez[-1, -ncz] + nisurfacez[-nrz, -1] + nisurfacez[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp(nisurfacex, nisurfacey, nisurfacez, col = color[facetcol], theta = (i*22.5), phi = 30,ticktype="detailed")
}

dev.off()

	}


for(n in c(6))
	{
nisurfacex<-((nisurface(get(paste("adcc.fit.fin.id.",colnames(base.cg[n]),sep="")),type="cor"))$axis)
nisurfacey<-((nisurface(get(paste("adcc.fit.fin.id.",colnames(base.cg[n]),sep="")),type="cor"))$axis)
nisurfacez<-((nisurface(get(paste("adcc.fit.fin.id.",colnames(base.cg[n]),sep="")),type="cor"))$nisurface)

pdf(paste("nisurface.rcor.adcc.fit.fin.id.",colnames(base.cg[n]),".pdf",sep=""),paper="a4r",width=11.69,height=8.27)

for (i in 1:16)
{
nrz <- nrow(nisurfacez)
ncz <- ncol(nisurfacez)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue","green","yellow","red") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- nisurfacez[-1, -1] + nisurfacez[-1, -ncz] + nisurfacez[-nrz, -1] + nisurfacez[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp(nisurfacex, nisurfacey, nisurfacez, col = color[facetcol], theta = (i*22.5), phi = 30,ticktype="detailed")
}

dev.off()

	}


for(n in c(6))
	{
nisurfacex<-((nisurface(get(paste("adcc.fit.fin.id.",colnames(base.ind[n]),sep="")),type="cor"))$axis)
nisurfacey<-((nisurface(get(paste("adcc.fit.fin.id.",colnames(base.ind[n]),sep="")),type="cor"))$axis)
nisurfacez<-((nisurface(get(paste("adcc.fit.fin.id.",colnames(base.ind[n]),sep="")),type="cor"))$nisurface)

pdf(paste("nisurface.rcor.adcc.fit.fin.id.",colnames(base.ind[n]),".pdf",sep=""),paper="a4r",width=11.69,height=8.27)

for (i in 1:16)
{
nrz <- nrow(nisurfacez)
ncz <- ncol(nisurfacez)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue","green","yellow","red") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- nisurfacez[-1, -1] + nisurfacez[-1, -ncz] + nisurfacez[-nrz, -1] + nisurfacez[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp(nisurfacex, nisurfacey, nisurfacez, col = color[facetcol], theta = (i*22.5), phi = 30,ticktype="detailed")
}

dev.off()

	}


