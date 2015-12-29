#'S3 class methods for providing summary plots
#'@export
plot.stockID <- function(sID,.THEME=NULL){

	with(sID,{
		if(exists("idx")){
			xinc = 50
			yinc = 50
			xpos = 2*xinc
			ypos = yinc
			# -----------------------------------------------------------------#
			# Vulnerable biomass.
			bio.df <- data.frame(Year=data$year,
			                     Biomass=t(ps.bt)[,idx])
			bio.df <- gather(bio.df,key,bt,-Year)

			p <- ggplot(bio.df,aes(Year,y=bt))
			p <- p + stat_summary(geom="ribbon",fun.ymin="min",fun.ymax="max",
			                      alpha=0.1)
			p <- p + stat_summary(geom="ribbon", fun.data="median_hilow",
			                      alpha=0.2,fill="blue")
			p <- p + stat_summary(geom="ribbon", fun.data="mean_cl_boot",
			                      alpha=0.2,fill="red")
			p <- p + labs(x="Year",y="Vulnerable Biomass (lbs)")
 			x11(title="Biomass",xpos=xpos,ypos=ypos)
			print(p+.THEME)
			# -----------------------------------------------------------------#

			# -----------------------------------------------------------------#
			#  Spawning biomass depletion
			xpos = xpos + xinc
			ypos = ypos + yinc
			ssb.df <- data.frame(Year=data$year,
			                     Biomass=t(ps.dt)[,idx])
			ssb.df <- gather(ssb.df,key,dt,-Year)

			p <- ggplot(ssb.df,aes(Year,y=dt))
			p <- p + stat_summary(geom="ribbon",fun.ymin="min",fun.ymax="max",
			                      alpha=0.1)
			p <- p + stat_summary(geom="ribbon", fun.data="median_hilow",
			                      alpha=0.2,fill="blue")
			p <- p + stat_summary(geom="ribbon", fun.data="mean_cl_boot",
			                      alpha=0.2,fill="red")
			p <- p + labs(x="Year",y="Spawning Biomass Depletion")
 			x11(title="Depletion",xpos=xpos,ypos=ypos)
			print(p+.THEME)
			# -----------------------------------------------------------------#

			# -----------------------------------------------------------------#
			# Parameter distributions
			xpos = xpos + xinc
			ypos = ypos + yinc
			gS <- gather(as.data.frame(S[idx,]))
			p <- ggplot(gS,aes(x=value)) 
			p <- p + geom_histogram(aes(y=..density..),alpha=0.3) 
			p <- p + geom_density(col=NA,fill="red",alpha=0.2) 
			p <- p + facet_wrap(~key,scales="free")
 			x11(title="Parameters",xpos=xpos,ypos=ypos)
			print(p+.THEME)
			# -----------------------------------------------------------------#

		}



	})
	
}
