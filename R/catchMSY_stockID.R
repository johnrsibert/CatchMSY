#' Generate a new stock ID object
#' @description Generates a template sID object for new assessments.
#' @param id Name of Stock
#' @param age A vector of integer ages where the terminal age is a plus group.
#' @param linf Asymptotic length for von Bertalanffy growth
#' @param winf Asymptotic weight.
#' @param vbk  von Bertalanffy growth coefficient.
#' @param to   theoretical age at 0 length (years).
#' @param a    allometric scaler in length-weight relationship \eqn{w = a L ^b}
#' @param b    allometric power parameter in length-weight relationship
#' @param ah age at 50\% maturity
#' @param gh standard deviation in age at 50\% maturity
#' @param dfile Name of file containing time series data
#' @return A template for a stock class object.
#' @details This function returns a list object containing all the necessary
#'          information to run the catchMSY routines.  
#'         Column headings are: year catch biomass biomass.lse index index.lse
#' @export
#'
#' @examples 
#' myStock <- new_sID()
#' names(myStock)
new_sID <- function(id  = "Stock Label",
					age = 1:10,
					linf = 100,
					winf = 5.0,
					vbk  = 0.2,
					to   = 0.0,
					a    = 5e-6,
					b    = 3.0,
					ah  = 2.0,
					gh  = 0.2,
					m    = 0.2,
					fmsy = 0.15,
					msy  = 1.0,
					dfile="")
{
	S     <- list()
	S$id  <- id
	S$age <- age

	# growth parameters
	S$linf <- linf
	S$winf <- winf
	S$vbk  <- vbk
	S$to   <- to
	S$a    <- a
	S$b    <- b

	# maturity parameters
	S$ah   <- ah
	S$gh   <- gh

	# selectivity parameters
	S$sel50 <- 2.0
	S$sel95 <- 5.0

	# population parameters starting values
	S$m    <- m
	S$fmsy <- fmsy
	S$msy  <- msy

	# data frame for parameter priors
	S$dfPriorInfo <- data.frame(id=1:3,
	  dist=c("lnorm","unif","norm"),
	  par1=c(log(0.2),0,S$msy),
	  par2=c(0.01,1.0,0.2*S$msy),
	  log = TRUE,
	  stringAsFactors=FALSE)


	# vector of parameters for prior samples
	S$mu <- c(m=S$m,fmsy=S$fmsy,msy=S$msy)
	S$sd <- S$mu * c(0.01,0.1,0.3)
	S$R  <- diag(1,nrow=3,ncol=3)
	S$R[lower.tri(S$R)] <- c(+0.50,-0.10,0.00)
	S$R[upper.tri(S$R)] <- S$R[lower.tri(S$R)]
	S$V  <- S$R*(S$sd%o%S$sd)

	# constraints
	S$lb.depletion <- 0.01
	S$ub.depletion <- 0.80

	# DATA FRAME TEMPLATE FOR TIME SERIES DATA	
	df <- data.frame("year"=1,
					 "catch"=1,
					 "index"=1,
					 "index.lse"=0.1,
					 "biomass"=NA,
					 "biomass.lse"=NA,
					 "avgSize"=NA
					)
	
	
	if(file.exists(dfile)){
		S$data <- read.table(dfile,header=TRUE)
	} else {
		S$data <- df
	}

	class(S) <- c("stockID",class(S))
	return(S)
}
