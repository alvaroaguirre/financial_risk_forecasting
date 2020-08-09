########## Output only desired information from GARCHfit ##########
# Function takes an object from the GARCHfit class 
# Outputs only specification, coefficient and robust s.e., and likelihood

garch_fm442 <- function(object){
	vmodel = object@model$modeldesc$vmodel
	model = object@model
	modelinc = object@model$modelinc
	cat(paste("\n*---------------------------------*", sep = ""))
	cat(paste("\n*          GARCH Model Fit        *", sep = ""))
	cat(paste("\n*---------------------------------*", sep = ""))
	cat("\n\nConditional Variance Dynamics \t")
	cat(paste("\n-----------------------------------", sep = ""))
	cat(paste("\nGARCH Model\t: ", vmodel,"(", modelinc[8], ",", modelinc[9], ")\n", sep=""))
	if(vmodel == "fGARCH"){
		cat(paste("fGARCH Sub-Model\t: ", model$modeldesc$vsubmodel, "\n", sep = ""))
	}
	cat("Mean Model\t: ARFIMA(", modelinc[2],",",ifelse(modelinc[4]>0, "d", 0),",",modelinc[3],")\n", sep = "")
	cat("Distribution\t:", model$modeldesc$distribution,"\n")
	if(object@fit$convergence == 0){
		cat("\nOptimal Parameters with Robust S.E.:")
		cat(paste("\n------------------------------------\n",sep=""))
		print(round(object@fit$robust.matcoef,6), digits = 5)
		if(!is.null(object@fit$hessian.message)){
			cat(paste("\n", object@fit$hessian.message))
		}
		cat("\nLogLikelihood :", object@fit$LLH, "\n")
	}
}