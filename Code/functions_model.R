
estimate.STmodel <- function(object, x, x.fixed=NULL, type="p",
                             h=1e-3, diff.type=1, hessian.all=FALSE,
                             lower=-15, upper=15, method="L-BFGS-B",
                             control=list(trace=3, maxit=1000), ...){
  ##check class belonging
  stCheckClass(object, "STmodel", name="object")
  
  ##get size of the models
  dimensions <- loglikeSTdim(object)
  
  ##ensure lower case
  type <- tolower(type) 
  ##first check that type is valid
  SpatioTemporal:::stCheckType(type)
  
  ##Second check the input x
  x <- as.matrix(x)
  ##if x has length one we need to construct a matrix of initial values
  if( length(x)==1 ){
    x <- matrix(seq(-5,5,length.out=x), dimensions$nparam.cov, x, byrow=TRUE)
  }
  ##check that starting point is valid
  tmp <- SpatioTemporal:::stCheckX(x, x.fixed, dimensions, type, object)
  x.all <- tmp$x.all
  x <- tmp$x
  x.fixed <- tmp$x.fixed

  ##Default values for control
  control <- defaultList(control, eval(formals(estimate.STmodel)$control) )
                                    
  ##define local version of gradient function, fixing h and diff.type
  loglikeSTGrad.loc <- function(x, STmodel, type, x.fixed){
    loglikeSTGrad(x, STmodel, type, x.fixed, h=h, diff.type=diff.type)
  }##function loglikeSTGrad.loc

  ##attempt to fit the model for each of the provided starting values.
  res <- as.list( rep(NA, dim(x)[2]) )
  ##vector with convergence information and optimal values
  conv <- rep(FALSE, dim(x)[2])
  value <- rep(NA, dim(x)[2])

  ##make sure that likelihood evaluates
  err <- tryCatch( loglikeST(x[,1], STmodel=object, type=type,
                             x.fixed=x.fixed), silent=TRUE )
  if( inherits(err,"try-error") ){
    stop( paste("log-likelihood fails at first starting point with:\n",
                err[[1]]) )
  }
  
  #############
  # J. Keller edit; not in original package code
  #
  # Define local version of the likelihood function, to use in calculating
  # Hessians using numDeriv package
  #
  loglikeST.loc <- function(x) loglikeST(x, object)
  ############
  
  ##ensure that we are doing maximisation
  control$fnscale <- -1
  ##loop over starting values
  for(i in 1:dim(x)[2]){
    if( control$trace!=0 ){
      message( paste("Optimisation using starting value ",
                     i, "/", dim(x)[2], sep="") )
    }
    try( res[[i]] <- optim(x[,i], loglikeST, gr=loglikeSTGrad.loc,
                           STmodel=object, type=type, x.fixed=x.fixed,
                           method=method, control=control, hessian=TRUE,
                           lower=lower, upper=upper), silent=TRUE)
    ##has optimisation converged?
    if( all( !is.na(res[[i]]) ) ){
      ##then compute convergence criteria
      
      #############
	  # J. Keller edit; not in original package code
	  #
	  # Calculate Hessian using numDeriv package
      res[[i]]$optim_hessian <- res[[i]]$hessian
      res[[i]]$hessian <- hessian(loglikeST.loc, res[[i]]$par) #, method.args=list(d=0.01, r=6))
 		 		
      ###############   
      
      conv[i] <- (res[[i]]$convergence==0 &&
                  all(eigen(res[[i]]$hessian)$value < -1e-10))
      ##extract ML-value
      value[i] <- res[[i]]$value

      ##add convergence and initial parameters
      res[[i]]$conv <- conv[i]
      res[[i]]$par.cov <- data.frame(par=double(dimensions$nparam.cov), sd=NA,
                                     fixed=double(dimensions$nparam.cov),
                                     init=double(dimensions$nparam.cov),
                                     tstat=double(dimensions$nparam.cov))
      res[[i]]$par.all <- data.frame(par=double(dimensions$nparam), sd=NA,
                                     fixed=double(dimensions$nparam),
                                     init=double(dimensions$nparam),
                                     tstat=double(dimensions$nparam))
     
      ##add standard deviations
      #
      # Edit by J. Kelller, January 2013
      # Original code had:
      #suppressWarnings( par.sd <- sqrt(-diag(solve(res[[i]]$hessian))) )
      #
      # If optimization resulted in (computationally) singular hessian, then this
      # throws an error that is not picked up and execution halts.
      #Switched to this code:
      par.sd <- sqrt(-diag(tryCatch(solve(res[[i]]$hessian), error=function(e) rep(-10000, length(res[[i]]$par)))))
            

      ##initial value
      if( type!="f" ){
        par.type <- "par.cov"
      }else{
        par.type <- "par.all"
      }
      ##parameters
      res[[i]][[par.type]]$init <- x.all[,i]
      res[[i]][[par.type]]$par <- res[[i]][[par.type]]$fixed <- x.fixed
      res[[i]][[par.type]]$par[is.na(x.fixed)] <- res[[i]]$par
      ##standard error
      res[[i]][[par.type]]$sd[is.na(x.fixed)] <- par.sd
      
      if( type!="f" ){
        ##compute regression parameters
        tmp <- predict(object, res[[i]]$par.cov$par, only.pars=TRUE,
                       pred.var=FALSE, type=type)$pars
        res[[i]]$par.all$par <- c(tmp$gamma.E, tmp$alpha.E, res[[i]]$par.cov$par)
        N.reg <- length(tmp$gamma.E)+length(tmp$alpha.E)
        res[[i]]$par.all$sd <- c(rep(NA,N.reg), res[[i]]$par.cov$sd)
        res[[i]]$par.all$init <- c(rep(NA,N.reg), x.all[,i])
        res[[i]]$par.all$fixed <- c(rep(NA,N.reg), x.fixed)
      }else{
        ##all the covariance parameters
        I <- (dimensions$nparam-dimensions$nparam.cov+1):dimensions$nparam
        res[[i]]$par.cov <- res[[i]]$par.all[I,,drop=FALSE]
      }
      ##t-statistic
      res[[i]]$par.cov$tstat <- res[[i]]$par.cov$par / res[[i]]$par.cov$sd
      res[[i]]$par.all$tstat <- res[[i]]$par.all$par / res[[i]]$par.all$sd
      
      ##add names to the variables.
      rownames(res[[i]]$par.all) <- loglikeSTnames(object, all=TRUE)
      rownames(res[[i]]$par.cov) <- loglikeSTnames(object, all=FALSE)

      if( type!="f" ){
        names(res[[i]]$par) <- loglikeSTnames(object, all=FALSE)[is.na(x.fixed)]
      }else{
        names(res[[i]]$par) <- loglikeSTnames(object, all=TRUE)[is.na(x.fixed)]
      }
    }##if(all(!is.na(res[[i]])))
  }##for(i in 1:dim(x)[2])

  if( all(is.na(res)) ){
    stop("All optimisations failed, consider trying different starting values.")
  }

  ##extract summaries of the optimisations
  status <- data.frame(value=value, convergence=logical(length(res)),
                       conv=(conv==1))
  par.cov <- matrix(NA, dimensions$nparam.cov, length(res))
  par.all <- matrix(NA, dimensions$nparam, length(res))
  for(i in 1:length(res)){
    if( all(!is.na(res[[i]])) ){
      status$convergence[i] <- res[[i]]$convergence==0
      par.cov[,i] <- res[[i]]$par.cov$par
      par.all[,i] <- res[[i]]$par.all$par
    }
  }
  ##add names to the summaries
  rownames(par.all) <- loglikeSTnames(object, all=TRUE)
  rownames(par.cov) <- loglikeSTnames(object, all=FALSE)
  
  ##pick out the converged option with the best value
  Ind.overall <- which.max(value)
  if(any(conv==TRUE)){
    ##mark no-converged values as NA to avoid picking these
    value[!conv] <- NA
  }
  ##extract the best value
  Ind <- which.max(value)
  res.best <- res[[Ind]]
  
  ##collect status results
  summary <- list(status=status, par.all=par.all, par.cov=par.cov, x.fixed=x.fixed)
  
  if(hessian.all==TRUE){
    if(type!="f"){
      x.fixed <- res.best$par.all$fixed
      x <- res.best$par.all$par[ is.na(x.fixed) ]
      res.best$hessian.all <- loglikeSTHessian(x, object, type="f",
                                               x.fixed=x.fixed, h=h)
      ##standard error
      suppressWarnings( par.sd <- sqrt(-diag(solve(res.best$hessian.all))) )
      res.best$par.all$sd <- NA
      res.best$par.all$sd[ is.na(x.fixed) ] <- par.sd
      ##update t-statistic for the best result, all parameters
      res.best$par.all$tstat <- res.best$par.all$par/res.best$par.all$sd
    }else{
      ##replicate hessian for all parameters so output is consistent
      res.best$hessian.all <- res.best$hessian
    }
  }##if(hessian.all==TRUE)
  
  ##return result
  out <- list(res.best=res.best, res.all=res, summary=summary)
  class(out) <- "estimateSTmodel"
  
  return( out )
}##function estimate.STmodel