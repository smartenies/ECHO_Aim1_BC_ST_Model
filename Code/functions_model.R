
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








predict.STmodel <- function (object, x, STdata = NULL, Nmax = 1000, only.pars = FALSE, 
                             nugget.unobs = 0, only.obs = FALSE, pred.var = TRUE, pred.covar = FALSE, 
                             beta.covar = FALSE, combine.data = FALSE, type = "p", LTA = FALSE, 
                             transform = c("none", "unbiased", "mspe"), ...) 
{
    stCheckClass(object, "STmodel", name = "object")
    if (!is.null(STdata)) {
        stCheckClass(STdata, c("STdata", "STmodel"), name = "STdata")
    }
    type <- tolower(type)
    SpatioTemporal:::stCheckType(type)
    transform <- match.arg(transform)
    if (transform == "none") {
        transform <- NULL
    }
    if (inherits(x, "estimateSTmodel")) {
        x <- coef(x, "all")$par
    }
    dimensions <- loglikeSTdim(object)
    if (type == "f" && length(x) != dimensions$nparam) {
        stop(paste("type=f, requires", dimensions$nparam, "parameters but length(x) =", 
                   length(x)))
    }
    if (type != "f" && length(x) == dimensions$nparam) {
        x <- x[(dimensions$nparam - dimensions$nparam.cov + 1):dimensions$nparam]
    }
    if (type != "f" && length(x) != dimensions$nparam.cov) {
        stop(paste("type!=f, requires", dimensions$nparam.cov, 
                   "parameters but length(x) =", length(x)))
    }
    if (only.pars && type == "f") {
        warning("only.pars=TRUE and type=(f)ull only returns KNOWN parameters.", 
                immediate. = TRUE)
    }
    if (only.pars) {
        only.obs <- pred.covar <- beta.covar <- combine.data <- LTA <- FALSE
        transform <- NULL
    }
    else {
        if (only.obs && is.null(STdata)) {
            stop("only.obs=TRUE requires STdata.")
        }
        if (combine.data && is.null(STdata)) {
            warning("No data to combine with; predicting for 'object'", 
                    immediate. = TRUE)
            combine.data <- FALSE
        }
        if (only.obs && combine.data) {
            warning("only.obs=TRUE implies combine.data=FALSE.", 
                    immediate. = TRUE)
            combine.data <- FALSE
        }
        if (pred.covar && !pred.var) {
            warning("pred.covar=TRUE implies pred.var=TRUE.", 
                    immediate. = TRUE)
            pred.var <- TRUE
        }
        if (pred.covar && only.obs) {
            warning("only.obs=TRUE implies pred.covar=FALSE.", 
                    immediate. = TRUE)
            pred.covar <- FALSE
        }
        if (beta.covar && !pred.var) {
            warning("beta.covar=TRUE implies pred.var=TRUE.", 
                    immediate. = TRUE)
            pred.var <- TRUE
        }
        if (!is.null(transform)) {
            if (type != "r" && transform != "unbiased") {
                warning("transform 'unbiased' and 'mspe' are equivalent when type!='r'", 
                        immediate. = TRUE)
                transform <- "unbiased"
            }
            if (pred.covar) {
                warning("transform implies pred.covar=FALSE.", 
                        immediate. = TRUE)
                pred.covar <- FALSE
            }
        }
    }
    if (is.null(STdata)) {
        STdata <- object
    }
    else if (combine.data) {
        STdata <- c(object, STdata)
    }
    else {
        if (is.null(object$trend.fnc) && dim(object$trend)[2] != 
            1) {
            object$trend.fnc <- internalCreateTrendFnc(object$trend)
        }
        if (!inherits(STdata, "STmodel")) {
            if (dimensions$L == 0) {
                STdata$SpatioTemporal <- NULL
            }
            suppressMessages(STdata <- updateTrend(STdata, fnc = object$trend.fnc, 
                                                   extra.dates = STdata$trend$date))
            cov.nu <- object$cov.nu
            cov.nu$nugget <- TRUE
            STdata <- createSTmodel(STdata, LUR = object$LUR.list, 
                                    ST = object$ST.list, cov.beta = object$cov.beta, 
                                    cov.nu = cov.nu, locations = object$locations.list, 
                                    scale = !is.null(object$scale.covars), scale.covars = object$scale.covars)
        }
        else {
            SpatioTemporal:::areSTmodelsConsistent(object, STdata, "STdata")
        }
        suppressMessages(STdata <- SpatioTemporal:::updateTrend.STmodel(STdata, 
                                                       fnc = object$trend.fnc))
    }
    if (!is.list(LTA) && LTA) {
        if (only.obs) {
            LTA.list <- split(STdata$obs$date, STdata$obs$ID)
        }
        else {
            LTA.list <- lapply(STdata$locations$ID, function(x) {
                return(STdata$trend$date)
            })
            names(LTA.list) <- STdata$locations$ID
        }
    }
    else if (is.list(LTA)) {
        LTA.list <- LTA
        LTA <- TRUE
    }
    else {
        LTA.list <- NULL
        LTA <- FALSE
    }
    if (LTA) {
        LTA.missing <- !(names(LTA.list) %in% STdata$locations$ID)
        if (any(LTA.missing)) {
            warning("Removed ", sum(LTA.missing), " elements from LTA not found in STdata$locations$ID", 
                    immediate. = TRUE)
            LTA.list <- LTA.list[!LTA.missing]
        }
        if (length(LTA.list) != 0) {
            LTA.list <- lapply(LTA.list, function(x) {
                switch(is.list(x) + 1, list(x), x)
            })
            LTA.tot <- 0
            for (i in 1:length(LTA.list)) {
                for (j in length(LTA.list[[i]])) {
                    LTA.missing <- !(LTA.list[[i]][[j]] %in% STdata$trend$date)
                    LTA.list[[i]][[j]] <- LTA.list[[i]][[j]][!LTA.missing]
                    LTA.tot <- LTA.tot + sum(LTA.missing)
                }
                LTA.list[[i]] <- LTA.list[[i]][sapply(LTA.list[[i]], 
                                                      length) != 0]
            }
            if (LTA.tot != 0) {
                warning("Removed ", LTA.tot, " dates not found in STdata$trend$date from all LTA elements", 
                        immediate. = TRUE)
            }
            LTA.list <- LTA.list[sapply(LTA.list, length) != 
                                     0]
        }
        if (length(LTA.list) == 0) {
            LTA <- FALSE
            LTA.list <- NULL
        }
    }
    tmp <- loglikeSTgetPars(x, object)
    if (type == "f") {
        gamma <- tmp$gamma
        alpha <- tmp$alpha
    }
    cov.pars.beta <- tmp$cov.beta
    cov.pars.nu <- tmp$cov.nu
    if (type != "f" || !only.pars) {
        nugget.unobs <- SpatioTemporal:::internalFixNuggetUnobs(nugget.unobs, 
                                               STdata, cov.pars.nu$nugget)
        Fobs <- expandF(object$F, object$obs$idx, n.loc = dimensions$n.obs)
        Xtilde <- as.matrix(Fobs %*% bdiag(object$LUR))
        if (dimensions$L != 0) {
            Xtilde <- cbind(object$ST, Xtilde)
        }
        i.sigma.B <- makeSigmaB(cov.pars.beta$pars, dist = object$D.beta, 
                                type = object$cov.beta$covf, nugget = cov.pars.beta$nugget)
        i.sigma.nu <- makeSigmaNu(cov.pars.nu$pars, dist = object$D.nu, 
                                  type = object$cov.nu$covf, nugget = cov.pars.nu$nugget, 
                                  random.effect = cov.pars.nu$random.effect, blocks1 = object$nt, 
                                  ind1 = object$obs$idx, sparse = TRUE)
        i.sigma.B <- makeCholBlock(i.sigma.B, n.blocks = dimensions$m)
        i.sigma.nu <- chol(i.sigma.nu)
        i.sigma.B <- invCholBlock(i.sigma.B, n.blocks = dimensions$m)
        i.sigma.nu <- chol2inv(i.sigma.nu)
        tF.iS <- t(Fobs) %*% i.sigma.nu
        tF.iS.F <- as.matrix(tF.iS %*% Fobs)
        R.i.sigma.B.Y <- tF.iS.F + i.sigma.B
        R.i.sigma.B.Y <- chol(R.i.sigma.B.Y)
        iS.X <- i.sigma.nu %*% Xtilde
        iS.Y <- i.sigma.nu %*% object$obs$obs
        tF.iS.X <- as.matrix(t(Fobs) %*% iS.X)
        tF.iS.Y <- as.matrix(t(Fobs) %*% iS.Y)
        iSBY.tF.iS.X <- solveTriBlock(R.i.sigma.B.Y, tF.iS.X, 
                                      transpose = TRUE)
        iSBY.tF.iS.X <- solveTriBlock(R.i.sigma.B.Y, iSBY.tF.iS.X, 
                                      transpose = FALSE)
    }
    if (type == "f") {
        gamma.E <- c(tmp$gamma)
        alpha.E <- unlist(tmp$alpha)
        gamma.V <- matrix(0, length(gamma.E), length(gamma.E))
        alpha.V <- matrix(0, length(alpha.E), length(alpha.E))
        gamma.alpha.C <- matrix(0, length(gamma.E), length(alpha.E))
        gamma.alpha <- as.matrix(c(gamma.E, alpha.E))
    }
    else {
        i.XSX <- as.matrix(t(Xtilde) %*% iS.X)
        i.XSX <- symmpart(i.XSX - t(tF.iS.X) %*% iSBY.tF.iS.X)
        tmp2 <- as.matrix(object$obs$obs %*% iS.X)
        tmp2 <- tmp2 - t(tF.iS.Y) %*% iSBY.tF.iS.X
        gamma.alpha <- solve(i.XSX, t(tmp2))
        if (dimensions$L != 0) {
            gamma.E <- c(gamma.alpha[1:dimensions$L])
            alpha.E <- c(gamma.alpha[-(1:dimensions$L)])
        }
        else {
            gamma.E <- double(0)
            alpha.E <- c(gamma.alpha)
        }
        i.XSX <- solve(i.XSX)
        if (dimensions$L != 0) {
            gamma.V <- i.XSX[1:dimensions$L, 1:dimensions$L, 
                             drop = FALSE]
            alpha.V <- i.XSX[-c(1:dimensions$L), -c(1:dimensions$L), 
                             drop = FALSE]
            gamma.alpha.C <- i.XSX[1:dimensions$L, -c(1:dimensions$L), 
                                   drop = FALSE]
        }
        else {
            gamma.V <- matrix(0, 0, 0)
            alpha.V <- i.XSX
            gamma.alpha.C <- matrix(0, length(gamma.E), length(alpha.E))
        }
    }
    gamma.E <- as.matrix(gamma.E)
    alpha.E <- as.matrix(alpha.E)
    names.tmp <- loglikeSTnames(object, TRUE)
    names.tmp <- names.tmp[1:(dimensions$nparam - dimensions$nparam.cov)]
    if (dimensions$L != 0) {
        rownames(gamma.E) <- names.tmp[1:dimensions$L]
        colnames(gamma.V) <- rownames(gamma.V) <- rownames(gamma.E)
        rownames(gamma.alpha.C) <- rownames(gamma.E)
        rownames(alpha.E) <- names.tmp[-(1:dimensions$L)]
    }
    else {
        rownames(alpha.E) <- names.tmp
    }
    colnames(alpha.V) <- rownames(alpha.V) <- rownames(alpha.E)
    colnames(gamma.alpha.C) <- rownames(alpha.E)
    out <- list()
    class(out) <- "predictSTmodel"
    out$opts <- list(only.pars = only.pars, nugget.unobs = nugget.unobs, 
                     only.obs = only.obs, pred.var = pred.var, pred.covar = pred.covar, 
                     beta.covar = beta.covar, combine.data = combine.data, 
                     type = type, transform = transform, LTA = LTA, LTA.list = LTA.list)
    out$pars <- list(gamma.E = gamma.E, alpha.E = alpha.E, gamma.V = gamma.V, 
                     alpha.V = alpha.V, gamma.alpha.C = gamma.alpha.C)
    if (out$opts$only.pars) {
        return(out)
    }
    rm(gamma.E, alpha.E, gamma.V, alpha.V, gamma.alpha.C, names.tmp)
    rm(only.pars, nugget.unobs, only.obs, pred.var, pred.covar, 
       beta.covar, combine.data, type, LTA, LTA.list, transform)
    tF.iS.C <- tF.iS.Y - tF.iS.X %*% gamma.alpha
    iS.C <- iS.Y - iS.X %*% gamma.alpha
    iSBY.tF.iS.C <- solveTriBlock(R.i.sigma.B.Y, tF.iS.C, transpose = TRUE)
    iSBY.tF.iS.C <- solveTriBlock(R.i.sigma.B.Y, iSBY.tF.iS.C, 
                                  transpose = FALSE)
    iSoo.C <- as.matrix(iS.C - t(tF.iS) %*% iSBY.tF.iS.C)
    if (out$opts$type == "r") {
        iSoo.Xtilde <- as.matrix(iS.X - t(tF.iS) %*% iSBY.tF.iS.X)
    }
    rm(iS.X, iS.Y, tF.iS.X, tF.iS.Y, iSBY.tF.iS.X, tF.iS.C, iS.C, 
       iSBY.tF.iS.C)
    if (out$opts$only.obs) {
        idx.unobs <- STdata$obs$idx
        T1 <- STdata$obs$date
        Funobs <- STdata$F
        if (dimensions$L != 0) {
            ST.unobs <- STdata$ST
        }
        N.unobs <- length(unique(idx.unobs))
        T.unobs <- length(unique(T1))
    }
    else {
        N.unobs <- dim(STdata$locations)[1]
        T.unobs <- dim(STdata$trend)[1]
        idx.unobs <- rep(1:N.unobs, each = T.unobs)
        T1 <- rep(STdata$trend$date, N.unobs)
        Funobs <- matrix(1, (T.unobs * N.unobs), dimensions$m)
        for (i in (1:dimensions$m)) {
            if (colnames(STdata$F)[i] != "const") {
                Funobs[, i] <- rep(STdata$trend[, colnames(STdata$F)[i]], 
                                   N.unobs)
            }
        }
        if (dimensions$L != 0) {
            ST.unobs <- matrix(STdata$ST.all, (T.unobs * N.unobs), 
                               dimensions$L)
        }
    }
    date.all <- sort(unique(c(object$trend$date, STdata$trend$date)))
    nt.unobs <- nt.obs <- double(length(date.all))
    for (i in c(1:length(date.all))) {
        nt.obs[i] <- sum(object$obs$date == date.all[i])
    }
    Funobs <- expandF(Funobs, idx.unobs, n.loc = N.unobs)
    Xtilde.unobs <- as.matrix(Funobs %*% bdiag(STdata$LUR.all))
    if (dimensions$L != 0) {
        Xtilde.unobs <- cbind(ST.unobs, Xtilde.unobs)
    }
    out$beta <- list()
    out$beta$mu <- matrix(bdiag(STdata$LUR.all) %*% out$pars$alpha.E, 
                          ncol = length(STdata$LUR.all))
    colnames(out$beta$mu) <- names(STdata$LUR.all)
    rownames(out$beta$mu) <- rownames(STdata$LUR.all[[1]])
    loc.unobs.nu <- STdata$locations[, c("x.nu", "y.nu"), drop = FALSE]
    loc.unobs.beta <- STdata$locations[, c("x.beta", "y.beta"), 
                                       drop = FALSE]
    I.obs <- match(colnames(object$D.nu), object$locations$ID)
    loc.obs.nu <- object$locations[I.obs, c("x.nu", "y.nu"), 
                                   drop = FALSE]
    loc.obs.beta <- object$locations[I.obs, c("x.beta", "y.beta"), 
                                     drop = FALSE]
    Ind.2.1 <- match(object$locations$ID[I.obs], STdata$locations$ID, 
                     nomatch = 0)
    sigma.B.C <- makeSigmaB(cov.pars.beta$pars, dist = crossDist(loc.unobs.beta, 
                                                                 loc.obs.beta), type = object$cov.beta$covf, nugget = cov.pars.beta$nugget, 
                            ind2.to.1 = Ind.2.1, sparse = TRUE)
    out$beta$EX <- out$beta$mu + matrix(sigma.B.C %*% (t(Fobs) %*% 
                                                           iSoo.C), ncol = dim(out$beta$mu)[2])
    if (out$opts$pred.var) {
        Sby.iSb.Sou <- as.matrix(i.sigma.B %*% t(sigma.B.C))
        Sby.iSb.Sou <- solveTriBlock(R.i.sigma.B.Y, Sby.iSb.Sou, 
                                     transpose = TRUE)
        Sby.iSb.Sou <- solveTriBlock(R.i.sigma.B.Y, Sby.iSb.Sou, 
                                     transpose = FALSE)
        if (out$opts$type == "r") {
            var.beta.REML <- (cbind(rep(0, dimensions$L), bdiag(STdata$LUR.all)) - 
                                  sigma.B.C %*% (t(Fobs) %*% iSoo.Xtilde))
            var.beta.REML <- as.matrix(var.beta.REML)
            if (out$opts$beta.covar) {
                V.REML <- (var.beta.REML %*% i.XSX) %*% t(var.beta.REML)
            }
            else {
                V.REML <- rowSums((var.beta.REML %*% i.XSX) * 
                                      var.beta.REML)
            }
        }
        else {
            V.REML <- 0
        }
        if (out$opts$beta.covar) {
            sigma.B.uu <- makeSigmaB(cov.pars.beta$pars, dist = crossDist(loc.unobs.beta), 
                                     type = object$cov.beta$covf, nugget = cov.pars.beta$nugget)
            tmp <- sigma.B.uu - sigma.B.C %*% tF.iS.F %*% Sby.iSb.Sou + 
                V.REML
            out$beta$VX.full <- list()
            for (i in 1:dim(out$beta$EX)[2]) {
                Ind <- (1:dim(out$beta$EX)[1]) + (i - 1) * dim(out$beta$EX)[1]
                out$beta$VX.full[[i]] <- as.matrix(tmp[Ind, Ind, 
                                                       drop = FALSE])
                rownames(out$beta$VX.full[[i]]) <- rownames(out$beta$EX)
                colnames(out$beta$VX.full[[i]]) <- rownames(out$beta$EX)
            }
            names(out$beta$VX.full) <- colnames(out$beta$EX)
            tmp <- diag(tmp)
        }
        else {
            sigma.B.uu <- makeSigmaB(cov.pars.beta$pars, dist = matrix(0, 
                                                                       1, 1), type = object$cov.beta$covf, nugget = cov.pars.beta$nugget)
            sigma.B.uu <- matrix(diag(sigma.B.uu), ncol = dim(sigma.B.uu)[1], 
                                 nrow = dim(loc.unobs.beta)[1], byrow = TRUE)
            tmp <- (c(sigma.B.uu) - rowSums(sigma.B.C * t(tF.iS.F %*% 
                                                              Sby.iSb.Sou)) + V.REML)
        }
        out$beta$VX <- matrix(tmp, ncol = dim(out$beta$EX)[2])
        dimnames(out$beta$VX) <- dimnames(out$beta$EX)
        rm(tmp, Sby.iSb.Sou, sigma.B.uu, V.REML)
    }
    rm(i.sigma.B, tF.iS.F)
    out$EX.mu <- as.matrix(Xtilde.unobs %*% gamma.alpha)
    out$EX.mu.beta <- as.matrix(Funobs %*% c(out$beta$EX))
    if (dimensions$L != 0) {
        out$EX.mu.beta <- out$EX.mu.beta + (ST.unobs %*% out$pars$gamma.E)
    }
    if (!out$opts$only.obs) {
        dim(out$EX.mu.beta) <- dim(out$EX.mu) <- c(T.unobs, N.unobs)
        colnames(out$EX.mu) <- STdata$locations$ID
        rownames(out$EX.mu) <- as.character(STdata$trend$date)
        dimnames(out$EX.mu.beta) <- dimnames(out$EX.mu)
    }
    else {
        dimnames(out$EX.mu.beta) <- dimnames(out$EX.mu) <- NULL
    }
    out$EX <- out$EX.mu
    if (!is.null(out$opts$transform)) {
        EX.trans <- out$EX.mu
        EX.trans.pred <- out$EX.mu
        out$log.EX <- out$EX.pred <- NA
    }
    if (out$opts$pred.var || !is.null(out$opts$transform)) {
        out$VX <- matrix(NA, dim(out$EX)[1], dim(out$EX)[2])
        out$VX.pred <- matrix(NA, dim(out$EX)[1], dim(out$EX)[2])
        if (!out$opts$only.obs) {
            dimnames(out$VX) <- dimnames(out$VX.pred) <- dimnames(out$EX)
        }
        if (out$opts$pred.covar) {
            out$VX.full <- list()
        }
        if (out$opts$pred.var && !is.null(out$opts$transform)) {
            out$MSPE <- out$MSPE.pred <- out$VX
        }
    }
    if (out$opts$LTA) {
        out$LTA <- vector("list", length(out$opts$LTA.list))
        names(out$LTA) <- names(out$opts$LTA.list)
    }
    cross.D.nu <- crossDist(loc.unobs.nu, loc.obs.nu)
    sigma.B.C.tF <- sigma.B.C %*% t(Fobs)
    if (out$opts$pred.covar || out$opts$LTA) {
        Ind.list <- split(1:length(out$EX), idx.unobs)
    }
    else {
        Ind.list <- split(1:length(out$EX), rep(1:ceiling(length(out$EX)/Nmax), 
                                                each = Nmax)[1:length(out$EX)])
    }
    for (i in 1:length(Ind.list)) {
        Ind <- Ind.list[[i]]
        T1.Ind <- T1[Ind]
        for (j in c(1:length(date.all))) {
            nt.unobs[j] <- sum(T1.Ind == date.all[j])
        }
        T1.order <- order(T1.Ind)
        sigma.B.full.C <- as.matrix(Funobs[Ind, , drop = FALSE] %*% 
                                        sigma.B.C.tF)
        sigma.nu.C <- makeSigmaNu(cov.pars.nu$pars, dist = cross.D.nu, 
                                  type = object$cov.nu$covf, nugget = 0, random.effect = cov.pars.nu$random.effect, 
                                  ind1 = (idx.unobs[Ind])[T1.order], ind2 = object$obs$idx, 
                                  blocks1 = nt.unobs, blocks2 = nt.obs, ind2.to.1 = Ind.2.1)
        sigma.nu.C[T1.order, ] <- sigma.nu.C
        sigma.nu.C <- sigma.nu.C + sigma.B.full.C
        out$EX[Ind] <- out$EX.mu[Ind] + sigma.nu.C %*% iSoo.C
        if (out$opts$LTA) {
            ID.tmp <- STdata$locations$ID[unique(idx.unobs[Ind])]
            if (length(ID.tmp) != 1) {
                stop("Something wrong with LTA-computations.")
            }
            LTA.tmp <- out$opts$LTA.list[[ID.tmp]]
        }
        else {
            LTA.tmp <- NULL
        }
        if (out$opts$pred.var || !is.null(out$opts$transform)) {
            I.loc <- sort(unique(idx.unobs[Ind]))
            I.loc2 <- (rep(I.loc, dimensions$m) + rep((0:(dimensions$m - 
                                                              1)) * N.unobs, each = length(I.loc)))
            unobs.D.beta <- crossDist(loc.unobs.beta[I.loc, , 
                                                     drop = FALSE])
            sigma.B.uu <- makeSigmaB(cov.pars.beta$pars, dist = unobs.D.beta, 
                                     type = object$cov.beta$covf, nugget = cov.pars.beta$nugget, 
                                     sparse = TRUE)
            V.uu <- (Funobs[Ind, I.loc2, drop = FALSE] %*% Matrix(sigma.B.uu %*% 
                                                                      t(Funobs[Ind, I.loc2, drop = FALSE])))
            unobs.D.nu <- crossDist(loc.unobs.nu[I.loc, , drop = FALSE])
            V.uu <- V.uu + makeSigmaNu(cov.pars.nu$pars, dist = unobs.D.nu, 
                                       type = object$cov.nu$covf, nugget = 0, random.effect = cov.pars.nu$random.effect, 
                                       ind1 = idx.unobs[Ind] - min(I.loc) + 1, blocks1 = nt.unobs)
            iS.Sou <- i.sigma.nu %*% t(sigma.nu.C)
            tF.iS.Sou <- as.matrix(tF.iS %*% t(sigma.nu.C))
            Sby.tF.iS.Sou <- solveTriBlock(R.i.sigma.B.Y, tF.iS.Sou, 
                                           transpose = TRUE)
            if (out$opts$type == "r") {
                tmp <- Xtilde.unobs[Ind, , drop = FALSE] - sigma.nu.C %*% 
                    iSoo.Xtilde
                if (out$opts$pred.covar || !is.null(LTA.tmp)) {
                    V.REML <- (tmp %*% i.XSX) %*% t(tmp)
                    if (!is.null(out$opts$transform)) {
                        lambda.full <- (tmp %*% i.XSX) %*% t(Xtilde.unobs[Ind, 
                                                                          , drop = FALSE])
                        lambda <- diag(lambda.full)
                    }
                }
                else {
                    V.REML <- rowSums((tmp %*% i.XSX) * tmp)
                    if (!is.null(out$opts$transform)) {
                        lambda <- rowSums((tmp %*% i.XSX) * (Xtilde.unobs[Ind, 
                                                                          , drop = FALSE]))
                    }
                }
            }
            else {
                V.REML <- 0
                lambda <- 0
            }
            if (out$opts$pred.covar || !is.null(LTA.tmp)) {
                tmp <- -sigma.nu.C %*% iS.Sou + t(Sby.tF.iS.Sou) %*% 
                    Sby.tF.iS.Sou
                V.cond <- V.cond.0 <- as.matrix(V.uu + tmp + 
                                                    V.REML)
                diag(V.cond) <- diag(V.cond) + out$opts$nugget.unobs[idx.unobs[Ind]]
                out$VX[Ind] <- diag(V.cond.0)
                out$VX.pred[Ind] <- diag(V.cond)
                if (out$opts$pred.covar) {
                    out$VX.full[[i]] <- V.cond.0
                }
            }
            else {
                tmp <- (-colSums(t(sigma.nu.C) * iS.Sou) + colSums(Sby.tF.iS.Sou * 
                                                                       Sby.tF.iS.Sou))
                out$VX[Ind] <- diag(V.uu) + tmp + V.REML
                out$VX.pred[Ind] <- out$VX[Ind] + out$opts$nugget.unobs[idx.unobs[Ind]]
            }
            out$VX[Ind] <- pmax(out$VX[Ind], 0)
            out$VX.pred[Ind] <- pmax(out$VX.pred[Ind], 0)
        }
        else {
            V.cond <- V.cond.0 <- NULL
        }
        if (!is.null(out$opts$transform)) {
            if (out$opts$transform == "unbiased") {
                c <- 1
            }
            else {
                c <- 2
            }
            EX.trans[Ind] <- exp(out$EX[Ind] + out$VX[Ind]/2 - 
                                     c * lambda)
            EX.trans.pred[Ind] <- exp(out$EX[Ind] + out$VX.pred[Ind]/2 - 
                                          c * lambda)
            if (out$opts$pred.var) {
                if (out$opts$transform == "mspe") {
                    EX.ub <- exp(out$EX[Ind] + out$VX[Ind]/2 - 
                                     lambda)
                    EX.ub.pred <- exp(out$EX[Ind] + out$VX.pred[Ind]/2 - 
                                          lambda)
                }
                else {
                    EX.ub <- EX.trans[Ind]
                    EX.ub.pred <- EX.trans.pred[Ind]
                }
                V.uu.pred <- V.uu + diag(out$opts$nugget.unobs[idx.unobs[Ind]])
                if (!is.null(LTA.tmp)) {
                    if (out$opts$transform == "unbiased" && out$opts$type == 
                        "r") {
                        Z <- exp(lambda.full)
                        Z <- Z + t(Z) - exp(lambda.full + t(lambda.full))
                    }
                    else {
                        Z <- 1
                    }
                    MSPE.part <- exp(V.uu) * (1 - exp(-V.cond.0) * 
                                                  Z)
                    MSPE.part.pred <- exp(V.uu.pred) * (1 - exp(-V.cond) * 
                                                            Z)
                    out$MSPE[Ind] <- (EX.ub * EX.ub * diag(MSPE.part))
                    out$MSPE.pred[Ind] <- (EX.ub.pred * EX.ub.pred * 
                                               diag(MSPE.part.pred))
                }
                else {
                    if (out$opts$transform == "unbiased" && out$opts$type == 
                        "r") {
                        Z <- 2 * exp(lambda) - exp(2 * lambda)
                    }
                    else {
                        Z <- 1
                    }
                    out$MSPE[Ind] <- (EX.ub * EX.ub * exp(diag(V.uu)) * 
                                          (1 - exp(-out$VX[Ind]) * Z))
                    out$MSPE.pred[Ind] <- (EX.ub.pred * EX.ub.pred * 
                                               exp(diag(V.uu.pred)) * (1 - exp(-out$VX.pred[Ind]) * 
                                                                           Z))
                }
                out$MSPE[Ind] <- pmax(out$MSPE[Ind], 0)
                out$MSPE.pred[Ind] <- pmax(out$MSPE.pred[Ind], 
                                           0)
            }
            else {
                MSPE.part <- MSPE.part.pred <- NULL
            }
            if (!is.null(LTA.tmp)) {
                EX.tmp <- cbind(exp(out$EX.mu[Ind]), exp(out$EX.mu.beta[Ind]), 
                                EX.trans[Ind], EX.trans.pred[Ind])
                colnames(EX.tmp) <- c("EX.mu", "EX.mu.beta", 
                                      "EX", "EX.pred")
                out$LTA[[ID.tmp]] <- internalComputeLTA(LTA.tmp, 
                                                        EX.tmp, T1[Ind], V = MSPE.part, V.pred = MSPE.part.pred, 
                                                        E.vec = EX.ub, E.vec.pred = EX.ub.pred)
            }
        }
        else {
            if (!is.null(LTA.tmp)) {
                EX.tmp <- cbind(out$EX.mu[Ind], out$EX.mu.beta[Ind], 
                                out$EX[Ind])
                colnames(EX.tmp) <- c("EX.mu", "EX.mu.beta", 
                                      "EX")
                out$LTA[[ID.tmp]] <- internalComputeLTA(LTA.tmp, 
                                                        EX.tmp, T1[Ind], V = V.cond.0, V.pred = V.cond)
            }
        }
    }
    if (out$opts$LTA) {
        out$LTA <- do.call(cbind, out$LTA)
        tmp <- sapply(out$opts$LTA.list, length)
        if (all(tmp == 1)) {
            colnames(out$LTA) <- names(tmp)
        }
        else {
            colnames(out$LTA) <- paste(rep(names(tmp), times = tmp), 
                                       unlist(lapply(tmp, seq)), sep = ".")
        }
        out$LTA <- as.data.frame(t(out$LTA))
    }
    if (!is.null(out$opts$transform)) {
        out$log.EX <- out$EX
        out$EX <- EX.trans
        out$EX.pred <- EX.trans.pred
        out$EX.mu <- exp(out$EX.mu)
        out$EX.mu.beta <- exp(out$EX.mu.beta)
        I <- grep("^VX", names(out))
        names(out)[I] <- paste("log.", names(out)[I], sep = "")
    }
    if (out$opts$pred.covar) {
        names(out$VX.full) <- colnames(out$EX)
        for (i in 1:length(out$VX.full)) colnames(out$VX.full[[i]]) <- rownames(out$VX.full[[i]]) <- rownames(out$EX)
    }
    if (length(STdata$obs$obs) != 0) {
        if (out$opts$only.obs) {
            I <- 1:length(out$EX)
        }
        else {
            I <- (match(STdata$obs$ID, colnames(out$EX)) - 1) * 
                dim(out$EX)[1] + match(STdata$obs$date, STdata$trend$date)
        }
        out$I <- data.frame(I = I, date = STdata$obs$date, ID = STdata$obs$ID, 
                            stringsAsFactors = FALSE)
    }
    return(out)
}

predictCV.STmodel <- function (object, x, Ind.cv = NULL, ..., silent = TRUE, LTA = FALSE) 
{
    stCheckClass(object, "STmodel", name = "object")
    if (inherits(x, "estCVSTmodel")) {
        if (missing(Ind.cv) || is.null(Ind.cv)) {
            Ind.cv <- x$Ind.cv
        }
        x <- coef(x, "all")
    }
    else if (inherits(x, "estimateSTmodel")) {
        x <- coef(x, "all")$par
    }
    Ind.cv <- SpatioTemporal:::stCheckInternalCV(Ind.cv)
    Ind.cv <- as.matrix(Ind.cv)
    if (dim(Ind.cv)[2] == 1) {
        N.CV.sets <- max(Ind.cv, na.rm = TRUE)
    }
    else {
        stop("Some observation(s) are left out in several Cv-groups.")
    }
    x <- as.matrix(x)
    if (dim(x)[2] == 1) {
        x <- matrix(x, length(x), N.CV.sets)
    }
    else if (dim(x)[2] != N.CV.sets) {
        stop("Number of parameters does not match the number of cv-sets.")
    }
    pred <- list()
    for (i in 1:N.CV.sets) {
        if (!silent) 
            message(sprintf("Predicting cv-set %d/%d", i, N.CV.sets))
        if (dim(Ind.cv)[2] == 1) {
            Ind.current <- Ind.cv == i
        }
        else {
            Ind.current <- as.logical(Ind.cv[, i])
        }
        object.obs <- dropObservations(object, Ind.current)
        suppressWarnings(object.pred <- dropObservations(object, 
                                                         !Ind.current))
        nugget.unobs <- loglikeSTgetPars(x[, i], object)$cov.nu$nugget
        nugget.unobs <- nugget.unobs[object.pred$locations$ID, 
                                     , drop = FALSE]
        if (LTA) {
            LTA.pred <- with(object.pred$obs, split(date, ID))
        }
        else {
            LTA.pred <- FALSE
        }
        # pred[[i]] <- predict(object.obs, x[, i], STdata = object.pred, 
                             # nugget.unobs = nugget.unobs, only.pars = FALSE, combine.data = FALSE, 
                             # LTA = LTA.pred, ...)
        # J Keller edit to make this call the version of predict() that calls the corrected LTA code
        pred[[i]] <- predict.STmodel(object.obs, x[, i], STdata = object.pred, 
                             nugget.unobs = nugget.unobs, only.pars = FALSE, combine.data = FALSE, 
                             LTA = LTA.pred, ...)
    }
    out <- list()
    class(out) <- "predCVSTmodel"
    out$opts <- pred[[1]]$opts
    if (!is.null(out$opts$nugget.unobs)) {
        out$opts$nugget.unobs <- unlist(sapply(pred, function(x) {
            x$opts$nugget.unobs
        }))
        names(out$opts$nugget.unobs) <- unlist(sapply(pred, function(x) {
            rownames(x$opts$nugget.unobs)
        }))
    }
    if (out$opts$LTA) {
        out$opts$LTA.list <- unlist(lapply(pred, function(x) {
            x$opts$LTA.list
        }), recursive = FALSE)
    }
    else {
        out$opts$LTA.list <- NULL
    }
    out$Ind.cv <- Ind.cv
    out$pred.obs <- object$obs[, c("obs", "date", "ID"), drop = FALSE]
    if (!is.null(out$opts$transform)) {
        out$pred.obs$obs <- exp(out$pred.obs$obs)
    }
    EX.names <- names(pred[[1]])[grep("EX", names(pred[[1]]))]
    for (i in EX.names) {
        out$pred.obs[[i]] <- NA
    }
    if (out$opts$pred.var || !is.null(out$opts$transform)) {
        VX.names <- names(pred[[1]])[grep("VX|MSPE", names(pred[[1]]))]
        for (i in VX.names) {
            out$pred.obs[[i]] <- NA
        }
    }
    else {
        VX.names <- NULL
    }
    for (i in 1:N.CV.sets) {
        Ind.current <- Ind.cv == i
        I <- pred[[i]]$I$I
        out$pred.obs[Ind.current, EX.names] <- sapply(pred[[i]][EX.names], 
                                                      function(x) {
                                                          x[I]
                                                      })
        if (!is.null(VX.names)) {
            out$pred.obs[Ind.current, VX.names] <- sapply(pred[[i]][VX.names], 
                                                          function(x) {
                                                              x[I]
                                                          })
        }
    }
    out$pred.obs$res <- out$pred.obs$obs - out$pred.obs$EX
    if (out$opts$pred.var && is.null(out$opts$transform)) {
        out$pred.obs$res.norm <- out$pred.obs$res/sqrt(out$pred.obs$VX.pred)
    }
    if (out$opts$LTA) {
        out$pred.LTA <- do.call(rbind, lapply(pred, function(x) {
            x$LTA
        }))
        LTA.mean <- sapply(with(out$pred.obs, split(obs, ID)), 
                           mean)
        out$pred.LTA <- cbind(LTA.mean[match(rownames(out$pred.LTA), 
                                             names(LTA.mean))], rownames(out$pred.LTA), out$pred.LTA)
        names(out$pred.LTA)[1:2] <- c("obs", "ID")
        out$pred.LTA$ID <- as.character(out$pred.LTA$ID)
        rownames(out$pred.LTA) <- NULL
        if ("EX.pred" %in% names(out$pred.LTA)) {
            out$pred.LTA$res <- out$pred.LTA$obs - out$pred.LTA$EX.pred
        }
        else {
            out$pred.LTA$res <- out$pred.LTA$obs - out$pred.LTA$EX
        }
        if (out$opts$pred.var) {
            out$pred.LTA$res.norm <- out$pred.LTA$res/sqrt(out$pred.LTA$VX.pred)
        }
    }
    if (out$opts$only.obs) {
        any.duplicates <- any(duplicated(unlist(sapply(pred, 
                                                       function(x) {
                                                           unique(x$I$ID)
                                                       }))))
    }
    else {
        any.duplicates <- any(duplicated(unlist(sapply(pred, 
                                                       function(x) {
                                                           colnames(x$EX)
                                                       }))))
    }
    if (!any.duplicates) {
        out$pred.all <- list()
        out$pred.all$EX.mu <- matrix(NA, dim(object$trend)[1], 
                                     dim(object$locations)[1])
        colnames(out$pred.all$EX.mu) <- object$locations$ID
        rownames(out$pred.all$EX.mu) <- as.character(object$trend$date)
        for (i in EX.names[EX.names != "EX.mu"]) {
            out$pred.all[[i]] <- out$pred.all$EX.mu
        }
        if (!is.null(VX.names)) {
            for (i in VX.names) {
                out$pred.all[[i]] <- out$pred.all$EX.mu
            }
        }
        out$pred.all$beta <- list()
        out$pred.all$beta$mu <- matrix(NA, dim(object$locations)[1], 
                                       length(object$LUR))
        colnames(out$pred.all$beta$mu) <- names(object$LUR)
        rownames(out$pred.all$beta$mu) <- object$locations$ID
        out$pred.all$beta$EX <- out$pred.all$beta$mu
        if (out$opts$pred.var) {
            out$pred.all$beta$VX <- out$pred.all$beta$EX
        }
        for (i in 1:N.CV.sets) {
            if (out$opts$only.obs) {
                ID.names <- rownames(pred[[i]]$beta$EX)
                I <- ((match(pred[[i]]$I$ID, object$locations$ID) - 
                           1) * dim(out$pred.all$EX)[1] + match(as.character(pred[[i]]$I$date), 
                                                                rownames(out$pred.all$EX)))
            }
            else {
                ID.names <- colnames(pred[[i]]$EX)
                I <- (rep(match(ID.names, object$locations$ID) - 
                              1, each = dim(out$pred.all$EX)[1]) * dim(out$pred.all$EX)[1] + 
                          rep(match(rownames(pred[[i]]$EX), rownames(out$pred.all$EX)), 
                              length(ID.names)))
            }
            for (j in EX.names) {
                out$pred.all[[j]][I] <- pred[[i]][[j]]
            }
            if (!is.null(VX.names)) {
                for (j in VX.names) {
                    out$pred.all[[j]][I] <- pred[[i]][[j]]
                }
            }
            out$pred.all$beta$mu[ID.names, ] <- pred[[i]]$beta$mu
            out$pred.all$beta$EX[ID.names, ] <- pred[[i]]$beta$EX
            if (out$opts$pred.var) {
                out$pred.all$beta$VX[ID.names, ] <- pred[[i]]$beta$VX
            }
        }
    }
    else {
        out$pred.all.by.cv <- vector("list", N.CV.sets)
        for (i in 1:N.CV.sets) {
            if (out$opts$only.obs) {
                out$pred.all.by.cv[[i]] <- list()
                EX.tmp <- vector("list", length(EX.names))
                names(EX.tmp) <- EX.names
                for (j in EX.names) {
                    EX.tmp[[j]] <- createDataMatrix(obs = pred[[i]][[j]], 
                                                    date = pred[[i]]$I$date, ID = pred[[i]]$I$ID)
                }
                if (!is.null(VX.names)) {
                    VX.tmp <- vector("list", length(VX.names))
                    names(VX.tmp) <- VX.names
                    for (j in VX.names) {
                        VX.tmp[[j]] <- createDataMatrix(obs = pred[[i]][[j]], 
                                                        date = pred[[i]]$I$date, ID = pred[[i]]$I$ID)
                    }
                }
                out$pred.all.by.cv[[i]][[EX.names[1]]] <- matrix(NA, 
                                                                 dim(object$trend)[1], dim(EX.tmp[[1]])[2])
                colnames(out$pred.all.by.cv[[i]][[EX.names[1]]]) <- colnames(EX.tmp[[1]])
                rownames(out$pred.all.by.cv[[i]][[EX.names[1]]]) <- as.character(object$trend$date)
                for (j in EX.names[-1]) {
                    out$pred.all.by.cv[[i]][[j]] <- out$pred.all.by.cv[[i]][[EX.names[1]]]
                }
                if (!is.null(VX.names)) {
                    for (j in VX.names) {
                        out$pred.all.by.cv[[i]][[j]] <- out$pred.all.by.cv[[i]][[EX.names[1]]]
                    }
                }
                for (j in EX.names) {
                    out$pred.all.by.cv[[i]][[j]][rownames(EX.tmp[[j]]), 
                    ] <- EX.tmp[[j]]
                }
                if (!is.null(VX.names)) {
                    for (j in VX.names) {
                        out$pred.all.by.cv[[i]][[j]][rownames(VX.tmp[[j]]), 
                        ] <- VX.tmp[[j]]
                    }
                }
                out$pred.all.by.cv[[i]]$beta <- pred[[i]]$beta
            }
            else {
                pick.names <- c(EX.names, VX.names, "beta")
                out$pred.all.by.cv[[i]] <- pred[[i]][pick.names]
            }
        }
    }
    return(out)
}



internalComputeLTA <- function (LTA, EX, T, V = NULL, V.pred = NULL, E.vec = NULL, 
          E.vec.pred = NULL) 
{
    if (is.null(V)) {
        res <- matrix(NA, dim(EX)[2], length(LTA))
        rownames(res) <- colnames(EX)
    }
    else {
        res <- matrix(NA, dim(EX)[2] + 2, length(LTA))
        rownames(res) <- c(colnames(EX), "VX", "VX.pred")
        if (is.null(E.vec)) {
            E.vec <- rep(1, dim(V)[1])
        }
        if (is.null(E.vec.pred)) {
            E.vec.pred <- E.vec
        }
    }
    for (j in 1:length(LTA)) {
        Ind.LTA <- T %in% LTA[[j]]
        # LTA.tmp.res <- colMeans(EX[Ind.LTA,])
        # J Keller edit to prevent error for a site with 1 obs
        LTA.tmp.res <- colMeans(EX[Ind.LTA, , drop=FALSE])
        if (!is.null(V)) {
            LTA.tmp.res <- c(LTA.tmp.res, sum(E.vec[Ind.LTA] * 
                                                  (V[Ind.LTA, Ind.LTA, drop = FALSE] %*% E.vec[Ind.LTA]))/sum(Ind.LTA)^2, 
                             sum(E.vec.pred[Ind.LTA] * (V.pred[Ind.LTA, Ind.LTA, 
                                                               drop = FALSE] %*% E.vec.pred[Ind.LTA]))/sum(Ind.LTA)^2)
        }
        res[, j] <- LTA.tmp.res
    }
    return(res)
}
