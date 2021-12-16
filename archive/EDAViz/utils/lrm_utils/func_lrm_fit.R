lrm.fit <- function(x, y, offset=0, initial, est,
                    maxit=12, eps=.025, tol=1E-7, trace=FALSE,
                    penalty.matrix=NULL, weights=NULL, normwt=FALSE,
                    scale=FALSE)
{
  cal <- match.call()
  opts <- double(12)
  opts[1:4] <- c(tol, eps, maxit, trace)
  len.penmat <- length(penalty.matrix)
  
  n <- length(y)
  
  wtpres <- TRUE
  if(!length(weights)) {
    wtpres <- FALSE
    normwt <- FALSE
    weights <- rep(1, n)
  }
  if(length(weights) != n) stop('length of wt must equal length of y')
  if(normwt) weights <- weights * n / sum(weights)
  storage.mode(weights) <- 'double'
  opts[12] <- normwt
  
  initial.there <- !missing(initial)
  if(missing(x) || length(x) == 0) {
    nx <- 0
    xname <- NULL
    if(!missing(est))stop("est was given without x")
    est <- NULL
    x <- 0
  } else {
    if(! is.matrix(x)) x <- as.matrix(x)
    dx <- dim(x)
    nx <- dx[2]
    if(dx[1] != n)stop("x and y must have same length")
    if(scale) {
      x <- scale(x)
      scinfo <- attributes(x)[c('scaled:center', 'scaled:scale')]
      xbar <- as.matrix(scinfo[[1]])
      xsd  <- as.matrix(scinfo[[2]])
    }
    
    storage.mode(x) <- "double"
    
    if(missing(est)) est <- 1:nx
    else
      if(length(est)) {
        estr <- range(est)
        if(estr[1] < 1 | estr[2] > nx)
          stop("est has illegal column number for x")
        if(anyDuplicated(est)) stop("est has duplicates")
        storage.mode(est) <- "integer"
      }
    xname <- dimnames(x)[[2]]
    if(length(xname)==0) xname <- paste("x[",1:nx,"]",sep="")
  }
  
  nxin <- length(est)
  
  if(! is.factor(y)) y <- as.factor(y)
  y <- unclass(y)   # in case is.factor
  ylevels <- levels(y)
  
  ofpres <- !all(offset == 0)
  opts[5] <- ofpres
  if(ofpres && length(offset) != n) stop("offset and y must have same length")
  storage.mode(offset) <- "double"
  
  if(n < 3) stop("must have >=3 non-missing observations")
  kint <- as.integer(length(ylevels) - 1)
  ftable <- integer(5001 * (kint + 1))
  levels(y) <- ylevels
  numy <- table(y)
  names(numy) <- ylevels
  y <- as.integer(y - 1)
  nvi <- as.integer(nxin + kint)
  
  sumwty <- tapply(weights, y, sum)
  sumwt  <- sum(sumwty)
  if(!wtpres && any(numy != sumwty)) stop('program logic error 1')
  sumw <- if(normwt) numy else as.integer(round(sumwty))
  
  if(missing(initial)) {
    ncum <- rev(cumsum(rev(sumwty)))[2 : (kint + 1)]
    pp   <- ncum/sumwt
    initial <- log(pp / (1 - pp))
    if(ofpres) initial <- initial - mean(offset)
  }
  if(length(initial) < nvi)
    initial <- c(initial, rep(0, nvi - length(initial)))
  storage.mode(initial) <- "double"
  
  loglik <- -2 * sum(sumwty*logb(sumwty/sum(sumwty)))
  ## loglik <-  -2 * sum(numy * logb(numy/n))
  
  if(nxin > 0) {
    if(len.penmat == 0) penalty.matrix <- matrix(0, nrow=nx, ncol=nx)
    if(nrow(penalty.matrix) != nx || ncol(penalty.matrix) != nx) 
      stop(paste("penalty.matrix does not have", nx, "rows and columns"))
    penmat <- rbind(
      matrix(0, ncol=kint+nx, nrow=kint),
      cbind(matrix(0, ncol=kint, nrow=nx), penalty.matrix))
  }
  else
    penmat <- matrix(0, ncol=kint, nrow=kint)
  storage.mode(penmat) <- 'double'
  
  if(nxin == 0 & ! ofpres) {
    loglik <- rep(loglik,2)
    z <- list(coef=initial, u=rep(0,kint),
              opts=as.double(c(rep(0,7), .5, 0, 0, 0, 0)))
  }
  
  if(ofpres) {
    ##Fit model with only intercept(s) and offset
    z <- 
      .Fortran(F_lrmfit, coef=initial, as.integer(0), as.integer(0),
               x, y, offset,
               u=double(kint),
               double(kint*(kint+1)/2),loglik=double(1), n, as.integer(0),
               sumw, kint,
               v=double(kint*kint), double(kint), double(2*kint),
               double(kint), pivot=integer(kint), opts=opts, ftable,
               penmat, weights)
    
    loglik <- c(loglik,z$loglik)
    if(z$opts[6] | z$opts[7] < kint)
      return(structure(list(fail=TRUE), class="lrm"))
    initial <- z$coef
  }
  
  if(nxin > 0) {
    ##Fit model with intercept(s), offset, and any fitted covariables
    z <- 
      .Fortran(F_lrmfit, coef=initial, nxin, est, x, y, offset,
               u=double(nvi),
               double(nvi*(nvi+1)/2), loglik=double(1), n, nx, sumw, nvi,
               v=double(nvi*nvi), double(nvi), double(2*nvi), double(nvi),
               pivot=integer(nvi), opts=opts, ftable, penmat, weights)
    
    irank <- z$opts[7]
    if(irank < nvi) {
      cat("singular information matrix in lrm.fit (rank=",irank,
          ").  Offending variable(s):\n")
      err_msg <- paste(xname[est[z$pivot[nvi : (irank + 1)] - kint]],
                       collapse=" ")
      cat(paste(xname[est[z$pivot[nvi : (irank + 1)] - kint]],
                collapse=" "),"\n")
      return(structure(list(fail=TRUE, offending_var=err_msg), class="lrm"))
    }
    loglik <- c(loglik, z$loglik)
  }
  
  dvrg <- z$opts[6] > 0
  
  if(nxin != nx) {
    ##Set up for score statistics - last model is not refitted but derivatives
    ##with respect to all other columns of x are evaluated
    initial <- rep(0,nx)
    if(length(est)) initial[est] <- z$coef[(kint + 1) : nvi]
    initial <- c(z$coef[1 : kint], initial)
    nvi <- as.integer(kint + nx)
    opts[3] <- 1	#Max no. iterations
    z <-
      .Fortran(F_lrmfit, coef=initial, nx, 1:nx, x, y, offset,
               u=double(nvi), double(nvi*(nvi+1)), double(1), n, nx,
               sumw, nvi, v=double(nvi*nvi), double(nvi), double(2*nvi),
               double(nvi), integer(nvi), opts=opts, ftable, penmat, weights)
  }
  
  ##Invert v with respect to fitted variables
  if(nxin == 0) elements <- 1 : kint
  else
    elements <- c(1 : kint, kint + est)
  if(nx == 0 && !ofpres) {
    v <- NULL; info.matrix <- NULL
    irank <- kint
  }
  else {
    if(nxin == nx) { 
      info.matrix <- matrix(z$v, nrow=nvi, ncol=nvi)
      v <- solvet(info.matrix, tol=tol)
      irank <- nvi
    }
    else {
      info.matrix <- matrix(z$v, nrow=nvi, ncol=nvi)
      v <- matinv(info.matrix, elements, negate=TRUE, eps=tol)
      info.matrix <- info.matrix[elements, elements]
      usc <- z$u[-elements]
      resid.chi2 <- usc %*% solve(v[-elements, -elements],
                                  tol=tol) %*% usc
      resid.df <- nx - nxin
      irank <- attr(v,"rank")
      attr(v,"rank") <- NULL
    }
  }
  
  if(kint == 1) name <- "Intercept"
  else 
    name <- paste("y>=", ylevels[2 : (kint + 1)], sep="")
  name <- c(name, xname)
  kof <- z$coef
  
  ## Compute linear predictor before unscaling beta, as x is scaled
  lp <- if(nxin > 0) matxv(x, kof, kint=1) else rep(kof[1], n)
  
  if(scale && nx > 0) {
    trans <-
      rbind(cbind(diag(kint), matrix(0, nrow=kint, ncol=nx)),
            cbind(-matrix(rep(xbar/xsd, kint), ncol=kint),
                  diag(1 / as.vector(xsd))))
    v   <- t(trans) %*% v %*% trans
    kof <- (kof %*% trans)[,, drop=TRUE]
  }
  
  names(kof) <- name
  names(z$u) <- name
  if(length(v)) dimnames(v) <- list(name, name)
  
  llnull <- loglik[length(loglik)-1]
  model.lr <- llnull - loglik[length(loglik)]
  model.df <- irank - kint
  model.p <- if(initial.there) NA else
    if(model.df > 0) 1 - pchisq(model.lr, model.df) else 1
  
  r2     <- 1 - exp(- model.lr / sumwt)
  r2.max <- 1 - exp(- llnull   / sumwt)
  r2     <- r2 / r2.max
  kmid <- floor((kint + 1) / 2)
  lpmid <- lp - kof[1] + kof[kmid] 
  prob <- plogis(lpmid)
  event <- y > (kmid - 1)
  ##  B <- mean((prob - event)^2)
  B <- sum(weights*(prob - event)^2) / sum(weights)
  g  <- GiniMd(lpmid)
  gp <- GiniMd(prob)
  stats <- c(n, max(abs(z$u[elements])), model.lr, model.df,
             model.p, z$opts[8], z$opts[9],
             z$opts[10], z$opts[11], r2, B, g, exp(g), gp)
  
  nam <- c("Obs", "Max Deriv",
           "Model L.R.", "d.f.", "P", "C", "Dxy",
           "Gamma", "Tau-a", "R2", "Brier", "g", "gr", "gp")
  
  if(nxin != nx) {
    stats <- c(stats, resid.chi2, resid.df,
               1 - pchisq(resid.chi2, resid.df))
    nam <- c(nam, "Residual Score", "d.f.", "P")
  }
  names(stats) <- nam
  
  if(wtpres) stats <- c(stats, 'Sum of Weights'=sumwt)
  
  retlist <- list(call=cal, freq=numy, sumwty=if(wtpres)sumwty else NULL,
                  stats=stats, fail=dvrg, coefficients=kof,
                  var=v, u=z$u,
                  deviance=loglik,
                  est=est, non.slopes=kint, linear.predictors=lp,
                  penalty.matrix=if(nxin>0 && any(penalty.matrix!=0))
                    penalty.matrix else NULL,
                  info.matrix=info.matrix,
                  weights=if(wtpres) weights else NULL)
  
  class(retlist) <- 'lrm'
  retlist
}

environment(lrm.fit) <- asNamespace('rms')
assignInNamespace("lrm.fit",lrm.fit,"rms")


lrm <- function(formula, data = environment(formula), subset, na.action = na.delete, 
                method = "lrm.fit", model = FALSE, x = FALSE, y = FALSE, 
                linear.predictors = TRUE, se.fit = FALSE, penalty = 0, penalty.matrix, 
                tol = 1e-07, strata.penalty = 0, var.penalty = c("simple", 
                                                                 "sandwich"), weights, normwt = FALSE, scale = FALSE, ...) {
  call <- match.call()
  var.penalty <- match.arg(var.penalty)
  callenv <- parent.frame()
  weights <- if (!missing(weights)) 
    eval(substitute(weights), data, callenv)
  subset <- if (!missing(subset)) 
    eval(substitute(subset), data, callenv)
  data <- rms::modelData(data, formula, weights = weights, subset = subset, 
                    na.action = na.action, callenv = callenv)
  tform <- terms(formula, specials = "strat", data = data)
  nstrata <- 1
  if (length(atl <- attr(tform, "term.labels")) && any(atl != ".")) {
    X <- rms::Design(data, formula = formula, specials = "strat")
    atrx <- attributes(X)
    sformula <- atrx$sformula
    nact <- atrx$na.action
    if (method == "model.frame") 
      return(X)
    Terms <- atrx$terms
    attr(Terms, "formula") <- formula
    atr <- atrx$Design
    mmcolnames <- atr$mmcolnames
    Y <- model.extract(X, "response")
    offs <- atrx$offset
    if (!length(offs)) 
      offs <- 0
    weights <- wt <- model.extract(X, "weights")
    if (length(weights)) 
      warning("currently weights are ignored in model validation and bootstrapping lrm fits")
    if (model) 
      m <- X
    stra <- attr(tform, "specials")$strat
    Strata <- NULL
    Terms.ns <- Terms
    if (length(stra)) {
      temp <- untangle.specials(Terms.ns, "strat", 1)
      Terms.ns <- Terms.ns[-temp$terms]
      attr(Terms, "factors") <- pmin(attr(Terms, "factors"), 
                                     1)
      attr(Terms.ns, "factors") <- pmin(attr(Terms.ns, 
                                             "factors"), 1)
      Strata <- X[[stra]]
      nstrata <- length(levels(Strata))
    }
    X <- model.matrix(Terms.ns, X)
    alt <- attr(mmcolnames, "alt")
    if (!all(mmcolnames %in% colnames(X)) && length(alt)) 
      mmcolnames <- alt
    X <- X[, mmcolnames, drop = FALSE]
    colnames(X) <- atr$colnames
    xpres <- length(X) > 0
    p <- length(atr$colnames)
    n <- length(Y)
    penpres <- !(missing(penalty) && missing(penalty.matrix))
    if (penpres && missing(var.penalty)) 
      warning("default for var.penalty has changed to \"simple\"")
    if (!penpres) 
      penalty.matrix <- matrix(0, ncol = p, nrow = p)
    else {
      if (missing(penalty.matrix)) 
        penalty.matrix <- Penalty.matrix(atr, X)
      else if (nrow(penalty.matrix) != p || ncol(penalty.matrix) != 
               p) 
        stop(paste("penalty.matrix does not have", p, 
                   "rows and columns"))
      psetup <- Penalty.setup(atr, penalty)
      penalty <- psetup$penalty
      multiplier <- psetup$multiplier
      if (length(multiplier) == 1) 
        penalty.matrix <- multiplier * penalty.matrix
      else {
        a <- diag(sqrt(multiplier))
        penalty.matrix <- a %*% penalty.matrix %*% a
      }
    }
  }
  else {
    X <- rms::Design(data, formula = formula, specials = "strat")
    offs <- model.offset(X)
    if (!length(offs)) 
      offs <- 0
    Y <- model.extract(X, "response")
    Y <- Y[!is.na(Y)]
    Terms <- X <- NULL
    xpres <- FALSE
    penpres <- FALSE
    penalty.matrix <- NULL
  }
  if (method == "model.matrix") 
    return(X)
  if (!is.factor(Y)) 
    Y <- as.vector(Y)
  if (nstrata > 1) {
    if (scale) 
      stop("scale=TRUE not implemented for stratified model")
    f <- lrm.fit.strat(X, Y, Strata, offset = offs, penalty.matrix = penalty.matrix, 
                       strata.penalty = strata.penalty, tol = tol, weights = weights, 
                       normwt = normwt, ...)
  }
  else {
    if (existsFunction(method)) {
      fitter <- getFunction(method)
      f <- fitter(X, Y, offset = offs, penalty.matrix = penalty.matrix, 
                  tol = tol, weights = weights, normwt = normwt, 
                  scale = scale, ...)
    }
    else stop(paste("unimplemented method:", method))
  }
  if (f$fail) {
    warning("Unable to fit model using ", dQuote(method))
    return(f)
  }
  f$call <- NULL
  if (model) 
    f$model <- m
  if (x) {
    f$x <- X
    f$strata <- Strata
  }
  if (y) 
    f$y <- Y
  nrp <- f$non.slopes
  if (penpres) {
    f$penalty <- penalty
    if (nstrata == 1) {
      v <- f$var
      if (var.penalty == "sandwich") 
        f$var.from.info.matrix <- v
      f.nopenalty <- fitter(X, Y, offset = offs, initial = f$coef, 
                            maxit = 1, tol = tol, scale = scale)
      info.matrix.unpenalized <- f.nopenalty$info.matrix
      dag <- diag(info.matrix.unpenalized %*% v)
      f$effective.df.diagonal <- dag
      f$var <- if (var.penalty == "simple") 
        v
      else v %*% info.matrix.unpenalized %*% v
      df <- sum(dag[-(1:nrp)])
      lr <- f.nopenalty$stats["Model L.R."]
      pval <- 1 - pchisq(lr, df)
      f$stats[c("d.f.", "Model L.R.", "P")] <- c(df, lr, 
                                                 pval)
    }
  }
  ass <- if (xpres) 
    DesignAssign(atr, nrp, Terms)
  else list()
  if (xpres) {
    if (linear.predictors) 
      names(f$linear.predictors) <- names(Y)
    else f$linear.predictors <- NULL
    if (se.fit) {
      if (nstrata > 1) 
        stop("se.fit=T not implemented for strat")
      xint <- matrix(0, nrow = length(Y), ncol = f$non.slopes)
      xint[, 1] <- 1
      X <- cbind(xint, X)
      se <- drop((((X %*% f$var) * X) %*% rep(1, ncol(X)))^0.5)
      names(se) <- names(Y)
      f$se.fit <- se
    }
  }
  f <- c(f, list(call = call, Design = if (xpres) atr, scale.pred = c("log odds", 
                                                                      "Odds Ratio"), terms = Terms, assign = ass, na.action = nact, 
                 fail = FALSE, interceptRef = 1, nstrata = nstrata, sformula = sformula))
  class(f) <- c("lrm", "rms", "glm")
  f
}




environment(lrm) <- asNamespace('rms')
assignInNamespace("lrm",lrm,"rms")


