# Adding "p-spline" classes and methods
# From the mgcv documentation, under 'smooth.construct'

smooth.construct.ps.smooth.spec<-function(object,data,knots)
# a p-spline constructor method function
{ require(splines)
  if (length(object$p.order)==1) m<-rep(object$p.order,2)
  else m<-object$p.order  # m[1] - basis order, m[2] - penalty order
  nk<-object$bs.dim-m[1]  # number of interior knots
  if (nk<=0) stop("basis dimension too small for b-spline order")
  x <- get.var(object$term,data)  # find the data
  xl<-min(x);xu<-max(x);xr<-xu-xl # data limits and range
  xl<-xl-xr*0.001;xu<-xu+xr*0.001;dx<-(xu-xl)/(nk-1)
  if (!is.null(knots)) k <- get.var(object$term,knots)
  else k<-NULL
  if (is.null(k))
  k<-seq(min(x)-dx*(m[1]+1),max(x)+dx*(m[1]+1),length=nk+2*m[1]+2)
  if (length(k)!=nk+2*m[1]+2)
  stop(paste("there should be ",nk+2*m[1]+2," supplied knots"))
  object$X<-spline.des(k,x,m[1]+2,x*0)$design # get model matrix
  if (!object$fixed)
  { S<-diag(object$bs.dim);if (m[2]) for (i in 1:m[2]) S<-diff(S)
    object$S<-list(t(S)%*%S)  # get penalty
    object$S[[1]] <- (object$S[[1]]+t(object$S[[1]]))/2 # exact symmetry
  }
  object$rank<-object$bs.dim-m[2]  # penalty rank
  object$null.space.dim <- m[2]  # dimension of unpenalized space
  object$knots<-k;object$m<-m      # store p-spline specific info.
  object$C<-matrix(colSums(object$X),1,object$bs.dim) #constraint
  object$df<-ncol(object$X)-1      # maximum DoF
  if (object$by!="NA")  # deal with "by" variable
  { by <- get.var(object$by,data) # find by variable
    if (is.null(by)) stop("Can't find by variable")
    object$X<-as.numeric(by)*object$X # form diag(by)%*%X
  }
  class(object)<-"pspline.smooth"  # Give object a class
  object
}

Predict.matrix.pspline.smooth<-function(object,data)
# prediction method function for the p.spline smooth class
{ require(splines)
  x <- get.var(object$term,data)
  X <- spline.des(object$knots,x,object$m[1]+2,x*0)$design
  if (object$by != "NA") { ## handle any by variables
        by <- get.var(object$by, data)
        if (is.null(by))
            stop("Can't find by variable")
        X <- as.numeric(by) * X
  }
  X
}

