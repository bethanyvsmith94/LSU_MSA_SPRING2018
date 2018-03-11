# Interctiev B-spline visualizer
library(rpanel)
source('bases.r')

# Compute x
x = seq(0, 1, length = 500)

bs.compute = function() {
  nb = ncol(B)
  cols = hcl(h = seq(60, 240, length = nb), c =90, l = 70)
  A = diag(a)
  z = B %*% a
  matplot(x, B %*% A, type = 'l', lty = 1, lwd = 2, col= cols,
          xlab = '', ylab = '', ylim = c(0, 1))
  lines(x, z, col = 'red', lwd = 3)        
  knots = seq(0, 1, length = nseg + 1)
  points(knots, 0 * knots, pch = 15, cex = 0.8)
  title(paste('B-spline basis, n = ', nb, ', degree = ', bdeg, sep = ''))
}

# Drawing function
bs.draw = function(p){
  nseg <<- floor(p$nseg)
  bdeg <<- p$bdeg
  B <<- bbase(x, nseg = nseg, deg = bdeg)
  a <<- rep(1, ncol(B))
  bs.compute()
  return(p)
}

bs.random = function(p) {
  a <<- runif(ncol(B))
  bs.compute()
  return(p)
}

# Initalize panel
bs.panel = rp.control('B-spline visualizer', nseg = 5, size = c(300, 100))

# Add a slider for size of bases
rp.slider(bs.panel, nseg, 1, 20, bs.draw, '# of B-splines')

# Up-down buttosn for degree
rp.doublebutton(bs.panel, var = bdeg, action = bs.draw, initval = 3,
                step = 1, range = c(0, 4), showvalue = T, "Degree")
                
# Button for random heights
rp.button(bs.panel, action = bs.random, 'Random')                
