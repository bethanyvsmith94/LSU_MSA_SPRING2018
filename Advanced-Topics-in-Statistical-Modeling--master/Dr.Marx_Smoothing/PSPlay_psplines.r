# Visualize P-splines
require(rpanel)
source('bases.r')

# Simulate data
n = 100
x = seq(0, 1, length = n)
set.seed(123)
y = 1.2 + sin(5  * x) + rnorm(n) * 0.2
order = 2
lla = 1

# Preparations
xg = seq(0, 1, length = 500)
nseg = 10
bdeg = 3
pord = 2

drawit = function() {
  nb = ncol(B)
  cols = hcl(h = seq(60, 240, length = nb), c =90, l = 70)
  Bg <<- bbase(xg, xl = 0, xr = 1, nseg = nseg, deg = bdeg)
  A = diag(a)
  z = Bg %*% a
  plot(x, y, lty = 1, type = 'l') 
  matlines(xg, Bg %*% A, type = 'l', lty = 1, lwd = 2, col= cols,
          xlab = '', ylab = '', ylim = c(0, 1))
  lines(xg, z, col = 'red', lwd = 3)        
  knots = seq(0, 1, length = nseg + 1)
  points(knots, 0 * knots, pch = 15, cex = 0.8)
  tl =paste('P-splines, n = ', nb, ', order = ', pord, 
            ', degree = ', bdeg, ', log10(lambda) = ', lla, sep = '')
  title(tl)
}


ps.smooth = function(p){
  nseg <<- floor(p$nseg)
  bdeg <<- p$bdeg
  lla <<- p$lla
  lambda <<- 10 ^ lla
  pord <<- p$ord 
#  cat(nseg, bdeg, pord, '\n')
  B <<- bbase(x,  xl = 0, xr = 1, nseg = nseg, deg = bdeg)
  nb = ncol(B)
  D = diff(diag(nb), diff = pord)
  P = lambda * t(D) %*% D
  a <<- solve(t(B) %*% B + P, t(B) %*% y)
  a <<- as.vector(a)
   drawit()
  return(p)
}

# Initalize panel
ps.panel = rp.control('PSP', size = c(400, 200),  
                      lla = 1, ord = 2, nseg = 10, bdeg = 3)

# Add a slider for penalty
rp.slider(ps.panel, var = lla, from = -4, to = 6, action = ps.smooth, 
#          pos = c(0, 0 ,width = 400, height = 100),
          resolution = 0.2, showvalue = T, title = 'Set log10(lambda)')

# Add a slider for penalty
rp.slider(ps.panel, var = nseg, from = 3, to = 20, action = ps.smooth,
#          pos = c(0, 100 ,width = 400, height = 100),
          resolution = 1, showvalue = T, title = 'Size of basis')

# Up-down buttons for penalty order
rp.doublebutton(ps.panel, var = ord, action = ps.smooth, initval = 2,
                step = 1, range = c(1, 4), showvalue = T, "Penalty order")

# Up-down buttons for B-spline degree
rp.doublebutton(ps.panel, var = bdeg, action = ps.smooth, initval = 3,
                step = 1, range = c(0, 4), showvalue = T, "B-spline degree")




