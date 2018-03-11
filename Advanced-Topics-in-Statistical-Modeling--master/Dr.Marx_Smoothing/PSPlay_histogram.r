# Visualize Whittaker smoother for histogram
require(rpanel)

# The Poisson smoother
histsm = function(y, lambda = 10, d = 2){
  # Penalty stuff
  m = length(y)
  E = diag(m)
  D = diff(E, diff = d)            
  P = lambda * t(D) %*% D
  
  # Initialie
  u = log(y + 0.5)
  z0 = 0
  
  # Iterate
  for (it in 1:20) {
    z = exp(u)
    dz = max(abs(z - z0))
    z0 = z
    Z = diag(z)
    u  = solve(Z + P, y - z + z * u)
    if (dz < 1e-5) break
  }
  return(z)
}

# Simulate data
n = 100
set.seed(123)
v = rnorm(n)
hst = hist(v, breaks = seq(-3, 3, by = 0.1))
x = hst$mids
y = hst$counts
order = 2
lla = 1

# Do the for drawing
ws.draw = function(){
  tl = paste('Histogram smoothing; order = ', order, ', log10(lambda) = ', lla,
             sep = '')
#  plot(x, y, type = 'h', lwd = 5, xlab = '', ylab = '', main = tl, 
#       col = gray(0.7))
  plot(hst)
  lines(x, z, col = 'red', lty = 1, lwd = 3)
}

# Do the smoothing (called by GUI)
ws.smooth = function(p){
  lla <<- p$lla
  order <<- p$order
  lambda = 10 ^ lla
  z <<- histsm(y, lambda, order)
  ws.draw()
  p
}

# Initialize panel
ws.panel = rp.control('Whittaker', lla = 1)

# Add a slider
rp.slider(ws.panel, var = lla, from = -2, to = 10, action = ws.smooth,
          resolution = 0.2, showvalue = T, title = 'Set lambda')

# Up-down buttosn for degree
rp.doublebutton(ws.panel, var = order, action = ws.smooth, initval = 2,
                step = 1, range = c(1, 4), showvalue = T, "Order")




