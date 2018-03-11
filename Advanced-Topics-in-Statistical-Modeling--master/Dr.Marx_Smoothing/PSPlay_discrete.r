# Visualize Whittaker smoother
require(rpanel)

# The smoother
whitsm = function(y, lambda = 10, d = 2){
  m = length(y)
  E = diag(m)
  D = diff(E, diff = d)
  z = solve(E + lambda * t(D) %*% D, y)
  return(z)
}

# Simulate data
n = 100
x = seq(0, 1, length = n)
set.seed(123)
y = 1.2 + sin(5  * x) + rnorm(n) * 0.2
order = 2
lla = 1

ws.draw = function(){
  tl = paste('Whittaker smoothing; order = ', order, ', log10(lambda) = ', lla,
             sep = '')
  plot(x, y, type = 'l', xlab = '', ylab = '', lwd = 1.5, main = tl)
  lines(x, z, col = 'red', lty = 1, lwd = 3)
}

ws.smooth = function(p){
  lla <<- p$lla
  order <<- p$order
  lambda = 10 ^ lla
  z <<- whitsm(y, lambda, order)
  ws.draw()
  p
}

# Initalize panel
ws.panel = rp.control('Whittaker', lla = 1)

# Add a slider
rp.slider(ws.panel, var = lla, from = -2, to = 10, action = ws.smooth,
          resolution = 0.2, showvalue = T, title = 'Set log10(lambda)')

# Up-down buttosn for degree
rp.doublebutton(ws.panel, var = order, action = ws.smooth, initval = 2,
                step = 1, range = c(1, 4), showvalue = T, "Order")




