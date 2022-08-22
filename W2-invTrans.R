###########################################
## Visualization of Inverse
## Transform method
###########################################
set.seed(1)

## CDF of Exp(1)
x <- seq(0, 10, length = 1e3)
plot(x, pexp(x), type = 'l', main = "CDF of Exp(1)", ylab = "F(x)")


## Inverse CDF
foo <- seq(0,1, length = 1e3)
inv.cdf <- -log(1-foo)
plot(foo, inv.cdf, type = 'l', ylab = "X = Finv(x)", xlab = "U", main = "Inverse CDF ")


# Draw random draws from Exp(1)
n <- 100
U <- runif(n)
X <- -log(1-U)

# Visualization
on.axis <- rep(-.2,n)
on.yaxis <- rep(-.02,n)
points(on.yaxis, X, pch = 16, col = "blue")
segments(x0 = U,y0 = on.axis, y1 = X, col = "blue", lty = 2)
segments(x0 = U,y0 = X, x1 = on.yaxis, col = "blue", lty = 2)





###########################################
# Repeat for Cauchy


## CDF of Cauchy
x <- seq(-20, 20, length = 1e3)
plot(x, pt(x, df = 1), type = 'l', main = "CDF of Cauchy", ylab = "F(x)")


## Inverse CDF
foo <- seq(0.01,.99, length = 1e3)
inv.cdf <- tan(pi*(foo - .5))
plot(foo, inv.cdf, type = 'l', ylab = "X", xlab = "U", main = "Inverse CDF ")


# Draw random draws from Exp(1)
n <- 100
U <- runif(n)
X <- tan(pi*(U - .5))

# Visualization
on.axis <- rep(-32,n)
on.yaxis <- rep(-.02,n)
points(on.yaxis, X, pch = 16, col = "blue")
segments(x0 = U,y0 = on.axis, y1 = X, col = "blue", lty = 2)
segments(x0 = U,y0 = X, x1 = on.yaxis, col = "blue", lty = 2)

