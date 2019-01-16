# M. Yusril Nugraha P
# 17523117


# number 11
f <- function(x) {
  return(x^2 -6)
}
trapezoid <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  h <- b - a
  fxdx <- (h / 2) * (f(a) + f(b))
  return(fxdx)
}
trapezoid(f, 0, 1)    # Answer is (a)


#number 12
f <- function(x) {
  return(x^3 + 4*x^2 - 10)
}
trapezoid <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  h <- b - a
  fxdx <- (h / 2) * (f(a) + f(b))
  return(fxdx)
}
trapezoid(f, 1, 2)    # Answer is (c)


# number 6
bi <- function(a, b) {
  re <- 3
  pn <- (a+b)/2
  while (re >= 0.0001) {
    print(paste(a,b,pn,fx(pn),fx(a),re,sep = " ") )
    p <- pn
    if (sign(g(p)) == sign(g(a)) ) {
      a <- p
    } else {
      b <- p
    }
    pn <- (a+b)/2
    re <- abs(pn-p) / abs(pn)
  }
}

# number 11


# number 13
h <- 0.1
x <- seq(0,1, by = h)
f <- function(x) {
  return(x^2)
}
f0 <- f(x[1])
fi <- sapply(x[2:10], f)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h) {
  L <- h * (f0 + 2 * sum(fi) + fn)/2
  
  return(L)
}
trap(f0, fi, fn, h)   # Answer is (a)



# number 14
# Hasil dari eksekusi baris ke - 14 adalah 
trap(f0, fi, fn, h)   # Answer is (b)



# number 15
h <- 0.2            # mengganti h = 0.2
x <- seq(0,1, by = h)
f <- function(x) {
  return(x^2)
}
f0 <- f(x[2])
fi <- sapply(x[2:10], f)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h) {
  L <- h * (f0 + 2 * sum(fi) + fn)/2
  
  return(L)
}
trap(f0, fi, fn, h) # Answer is (c)
