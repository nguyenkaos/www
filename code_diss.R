library(pdc)

####################  diss_MINDIST_SAX  ###########################
SAX.breakpoints.table_manuel <- function (n) 
{
    qnorm(0:n/n)
}


convert.to.SAX.symbol_manuel <- function (x, alpha) 
{
    symb <- SAX.breakpoints.table_manuel(alpha)
    saxstring <- NULL
    for (s in x) {
        saxstring <- c(saxstring, sum(symb < s))
    }
    saxstring
}



PAA_manuel <- function (x, w) 
{
    if ((w - floor(w)) > 0) {
        stop("w (number of frames) must be an integer")
    }
    n <- length(x)
    if (w > n) {
        stop("cannot have more parts than the length of the series")
    }
    PAA <- rep(0, w)
    d = n/w
    breakpoints <- seq(0, n, d)
    for (i in 1:w) {
        init <- breakpoints[i] + 1
        end <- breakpoints[i + 1]
        frac_first <- ceiling(init) - init
        frac_end <- end - floor(end)
        interv = floor(init):ceiling(end)
        sec <- x[interv]
        if (frac_first > 0) {
            sec[1] = sec[1] * frac_first
        }
        if (frac_end > 0) {
            sec[length(sec)] = sec[length(sec)] * frac_end
        }
        PAA[i] = sum(sec)/d
    }
    PAA
}

MINDIST.SAX_manuel <- function (x, y, alpha, n) 
{
    w <- length(x)
    symb <- SAX.breakpoints.table_manuel(alpha)
    d <- 0
    for (i in 1:w) {
        xi <- x[i]
        yi <- y[i]
        if (abs(xi - yi) > 1) {
            d <- d + (symb[max(xi, yi)] - symb[min(xi, yi) + 
                1])^2
        }
    }
    sqrt((n/w) * d)
}


diss_MINDIST_SAX <- function (x, y, w, alpha = 4, plot = FALSE) 
{ 
    n <- length(x)
    x <- (x - mean(x))/sd(x)
    y <- (y - mean(y))/sd(y)
    PAAx <- PAA_manuel(x, w)
    PAAy <- PAA_manuel(y, w)
    SAXx <- convert.to.SAX.symbol_manuel(PAAx, alpha)
    SAXy <- convert.to.SAX.symbol_manuel(PAAy, alpha)
    dSAX <- MINDIST.SAX_manuel(SAXx, SAXy, alpha, n) 
    dSAX
}

####################  diss_INT_PER  ###########################
diss_INT_PER <- function (x, y, normalize = TRUE) 
{ 
    Ix <- spec.pgram(x, plot = F)
    Iy <- spec.pgram(y, plot = F)
    Cx <- 1
    Cy <- 1
    if (normalize) {
        Cx <- sum(Ix$spec)
        Cy <- sum(Iy$spec)
    }
    sum(abs(cumsum(Ix$spec)/Cx - cumsum(Iy$spec)/Cy))
}



####################  diss_PACF  ###########################
internal.autocorr.dist_manuel <- function (rhox, rhoy, p = NULL, omega = NULL) 
{
    if (length(rhox) != length(rhoy)) {
        stop("The amount of autocorrelation coefficients must be the same, maybe lag.max greater than the length of one of the series")
    }
    if (is.null(omega)) {
        if (!is.null(p)) {
            omega <- diag(p * (1 - p)^(1:length(rhox)))
        }
        else {
            omega <- diag(length(rhox))
        }
    }
    sqrt(t(rhox - rhoy) %*% omega %*% (rhox - rhoy))
}

diss_PACF <- function (x, y, p = NULL, omega = NULL, lag.max = 50) 
{ 
    rhox <- as.vector(pacf(x, lag.max = lag.max, plot = FALSE)$acf)
    rhoy <- as.vector(pacf(y, lag.max = lag.max, plot = FALSE)$acf)
    internal.autocorr.dist_manuel(rhox, rhoy, p, omega)
}


####################  diss_PDC  ###########################
diss_PDC <- function (x, y, ...) 
{
    pdcDist(cbind(x, y), ...)
}


####################  diss_PER  ###########################
diss_PER <- function (x, y, logarithm = FALSE, normalize = FALSE) 
{ 
    Ix <- spec.pgram(x, plot = F)$spec
    Iy <- spec.pgram(y, plot = F)$spec
    if (normalize) {
        Ix <- Ix/var(x)
        Iy <- Iy/var(y)
    }
    if (logarithm) {
        Ix <- log(Ix)
        Iy <- log(Iy)
    }
    dist(rbind(Ix, Iy))/(length(Ix))
}

 




