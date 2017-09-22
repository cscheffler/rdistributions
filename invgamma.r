# Function definitions for the inverse-gamma probability distribution.
# The parameters of the distribution (alpha, beta) are as defined here:
# https://en.wikipedia.org/wiki/Inverse-gamma_distribution

pinvgamma <- function(x, alpha, beta) {
    # The cdf of the inverse-gamma distribution.
    return(1 - pgamma(1/x, shape=alpha, rate=beta));
}

qinvgamma <- function(p, alpha, beta) {
    # The quantile function of the inverse-gamma distribution.
    return(1 / qgamma(1 - p, shape=alpha, rate=beta));
}

dinvgamma <- function(x, alpha, beta) {
    # The pdf of the inverse-gamma distribution.
    if (alpha <= 0 | beta <= 0) {
        stop("Shape or scale parameter negative in dinvgamma().\n");
    }
    log_density <- alpha * log(beta) - lgamma(alpha) - (alpha + 1) * log(x) - (beta / x);
    return(exp(log_density));
}

rinvgamma <- function(n, alpha, beta) {
    # Draw n samples from the inverse-gamma distribution.
    return(1 / rgamma(n=n, shape=alpha, rate=beta));
}
