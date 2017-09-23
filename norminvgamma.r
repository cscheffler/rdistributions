# Function definitions for the normal-inverse-gamma distribution.
# The parameters of the distribution (mu, lambda, alpha, beta) are as defined here:
# https://en.wikipedia.org/wiki/Normal-inverse-gamma_distribution

# NOTE: The functions below require the function definitions in invgamma.r

dnorminvgamma <- function(x, sigma2, mu, lambda, alpha, beta) {
    # The pdf of the normal-inverse-gamma distribution at x (mean) and sigma2 (variance).
    return(dnorm(x, mu, sqrt(sigma2 / lambda)) * dinvgamma(sigma2, alpha, beta));
}

rnorminvgamma <- function(n, mu, lambda, alpha, beta) {
    # Draw n samples from the normal-inverse-gamma distribution.
    # Returns a matrix where each column contains a (x, sigma2) sample.
    sigma2 = rinvgamma(n, alpha, beta);  # Sample sigma^2 from the inverse-gamma
    x = rnorm(n, mu, sqrt(sigma2 / lambda));  # Sample x from the normal
    return(t(matrix(c(x, sigma2), nrow=n, ncol=2)));
}
