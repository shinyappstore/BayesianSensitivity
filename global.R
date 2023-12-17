library("coda")
library("rstan")
library("rstanarm")
library("ggmcmc")
library("bayesplot")
library("RColorBrewer")
library("gridExtra")
library("DT")
library("Rcpp")

#############################################################################
## Set up inverse gamma density function (from invgamma package)
dinvgamma <- function(x, shape, rate = 1, scale = 1/rate, log = FALSE) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  log_f <- dgamma(1/x, shape, rate, log = TRUE) - 2*log(x)
  if(log) return(log_f)
  exp(log_f)
}

#############################################################################
## Read Data

reg_data <- read.csv("data_regression100.csv")


#############################################################################
## Specify and compile the stan model (this takes a bit)

## Specify the stan model

model_reg.stan <- "
data {
int<lower=0> N;   // number of data items
int<lower=0> K;   // number of predictors
matrix[N, K] x;   // predictor matrix
vector[N] y;      // outcome vector
// hyperparameters of the prior distributions
real mu0; // mean intercept	
real mu1; // mean sex coefficient
real mu2; // mean trust coefficient
real s20; // variance intercept	
real s21; // variance sex coefficient
real s22; // variance trust coefficient
real ig1; // resid variance shape param
real ig2; // resid variance scale param

}

parameters {
real alpha;           // intercept
vector[K] beta;      // coefficients
real<lower=0> sigma2;  // error scale
}

model {
alpha ~ normal(mu0, sqrt(s20));
beta[1] ~ normal(mu1, sqrt(s21));
beta[2] ~ normal(mu2, sqrt(s22));
sigma2 ~ inv_gamma(ig1, ig2);

y ~ normal(x * beta + alpha, sqrt(sigma2));
}
"

## Compile model
model <- stan_model(model_code = model_reg.stan)

## Set number of iterations for sensitivity analyses
iterations <- 10000
#############################################################################
## OG Stan model Results

fit_og <- readRDS("fit_og100.rds")

#Posterior Chains with Burn-in (convergence plots)
post_all_og <- rstan::extract(fit_og, pars=c("alpha", "beta", "sigma2"), inc_warmup = TRUE, permuted = FALSE)

#Posterior Chains without burn-in for some parts of app (mcmc_dens_overlay)
post_half_og <- rstan::extract(fit_og, pars=c("alpha", "beta", "sigma2"), inc_warmup = FALSE, permuted = FALSE)

#Posterior Chains without Burn-in for some other parts of app (post density plots for sensitivity analysis)
post_og <- rstan::extract(fit_og, pars=c("alpha", "beta", "sigma2"), inc_warmup = FALSE, permuted = TRUE)

#Posterior Values for the Results Table
int_og <- matrix(unlist(post_og), nrow = nrow(post_og[[1]]), byrow = FALSE)
colnames(int_og) <- c("alpha", "beta[1]", "beta[2]", "sigma^2")

## Summary statistics OG prior
parnames <- c("beta[intercept]", "beta[sex]", "beta[lackoftrust]", "epsilon^2")

dimnames(post_all_og)$parameters <- parnames
dimnames(post_half_og)$parameters <- parnames

sum_og <- data.frame("Parameter" = parnames, 
                     "Mean" = colMeans(int_og),
                     "SD" = apply(int_og, 2, sd),
                     "ll" = posterior_interval(int_og)[,1],
                     "median" = apply(int_og, 2, median),
                     "ul"  = posterior_interval(int_og)[,2],
                     stringsAsFactors = FALSE)

## Static Prior Plots original study

#Convergence Diagnostics for Trace plot
rhats <- summary(fit_og)$summary[1:4,10]
n_eff <- summary(fit_og)$summary[1:4,9]

# Set up plot labels
plot_labels <- paste0("R-hat: ", format(rhats, digits = 2, nsmall = 2), 
                      "\n ESS: ", format(n_eff, digits = 0, nsmall = 0))

rhats <- data.frame("parameter" = parnames, "rhats" = plot_labels)
rhats <- bind_cols(rhats, "chain" = rep(1, nrow(rhats)))
rhats$chain <- factor(rhats$chain)
rhats$parameter <- factor(rhats$parameter)

# Use geom_blank in mcmc_trace plot to expand y-axis for some plots, to
# create white space for convergence diagnostics.
post_max <- summary(fit_og)$summary[1:4,8]
blank_data <- data.frame("parameter" = factor(rep(parnames, each = 2)), "chain" = factor(1))
blank_data$x <- 0
blank_data$y <- c(0, post_max[1]*1.3, 0, post_max[2]*2.2, 0, NA, 0, NA)


# df = xlimits for each plot
df <- data.frame(x = c(20, 60))

p1 <- ggplot(df, aes(x=x)) +
  stat_function(fun = dnorm, n = 1001, args = list(mean = 41, sd = sqrt(10)), geom = "area", aes(fill = "Original Prior"), alpha = .7) +
  stat_function(fun = dnorm, n = 1001, args = list(mean = 41, sd = sqrt(10))) +
  scale_fill_brewer(palette = "Blues", name = "") +
  ggtitle(bquote(beta[intercept]%~%italic(N)(41, 10))) +
  theme_classic() +  
  theme(legend.position="none", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", family="sans", size = 14))

df <- data.frame(x = c(-10, 10))

p2 <- ggplot(df, aes(x=x)) +
  stat_function(fun = dnorm, n = 1001, args = list(mean = 0, sd = sqrt(10)), geom = "area", aes(fill = "Original Prior"), alpha = .7) +
  stat_function(fun = dnorm, n = 1001, args = list(mean = 0, sd = sqrt(10))) +
  scale_fill_brewer(palette = "Blues", name = "") +
  ggtitle(bquote(beta[sex]%~%italic(N)(0, 10))) +
  theme_classic() +  
  theme(legend.position="none", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", family="sans", size = 14))

p3 <- ggplot(df, aes(x=x)) +
  stat_function(fun = dnorm, n = 1001, args = list(mean = 6, sd = sqrt(1)), geom = "area", aes(fill = "Original Prior"), alpha = .7) +
  stat_function(fun = dnorm, n = 1001, args = list(mean = 6, sd = sqrt(1))) +
  scale_fill_brewer(palette = "Blues", name = "") +
  ggtitle(bquote(beta[lackoftrust]%~%italic(N)(6, 1))) +
  theme_classic() +  
  theme(legend.position="none", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", family="sans", size = 14))


df <- data.frame(x = c(0.000000000001, 30))

p4 <- ggplot(df, aes(x=x)) +
  stat_function(fun = dinvgamma, n = 1001, args = list(shape = 0.5, scale = .5), geom = "area", aes(fill = "Original Prior"), alpha = .7) +
  stat_function(fun = dinvgamma, n = 1001, args = list(shape = 0.5, scale = .5)) +
  scale_fill_brewer(palette = "Blues", name = "") +
  ggtitle(bquote(epsilon^2%~%italic(IG)(.5, .5))) +
  theme_classic() +  
  theme(legend.position="none", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", family="sans", size = 14))

## Posterior Plot Comparison Function
post_plot_fun <- function(post_plot, param, senstype) {
  
  if(param == "alpha") param2 <- bquote(beta[intercept]) 
  if(param == "beta.1") param2 <- bquote(beta[sex])
  if(param == "beta.2") param2 <- bquote(beta[lackoftrust])
  if(param == "sigma2") param2 <- bquote(epsilon^2)
  
  if(senstype == 1) {
    p <- ggplot(post_plot) + 
      geom_density(aes(x=eval(parse(text=paste(param))), fill="Original Prior"), alpha = .7) + 
      geom_density(aes_(x=as.name(paste(param, ".1", sep = "")), fill="Alternative Prior 1"), alpha = .7) + 
      geom_density(aes_(x=as.name(paste(param, ".2", sep = "")), fill="Alternative Prior 2"), alpha = .7) + 
      scale_fill_brewer(palette = "Blues", name = "") +
      ggtitle(param2) +
      theme_classic() +  
      theme(
        #legend.position = "none",
        legend.position="bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", family="sans", size = 18),
        legend.text=element_text(size=12)
      )
  } else if(senstype == 2) {
    p <- ggplot(post_plot) + 
      geom_density(aes(x=eval(parse(text=paste(param))), fill="Original Priors"), alpha = .7) + 
      geom_density(aes_(x=as.name(paste(param, ".1", sep = "")), fill="Selected Priors"), alpha = .7) + 
      scale_fill_brewer(palette = "Blues", name = "") +
      ggtitle(param2) +
      theme_classic() +  
      theme(
        #legend.position = "none",
        legend.position="bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold", family="sans", size = 18),
        legend.text=element_text(size=12)
      )
  }
  p
}
