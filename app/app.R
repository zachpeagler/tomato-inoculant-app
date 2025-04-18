##### SETUP #####
## this code runs on the client at execution, and is a lot of setup,
## variable caching, and preemptive modeling.

# load packages
library(shiny) # reactivity/app framework
library(stringr) # handling text
library(ggplot2) # graphing
library(scico) # colors
library(bslib) # bootstrap elements
library(bsicons) # bootstrap icons
library(multcomp) # easy pairwise comparisons
library(MASS) # lots of statistical stuff
library(car) # logit transformations
library(MuMIn) # uhhh
library(lmerTest) # mixed effect models
library(vegan) # multivariate stuff

# graphics
p_palettes <- scico_palette_names()
font_sizes <- c(18,16,14)

# distributions
## currently only continuous distributions are supported
dists <- c("normal", "lognormal", "gamma", "exponential")

# load data
## TIM
load("data_tim_ds.RData")
load("data_tim_fluoro.RData")
load("data_tim_height.RData")
## TIL
load("data_til_fruit.RData")
load("data_til_fruit_summary.RData")
load("data_til_fluoro.RData")
## TIT
load("data_tit_fluoro.RData")
load("data_tit_fruit.RData")
load("data_tit_fruit_summary.RData")

# scale variables for later use in modeling
## scaling is important so they have equal "weight" in the model
## especially in multidimensional modeling (PCA, tSNE, NMDS)
## we only scale our independent (explanatory) variables
### tim fluoro
scaled_tim_fluoro_vars <- data.frame(apply(data_tim_fluoro[,c(6,7,11:15)], 2, scale))
mod_data_tim_fluoro <- cbind(data_tim_fluoro[,c(1:5,8,9,10, 16:18)], scaled_tim_fluoro_vars)
### tim height
#### nothing to scale here. we have no continuous predictor variables in the height dataset.
### tim ds
#### nothing to scale here. we have no continuous predictor variables in the ds dataset.
### til fruit
#### nothing to scale here (we could scale mass for use in sugar modeling, but ehhhhhhh i doubt we need to
#### we're not performing dimension reduction or multiple linear regression, so it shouldn't much matter)
#### same with tit fruit data
### til fluoro
scaled_til_fluoro_vars <- data.frame(apply(data_til_fluoro[,c(6,7,11:15)], 2, scale))
mod_data_til_fluoro <- cbind(data_til_fluoro[,c(1:5,8,9,10, 16:19)], scaled_til_fluoro_vars)
### tit fluoro
scaled_tit_fluoro_vars <- data.frame(apply(data_tit_fluoro[,c(6,7,11:15)], 2, scale))
mod_data_tit_fluoro <- cbind(data_tit_fluoro[,c(1:5,8,9,10, 16:19)], scaled_tit_fluoro_vars)

fluoro_mod_var_names <- c("DaysFromGermination", "MinutesFromStart", "AmbientHumidity", "AmbientPressure", "AmbientTemperature", "AmbientLight", "LeafTemperature")

# preload var names in continuous and discrete groups
### kind of a funky way of doing this, but it makes it REALLY easy to check if a variable
### is continuous or discrete later with [if (var %in% vars_d)] etc.
### you could also use the type attribute of your variables, but i didn't feel like it
#### tim (2024 small)
##### fluoro
tim_fluoro_vars <- c("Date", "DaysFromGermination", "MinutesFromStart", "AmbientHumidity", "AmbientPressure", "AmbientTemperature", "AmbientLight", "LeafTemperature", "gsw", "PhiPS2", "LogitPhiPS2")
tim_fluoro_vars_d <- c("Treatment", "Inoculation", "Chitosan", "Row", "Pot", "Plant", "Time")
all_tim_fluoro_vars <- c(tim_fluoro_vars_d, tim_fluoro_vars)
##### height
tim_height_vars <- c("Date", "DaysFromGermination", "Height")
tim_height_vars_d <- c("Treatment", "Inoculation", "Chitosan", "Row", "Pot", "Plant")
all_tim_height_vars <- c(tim_height_vars_d, tim_height_vars)
##### ds
tim_ds_vars <- c("AG_Length", "AG_Mass", "BG_Length", "BG_Mass", "RS_Length", "RS_Mass")
tim_ds_vars_d <- c("Treatment", "Inoculation", "Chitosan", "Row", "Pot", "Plant")
all_tim_ds_vars <- c(tim_ds_vars_d, tim_ds_vars)
### til (2023 big)
#### fluoro
til_fluoro_vars <- c("Date", "DaysFromGermination", "MinutesFromStart", "AmbientHumidity", "AmbientPressure", "AmbientTemperature", "AmbientLight", "LeafTemperature", "gsw", "PhiPS2", "LogitPhiPS2")
til_fluoro_vars_d <- c("Treatment", "Soil", "Foliar", "Row", "Pot", "Plant", "Time", "Device")
all_til_fluoro_vars <- c(til_fluoro_vars_d, til_fluoro_vars)
#### fruit
til_fruit_vars <- c("Date", "DaysFromGermination", "Mass", "Ripeness", "pSugar", "SugarGrams")
til_fruit_vars_d <- c("Treatment", "Soil", "Foliar", "Row", "Pot", "Plant")
all_til_fruit_vars <- c(til_fruit_vars_d, til_fruit_vars)
#### fruit sums
til_fruit_sum_vars <- c("Fruit_sum", "BER_sum", "Mass_sum", "Mass_mean", "pBER")
til_fruit_sum_vars_d <- c("Treatment", "Soil", "Foliar", "Plant")
all_til_fruit_sum_vars <- c(til_fruit_sum_vars_d, til_fruit_sum_vars)
#### tit (2024 big)
##### fluoro
tit_fluoro_vars <- c("Date", "DaysFromGermination", "MinutesFromStart", "AmbientHumidity", "AmbientPressure", "AmbientTemperature", "AmbientLight", "LeafTemperature", "gsw", "PhiPS2", "LogitPhiPS2")
tit_fluoro_vars_d <- c("Treatment", "Transplantation", "Germination", "Row", "Pot", "Plant", "Time", "Device")
all_tit_fluoro_vars <- c(tit_fluoro_vars_d, tit_fluoro_vars)
##### fruit
tit_fruit_vars <- c("DateHarvest", "DateAnalysis", "DaysFromHarvestToAnalysis", "DaysFromGermination", "Mass", "Ripeness", "pSugar", "SugarGrams")
tit_fruit_vars_d <- c("Treatment", "Transplantation", "Germination", "Row", "Pot", "Plant", "BER")
all_tit_fruit_vars <- c(tit_fruit_vars_d, tit_fruit_vars)
##### fruit sum
tit_fruit_sum_vars <- c("Fruit_sum", "BER_sum", "Mass_sum", "Mass_mean", "pBER")
tit_fruit_sum_vars_d <- c("Treatment", "Transplantation", "Germination", "Plant")
all_tit_fruit_sum_vars <- c(tit_fruit_sum_vars_d, tit_fruit_sum_vars)

fruit_lab_vars  <- c("pSugar", "SugarGrams", "Ripeness")

# custom functions
## i *think* this is faster and smaller than including these as a dependency via a custom package
## that being said, these functions are (mostly) taken from my package ztils (github.com/zachpeagler/ztils)
## some of these functions are bespoke (i.e. shitty and unreliable)
multiKS_cont  <- function(var, distributions) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  KS_df <- data.frame(matrix(ncol=3, nrow=0))
  colnames(KS_df) <- c("Distribution", "Distance", "P-Value")
  # check normal
  if ("normal" %in% distributions) {
    var_n <- MASS::fitdistr(var, "normal")
    var_KS_n <- ks.test(var, "pnorm", mean=var_n$estimate[1],
                        sd = var_n$estimate[2])
    KS_n <- data.frame(matrix(ncol=0, nrow=1))
    KS_n$Distribution <- "Normal"
    KS_n$Distance <- if (!is.null(var_KS_n$statistic)) {var_KS_n$statistic}
    else {"NA"}
    KS_n$PValue <- if (!is.null(var_KS_n$p.value)) {var_KS_n$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_n)
  }
  if ("lognormal" %in% distributions) {
    var_ln <- MASS::fitdistr(var, "lognormal")
    var_KS_ln <- ks.test(var, "plnorm",
                         meanlog=var_ln$estimate[1],
                         sdlog = var_ln$estimate[2])[c(1, 2)]
    KS_ln <- data.frame(matrix(ncol=0, nrow=1))
    KS_ln$Distribution <- "Lognormal"
    KS_ln$Distance <- if (!is.null(var_KS_ln$statistic)) {var_KS_ln$statistic}
    else {"NA"}
    KS_ln$PValue <- if (!is.null(var_KS_ln$p.value)) {var_KS_ln$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_ln)
  }
  if ("gamma" %in% distributions) {
    var_g <- MASS::fitdistr(var, "gamma")
    var_KS_g <- ks.test(var, "pgamma",
                        shape=var_g$estimate[1],
                        rate=var_g$estimate[2])
    KS_g <- data.frame(matrix(ncol=0, nrow=1))
    KS_g$Distribution <- "Gamma"
    KS_g$Distance <- if (!is.null(var_KS_g$statistic)) {var_KS_g$statistic}
    else {"NA"}
    KS_g$PValue <- if (!is.null(var_KS_g$p.value)) {var_KS_g$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_g)
  }
  if ("exponential" %in% distributions) {
    var_exp <- MASS::fitdistr(var, "exponential")
    var_KS_exp <- ks.test(var, "pexp", rate = var_exp$estimate)
    KS_exp <- data.frame(matrix(ncol=0, nrow=1))
    KS_exp$Distribution <- "Exponential"
    KS_exp$Distance <- if (!is.null(var_KS_exp$statistic)) {var_KS_exp$statistic}
    else {"NA"}
    KS_exp$PValue <- if (!is.null(var_KS_exp$p.value)) {var_KS_exp$p.value}
    else {"NA"}
    KS_df <- rbind(KS_df, KS_exp)
  }
  
  KS_df$Distribution = as.factor(KS_df$Distribution)
  KS_df$Distance = as.numeric(KS_df$Distance)
  KS_df$PValue = as.numeric(format(as.numeric(KS_df$PValue),
                                   scientific = FALSE))
  KS_df$Distance <- round(KS_df$Distance, 3)
  KS_df$PValue <- round(KS_df$PValue, 3)
  
  return(KS_df)
}
multiCDF_cont <- function(var, seq_length = 50, distributions = "all"){
  # get a sequence from the minimum to maximum of x with length
  #equal to seq_length + 1
  var_seq <- seq(min(var), max(var), length.out = seq_length+1)
  # create real cumulative density for x
  var_cdf <- ecdf(var)(var_seq)
  # initialize df of x and the cumulative density
  cdf_df <- as.data.frame(var_seq)
  cdf_df$dens = var_cdf
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  
  if ("normal" %in% distributions) {
    var_n <- MASS::fitdistr(var, "normal")
    var_cdf_n <- pnorm(var_seq, mean=var_n$estimate[1],
                       sd = var_n$estimate[2])
    cdf_df$cdf_normal = var_cdf_n
  }
  if ("lognormal" %in% distributions) {
    var_ln <- MASS::fitdistr(var, "lognormal")
    var_cdf_ln <- plnorm(var_seq, meanlog=var_ln$estimate[1],
                         sdlog = var_ln$estimate[2])
    cdf_df$cdf_lognormal = var_cdf_ln
  }
  if ("gamma" %in% distributions) {
    var_g <- MASS::fitdistr(var, "gamma")
    var_cdf_g <- pgamma(var_seq, shape=var_g$estimate[1],
                        rate=var_g$estimate[2])
    cdf_df$cdf_gamma = var_cdf_g
  }
  if ("exponential" %in% distributions) {
    var_exp <- MASS::fitdistr(var, "exponential")
    var_cdf_exp <- pexp(var_seq, rate = var_exp$estimate)
    cdf_df$cdf_exponential = var_cdf_exp
  }
  
  return(cdf_df)
}
multiPDF_cont <- function(var, seq_length = 50, distributions = "all"){
  # get a sequence from the minimum to maximum of x with length
  #equal to seq_length + 1
  var_seq <- seq(min(var), max(var), length.out = seq_length+1)
  # create real density for x
  var_pdf <- density(var, n=seq_length+1)
  # initialize df of x and the real density
  pdf_df <- as.data.frame(var_seq)
  pdf_df$dens = var_pdf$y
  ## see if "all" is in distributions
  if ("all" %in% distributions) {
    distributions <- c("normal", "lognormal", "gamma", "exponential")
  }
  
  if ("normal" %in% distributions) {
    var_n <- MASS::fitdistr(var, "normal")
    var_pdf_n <- dnorm(var_seq, mean=var_n$estimate[1],
                       sd = var_n$estimate[2])
    pdf_df$pdf_normal = var_pdf_n
  }
  if ("lognormal" %in% distributions) {
    var_ln <- MASS::fitdistr(var, "lognormal")
    var_pdf_ln <- dlnorm(var_seq, meanlog=var_ln$estimate[1],
                         sdlog = var_ln$estimate[2])
    pdf_df$pdf_lognormal = var_pdf_ln
  }
  if ("gamma" %in% distributions) {
    var_g <- MASS::fitdistr(var, "gamma")
    var_pdf_g <- dgamma(var_seq, shape=var_g$estimate[1],
                        rate=var_g$estimate[2])
    pdf_df$pdf_gamma = var_pdf_g
  }
  if ("exponential" %in% distributions) {
    var_exp <- MASS::fitdistr(var, "exponential")
    var_pdf_exp <- dexp(var_seq, rate = var_exp$estimate)
    pdf_df$pdf_exponential = var_pdf_exp
  }
  ## return dataframe with pdfs
  return(pdf_df)
}
multiPDF_plot <- function (var, seq_length = 50, distributions = "all", palette = "oslo", var_name = NULL) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  # calculate PDFs
  data <- multiPDF_cont(var, seq_length, distributions)
  if (is.null(var_name)) {
    var_name <- unlist(strsplit(deparse(substitute(var)), split="[$]"))[2]
  }
  # create plot with real density
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(aes(x=var_seq, y=dens, color="Real Density"), linetype = 2, linewidth = 3)+
    ggplot2::xlab(var_name)+
    ggplot2::ylab("PDF")+
    ggplot2::guides(color=guide_legend(title="Distribution"))+
    ggplot2::theme_bw()
  # check for each type of distribution in the distributions, and add it if present
  if ("normal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=var_seq, y=pdf_normal, color='Normal'), linewidth = 2)
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x= var_seq, y=pdf_lognormal, color='Lognormal'), linewidth = 2)
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x= var_seq, y=pdf_gamma, color='Gamma'), linewidth = 2)
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x= var_seq, y=pdf_exponential, color='Exponential'), linewidth = 2)
  }
  p <- p +
    scico::scale_color_scico_d(begin=0.9, end=0.2, palette = palette)+
    ggplot2::theme(
      text = ggplot2::element_text(size=font_sizes[3]),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
      axis.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
    )
  return(p)
}
multiCDF_plot <- function (var, seq_length = 50, distributions = "all", palette = "oslo", var_name = NULL) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  # calculate CDFs
  data <- multiCDF_cont(var, seq_length, distributions)
  # if var_name is not provided, get it from the input variable
  if (is.null(var_name)) {
    var_name <- unlist(strsplit(deparse(substitute(var)), split="[$]"))[2]
  }
  # create plot with real density
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(aes(x=var_seq, y=dens, color="Real Distribution"), linetype = 2, linewidth = 3)+
    ggplot2::xlab(var_name)+
    ggplot2::ylab("CDF")+
    ggplot2::guides(color=guide_legend(title="Distribution"))+
    ggplot2::theme_bw()
  # check for each type of distribution in the distributions, and add it if present
  if ("normal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=var_seq, y=cdf_normal, color='Normal'), linewidth = 2)
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=var_seq, y=cdf_lognormal, color='Lognormal'), linewidth = 2)
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=var_seq, y=cdf_gamma, color='Gamma'), linewidth = 2)
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p + ggplot2::geom_line(aes(x=var_seq, y=cdf_exponential, color='Exponential'), linewidth = 2)
  }
  p <- p +
    scico::scale_color_scico_d(begin=0.9, end=0.2, palette = palette)+
    ggplot2::theme(
      text = ggplot2::element_text(size=font_sizes[3]),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
      axis.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
    )
  return(p)
}
predict_plot <- function(mod, data, rvar, pvar, group = NULL, length = 50, interval = "confidence", correction = "normal") {
    ### deparse variables
    d_pvar <- deparse(substitute(pvar))
    d_rvar <- deparse(substitute(rvar))
    d_group  <- deparse(substitute(group))
    ### get explicit names  of deparsed variables
    ### weird, but necessary for renaming the newdata (dx) columns \>_>/
    pvar_name <- colnames(data[d_pvar])
    rvar_name <- colnames(data[d_rvar])
    group_name  <- colnames(data[d_group])
    ## get group data ready
    groups  <- sort(unique(data[[d_group]]))
    ngroups <- length(groups)
    ## get predictor range for each group
    agg <- aggregate(data[[d_pvar]] ~ data[[d_group]], data = data, range)
    dx_pvar <- data.frame(pvar = numeric(0))
    for (i in 1:ngroups) {
      tpvar <- data.frame(pvar = seq(agg[[2]][i,1], agg[[2]][i,2], length = length))
      dx_pvar <- rbind(dx_pvar, tpvar)
    }
    dx <- data.frame(group = rep(agg[[1]], each = length),
                     pvar = dx_pvar)
    colnames(dx) <- c(group_name, pvar_name)
    ## make prediction
    if (interval == "confidence") {
      ### we don't need to explicitly declare that it's a confidence interval, the predict function defaults to it
      pred <- predict(mod, newdata = dx, se.fit = TRUE, type = "response")
      ### check for correction type
      if (correction == "exponential") {
        dx$mn <- exp(qnorm(0.5,   pred$fit, pred$se.fit))
        dx$lo <- exp(qnorm(0.025, pred$fit, pred$se.fit))
        dx$up <- exp(qnorm(0.975, pred$fit, pred$se.fit))
      } else if (correction == "logit") {
        dx$mn <- plogis(qnorm(0.5,   pred$fit, pred$se.fit))
        dx$lo <- plogis(qnorm(0.025, pred$fit, pred$se.fit))
        dx$up <- plogis(qnorm(0.975, pred$fit, pred$se.fit))
      } else {
        dx$mn <- qnorm(0.5,   pred$fit, pred$se.fit)
        dx$lo <- qnorm(0.025, pred$fit, pred$se.fit)
        dx$up <- qnorm(0.975, pred$fit, pred$se.fit)
      }
    } else { ### end confidence interval
      pred <- predict(mod, newdata = dx, se.fit = TRUE,
                      type = "response", interval = "prediction")
      ### check for correction type
      if (correction == "exponential") {
        dx$mn <- exp(pred$fit[,"fit"])
        dx$lo <- exp(pred$fit[,"lwr"])
        dx$up <- exp(pred$fit[,"upr"])
      } else if (correction == "logit") {
        dx$mn <- plogis(pred$fit[,"fit"])
        dx$lo <- plogis(pred$fit[,"lwr"])
        dx$up <- plogis(pred$fit[,"upr"])
      } else {
        dx$mn <- pred$fit[,"fit"]
        dx$lo <- pred$fit[,"lwr"]
        dx$up <- pred$fit[,"upr"]
      }
    } ### end prediction interval
    ## initialize plot with real data
    p <- ggplot2::ggplot() + 
      ggplot2::geom_point(data = data, ggplot2::aes(x=.data[[d_pvar]], y=.data[[d_rvar]], color=.data[[d_group]]))
    ## loop through treatments
    for (g in 1:ngroups) {
      flag <- which(dx[[d_group]] == groups[g])
      tdx <- dx[flag,]
      p <- p + 
        ggplot2::geom_line(data=tdx, ggplot2::aes(x=.data[[d_pvar]], y=lo, color = .data[[d_group]]),
                           linewidth=1, show.legend=FALSE)+
        ggplot2::geom_line(data=tdx, ggplot2::aes(x=.data[[d_pvar]], y=mn, color = .data[[d_group]]),
                           linewidth=2, show.legend=FALSE)+
        ggplot2::geom_line(data=tdx, ggplot2::aes(x=.data[[d_pvar]], y=up, color = .data[[d_group]]),
                           linewidth=1, show.legend=FALSE)+
        ggplot2::geom_ribbon(data=tdx, ggplot2::aes(x=.data[[d_pvar]], ymin=lo, ymax=up,
                                                    fill=.data[[d_group]]), alpha = 0.5)
    }
  ### make the plot look good
  p <- p +
    ggplot2::theme_bw()+
    ggplot2::theme(
      text = ggplot2::element_text(size=font_sizes[3]),
      legend.position="bottom",
      legend.title.position = "top",
      axis.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
      plot.subtitle = ggplot2::element_text(size=font_sizes[3], face = "italic")
    )
  return(p)
}
pca_plot <- function(group, pcavars) {
  gr <- sort(unique(group))
  Groups <- gr[match(group, gr)]
  p1 <- rda(pcavars)
  PC1val <- round(p1$CA$eig[1]/sum(p1$CA$eig), 4)*100
  PC2val <- round(p1$CA$eig[2]/sum(p1$CA$eig), 4)*100
  px <- scores(p1)$sites
  vx <- scores(p1)$species
  ggplot()+
    geom_point(data=px, aes(x=PC1, y=PC2, color=Groups, fill=Groups), size=3)+
    geom_segment(data = vx, aes(x=0, y=0, xend=PC1*.25, yend=PC2*.25), color = "black")+
    annotate("text", x=vx[,1]*.27, y=vx[,2]*.27, label = rownames(vx))+
    xlim(-1, 1)+
    xlab(paste0("PC1 (", PC1val, "%)"))+
    ylab(paste0("PC2 (", PC2val, "%)"))+
    theme_bw()+
    theme(
      legend.position="bottom",
      legend.title.position = "top"
    )
}
pca_data <- function(data, pcavars){
  p1 <- rda(pcavars)
  outdata <- cbind(data, p1$CA$u)
  return(outdata)
}
no_extremes <- function(data, var) {
  # deparse variable
  d_var <- deparse(substitute(var))
  # get quantiles
  Qvar <- quantile(data[[d_var]], probs=c(.25, .75), na.rm=TRUE)
  # get IQR
  iqr_var <- IQR(data[[d_var]])
  # subset data
  data <- subset(data, data[[d_var]] > (Qvar[1]-3*iqr_var) &
                   data[[d_var]] < (Qvar[2]+3*iqr_var))
  return(data)
}
## this is dumb
p_from_modsum <- function(modsum) {
  f <- modsum$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail=FALSE)
  attributes(p) <- NULL
  if (p > 0.0001) {
    p <- round(p, 4)
  } else if (p < 2.2e-16) {
    p = "< 2.2e-16"
  }
  return(p)
}

# pre-calculate models that don't have user inputs (PCRs and fruit mods)
## tim
### tim fluoro
tim_pca <- rda(scaled_tim_fluoro_vars)
tim_pcr_data <- pca_data(mod_data_tim_fluoro, scaled_tim_fluoro_vars)
tim_pcr_gsw <- lm(log(gsw) ~ Treatment + PC1 + PC2, data = tim_pcr_data)
tim_pcr_gsw_pmod <- lm(log(gsw) ~ Treatment + PC1, data = tim_pcr_data)
tim_pcr_gsw_sum <- summary(tim_pcr_gsw)
tim_pcr_ps2 <- lm(logit(PhiPS2) ~ Treatment + PC1 + PC2, data = tim_pcr_data)
tim_pcr_ps2_pmod <- lm(logit(PhiPS2) ~ Treatment + PC1, data = tim_pcr_data)
tim_pcr_ps2_sum <- summary(tim_pcr_ps2)
### tim height
tim_height_mod <- lm(log(Height) ~ Treatment + DaysFromGermination, data = data_tim_height)
tim_height_mod_sum <- summary(tim_height_mod)
tim_height_letters <- cld(glht(tim_height_mod, linfct = mcp(Treatment = "Tukey")))
### tim ds
tim_rs_length_mod <- lm(logit(RS_Length) ~ Treatment, data = data_tim_ds)
tim_rs_length_mod_sum <- summary(tim_rs_length_mod)
tim_rs_length_letters <- cld(glht(tim_rs_length_mod, linfct = mcp(Treatment = "Tukey")))
tim_rs_mass_mod <- lm(logit(RS_Mass) ~ Treatment, data = data_tim_ds)
tim_rs_mass_mod_sum <- summary(tim_rs_mass_mod)
tim_rs_mass_letters <- cld(glht(tim_rs_mass_mod, linfct = mcp(Treatment = "Tukey")))
## til
### til fluoro
til_pca <- rda(scaled_til_fluoro_vars)
til_pcr_data <- pca_data(mod_data_til_fluoro, scaled_til_fluoro_vars)
til_pcr_gsw <- lm(log(gsw) ~ Treatment + PC1 + PC2, data = til_pcr_data)
til_pcr_gsw_pmod <- lm(log(gsw) ~ Treatment + PC1, data = til_pcr_data)
til_pcr_gsw_sum <- summary(til_pcr_gsw)
til_pcr_ps2 <- lm(LogitPhiPS2 ~ Treatment + PC1 + PC2, data = til_pcr_data)
til_pcr_ps2_pmod <- lm(LogitPhiPS2 ~ Treatment + PC1, data = til_pcr_data)
til_pcr_ps2_sum <- summary(til_pcr_ps2)
### til fruit
til_mass_mod <- lm(log(Mass_mean) ~ Treatment, data = data_til_fruit_summary)
til_mass_mod_sum <- summary(til_mass_mod)
til_mass_letters <- cld(glht(til_mass_mod, linfct = mcp(Treatment = "Tukey")))
til_fc_mod <- glm(Fruit_sum ~ Treatment, data = data_til_fruit_summary, family = poisson())
til_fc_mod_sum <- summary(til_fc_mod)
til_fc_letters <- cld(glht(til_fc_mod, linfct = mcp(Treatment = "Tukey")))
til_sug_mod <- lm(logit(pSugar_mean) ~ Treatment + log(Mass_mean), data = data_til_fruit_summary)
til_sug_mod_sum <- summary(til_sug_mod)
til_sug_letters <- cld(glht(til_sug_mod, linfct = mcp(Treatment = "Tukey")))
til_ber_mod <- lm(LogitpBER ~ Treatment, data = no_extremes(data_til_fruit_summary, LogitpBER))
til_ber_mod_sum <- summary(til_ber_mod)
til_ber_letters <- cld(glht(til_ber_mod, linfct = mcp(Treatment = "Tukey")))
## tit
### tit fluoro
tit_pca <- rda(scaled_tit_fluoro_vars)
tit_pcr_data <- pca_data(mod_data_tit_fluoro, scaled_tit_fluoro_vars)
tit_pcr_gsw <- lm(log(gsw) ~ Treatment + PC1 + PC2, data = tit_pcr_data)
tit_pcr_gsw_pmod <- lm(log(gsw) ~ Treatment + PC1, data = tit_pcr_data)
tit_pcr_gsw_sum <- summary(tit_pcr_gsw)
tit_pcr_ps2 <- lm(LogitPhiPS2 ~ Treatment + PC1 + PC2, data = tit_pcr_data)
tit_pcr_ps2_pmod <- lm(LogitPhiPS2 ~ Treatment + PC1, data = tit_pcr_data)
tit_pcr_ps2_sum <- summary(tit_pcr_ps2)
### tit fruit
tit_mass_mod <- lm(log(Mass_mean) ~ Treatment, data = data_tit_fruit_summary)
tit_mass_mod_sum <- summary(tit_mass_mod)
tit_mass_letters <- cld(glht(tit_mass_mod, linfct = mcp(Treatment = "Tukey")))
tit_fc_mod <- glm(Fruit_sum ~ Treatment, data = data_tit_fruit_summary, family = poisson())
tit_fc_mod_sum <- summary(tit_fc_mod)
tit_fc_letters <- cld(glht(tit_fc_mod, linfct = mcp(Treatment = "Tukey")))
tit_sug_mod <- lm(logit(pSugar_mean) ~ Treatment + log(Mass_mean), data = data_tit_fruit_summary)
tit_sug_mod_sum <- summary(tit_sug_mod)
tit_sug_letters <- cld(glht(tit_sug_mod, linfct = mcp(Treatment = "Tukey")))
tit_ber_mod <- lm(LogitpBER ~ Treatment, data = no_extremes(data_tit_fruit_summary, LogitpBER))
tit_ber_mod_sum <- summary(tit_ber_mod)
tit_ber_letters <- cld(glht(tit_ber_mod, linfct = mcp(Treatment = "Tukey")))

# options popover
gear <- popover(bs_icon("gear"),
                selectInput("palette","Select color palette",
                            choices = p_palettes, selected = "oslo"),
                title = "Options",
                class = "bg-primary"
                )

# github link
link_github <- tags$a(bs_icon("GitHub"), href = "https://github.com/zachpeagler/tomato-inoculant-app")

##### UI #####
ui <- navbarPage(collapsible = TRUE,
  title = "Tomato Inoculants",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  ##### BACKGROUND NAV PANEL #####
  nav_panel("Background",
    card(card_header("The Green Revolution and Synthetic Fertilizers", class = "bg-primary", style = "font-size: 20px"),
      layout_columns(col_widths = c(5,7),
          card(
         img(src="hab.png"),
         markdown("Image from [USGS EROS](https://landsat.gsfc.nasa.gov/article/satellites-on-toxic-algae-patrol/)")
       ),
       markdown("
        In the 20th century, new methods were required to feed an expanding human population,
        and the solution was the creation of synthetic fertilizers: inorganic sources of nitrogen (N),
        phosphorous (P), and sodium (K) that (along with synthetic pesticides) became the foundation
        for the Green Revolution. However, these solutions quickly became problems in their own right.
        Synthetic pesticides wreaked havoc on the ecosystem, destroying large bird populations and causing
        cancer at alarming rates. Synthetic fertilizers followed suit with harmful algal blooms. In recent years,
        the 4R method has garnered much attention, where synthetic fertilizers, when applied with the four R's in mind: Right time, Right place,
        Right source, and Right rate. However, this is not enough to stop the historic runoff rates that we're seeing.
        Up to 60% of applied fertilizers run off into the environment, and with fertilizers being more expensive than ever, 
        taking up over a third of the average American farmer's budget, with fertilizer prices tripling in 
        recent years, we can do better.
        ")
      )
    ),
    card(card_header("Microbes as sustainable agriculture solutions", class = "bg-secondary", style = "font-size: 20px"),
     layout_columns(col_widths = c(7,5),
       markdown("
        In recent years, microbes have garnered much attention for their potential in sustainable agriculture.
        A recent report has stated that microbial fertilizers have the potential to generate billions in social benefits by reducing the emissions from synthetic fertilizers (UC Innovation Commission, 2023).
        Plant growth promoting bacteria (PGPB) and arbuscular mycorrhical fungi (AMF)
        are two of the most common microbial archetypes for sustainable agriculture purposes. PGPBs can do lots of things,
        but act in four main ways: <br>
        - **Biofertilizer**: nitrogen fixation and phosphorous solubilization
        - **Phytostimulator**: production of phytohormones (auxins, gibberellins, cytokinins)
        - **Stress tolerance enhancer**: ACC deaminase production, proline and amine synthesis, antioxidative enzyme production
        - **Biopesticide/biocontroller**: Antibiotic production, competitive exclusion, induced systemic resistance <br>
        Applications of microbial biostimulants have been found to reduce fertilizer requirements by 25% and increase crop yield by 17.9% on average (Adesemoye & Kloepper, 2009 ; Li et al., 2022).
        While biostimulants show promise as a potential alternative for or amendment to synthetic fertilizers, microbial biostimulants are currently held back from widespread implementation due to 
        logistical constraints arising from sub-optimal carrier materials and a lack of awareness from farmers. Carrier materials are necessary for inoculant production, as without a carrier most
        PGPB populations decline rapidly after inoculation (Bashan et al., 2014). Multiple studies have shown that combinations of PGPBs are more effective than lone PGPBs (Madhaiyan et al., 2010; He et al., 2019), 
        with the exact biostimulant load depending on the target crop and soil conditions,
        warranting further research. The overarching objective of my research was to test 
        chitosan as a carrier material for PGPBs, ranging from a single species (M. oryzae) 
        to a mixture of multiple species, and test the impact of timing, location, and method of application on plant growth and health. 
        
        > Note: I did not use AMF in this study, but I do recommend the use of AMFs in future 
        sustainable agriculture solutions, if economically viable.
        "),
       card(
         img(src="moryzae.jpg"),
         markdown("A petri dish containing *Methylobacterium oryzae CBMB20* grown on DSMZ Medium 1 + 1% Methanol.")
       )
     ) # end column wrap
    ), # end microbe card
    card(card_header("Biopolymer immobilization of microbes", class = "bg-primary", style = "font-size: 20px"),
     layout_columns(col_widths = c(5,7),
     markdown("
      Then comes the question of how we most effectively deliver the microbes to the plants. This is the
      core question that I was interested in answering with my thesis.
      Current carrier materials can be classified as either wet or dry, with dry inoculants (peat, clay, lignite, etc.) 
      having longer shelf lives, lower contamination risk, and cheaper storage and transportation costs than liquid
      inoculants (Bashan et al., 2014). Dry carriers can be applied directly to the soil or dissolved in water for 
      a foliar inoculation, while wet carriers are applied almost exclusively as a foliar inoculation. 
      In a broad review (Li et al., 2022), soil inoculation has generally been shown to be more effective at increasing 
      yield than foliar inoculation. Dry inoculants have shown inconsistent results in the field, 
      shown unreliable performance in different environments, are often expensive, and are easily contaminated, 
      preventing their widespread implementation.(Bashan et al., 2014) 
      To achieve the next step towards widespread biostimulant use, inoculation methods
      must prove that they can reliably maintain microbial viability and efficacy in the field 
      after long-term storage and exposure to environmental stressors, all while being cheap, safe, and easy to produce and use (Bashan et al., 2014).
      <br>
      A new inoculation method that has shown promise is encapsulation, the process of immobilizing
      biostimulants in droplet-derived granules which can then be dried for storage and transport. 
      Encapsulation requires an encapsulation base and a cross-linker. One encapsulation base that has 
      shown great promise is chitosan, itself a biostimulant that has been shown to increase plant growth 
      and stress tolerance in tomato plants and can entrap microbial biostimulants in granules (Chanratana et al., 2019). 
      Another encapsulation base, alginate, is also a biostimulant that can entrap microbes (Joe et al., 2012). 
      Encapsulation has been shown to increase microbial viability in the presence of desiccation stress, 
      as well as allow for a controlled release of the inoculant (Schoebitz et al., 2013).
      Studies have shown that both bacteria and fungi can be encapsulated and maintain their viability, 
      but many of these studies have been done with alginate rather than chitosan, despite chitosan having 
      been shown to be more effective at maintaining microbial viability than alginate (Declerck, 1996; Bashan & Gonzalez, 1999; Chanratana et al., 2018). 
      Studies also suggest that encapsulation is necessary for PGPBs and AMFs to achieve maximum effectiveness and viability 
      while also being practical for implementation by farmers (Bashan et al., 2014; Berninger et al., 2018).
      "),
     div(
       card(
       img(src="cbg_comparison.png"),
       markdown("A comparison of chitosan biostimulant granules at different desiccation points: **a** is fresh,
                **b** is at 24 hours, **c** is at 72 hours.")
       ),
       card(
       img(src="cbg_morphology.png"),
       markdown("Chitosan bacterial granules at 400x magnification. This photo does a good job
                showing the morphological variation between beads from the same batch.")
       )
     )
     ) # end column layout
    ),
    card(card_header("Model organism: Tomato", class = "bg-secondary", style = "font-size: 20px"),
      layout_column_wrap(
        card(
        img(src="tomato.png"),
        markdown("Healthy tomato")
        ),
     markdown("
      Tomatoes are the second most-grown crop in the United States,
      grow like weeds, produce lots of fruit, and have easily identifiable stress responses,
      making them an excellent model organism. For this study, we used tomato cultivar **BHN 589**, a
      determinate variety (they produce one flush of fruit and are done). One of the most common
      tomato stress responses is blossom-end rot (BER) caused by a calcium deficiency that
      is often associated with salt stress. BER often leads to secondary infections, as shown in the picture to the right,
      and renders the fruit unmarketable.
      <br>
      
      > Note: While tomatoes are an excellent model organism for measuring fruit yield and 
      stress response, a shift to shorter lifecycle crops may help to speed up development.
      Tomatoes take months to grow, with our studies generally growing one round of tomatoes from April to October. 
      As you may be able to guess, this does not do a great job facilitating a rapid turnaround of results.
      "),
        card(
         img(src="ber-severe.png"),
         markdown("Tomato with blossom-end rot")
        ),
      )
    ),
    card(card_header("Stomates: Plant Lungs", class = "bg-primary", style = "font-size: 20px"),
      layout_column_wrap(
        markdown("
        Stomates are holes that plants breathe through. Carbon dioxide goes in, oxygen and water comes out.
        By measuring the rate at which molecules move through the stomates, we can get a relative
        estimate of the plant's photosynthesis and water use.
        "),
        card(
        img(src="stomates.png"),
        markdown("Tomato stomates at 1000x magnification.")
        )
      )
    ),
    card(card_header("Photosystem II and you", class = "bg-secondary", style = "font-size: 20px"),
      layout_column_wrap(
        card(img(src="thylakoid_membrane.png"),
          markdown("Diagram of the photosystem complexes on the thylakoid membrane, courtesy of [Wikipedia](https://en.wikipedia.org/wiki/Photosystem).")
        ),
        markdown("
        Plants perform photosynthesis inside of a specialized organelle called the chloroplast. <br>
        
        Specifically, chloroplasts have these things called thylakoids, and photosynthesis happens *across*
        the thylakoid membrane. It's complicated, but for our purposes we can think of it like a gas station (or charging center).
        Depleted molecules come in (ADP and NADP) and exit *recharged* (ATP and NADPH). There's a series of reactions that happen
        in two distinct reaction centers, Photosystem I and Photosystem II. By doing some funky stuff with light, we can use math
        to figure out the efficiency of Photosystem II (PhiPS2). PhiPS2 is calculated as (maximum fluorescence - steady state fluorescence)/maximum fluorescence,
        and is useful for estimating changes in the quantum yield of non-cyclic electron transport. <br>
        
        > For a more comprehensive explanation of PhiPS2, check out 
        [Genty *et al*., 1989](https://www.sciencedirect.com/science/article/abs/pii/S0304416589800169) or
        for a simpler explanation, the [chlorophyll fluorescence wikipedia page](https://en.wikipedia.org/wiki/Chlorophyll_fluorescence).
        ")
      )
    ),
    card(card_header("Pests and Pathogens", class = "bg-primary", style = "font-size: 20px"),
      markdown("
      These trials were performed in a greenhouse, and the plants accumulated a number of 
      ailments over the course of the trials. The plants were subject to aphid, spidermite, and
      whitefly infestations, for which we attempted treatment with organic pesticides.
      Tissue infested with spidermites was removed as it was discovered, 
      with an equal amount of tissue being removed from every other plant. These infections
      were not bad enough to warrant removing any plant's data from the analysis.
      "),
      div(layout_column_wrap(
        card(card_header("Aphids", class = "bg-primary"),
             img(src="aphids.jpg"),
             markdown("Image courtesy of [University of Maryland](https://extension.umd.edu/resource/aphids-vegetables/)")
        ),
        card(card_header("Spidermites", class = "bg-secondary"),
             img(src="spidermites.jpeg"),
             markdown("Image courtesy of [Southern Botanical](https://southernbotanical.com/the-benchmark/do-not-ignore-these-early-signs-of-spider-mites/)")
        ),
        card(card_header("Whiteflies"),
             img(src="whiteflies.jpg"),
             markdown("Image courtesy of [Utah State University](https://extension.usu.edu/planthealth/ipm/notes_ag/hemp-whiteflies)")
        )
      ))
    )
  ),
  ##### DEVELOPMENT NAV PANEL #####
  nav_panel("Development",
    card(card_header("First Steps: From the Literature to the Lab", class = "bg-primary", style = "font-size: 20px"),
      markdown("It didn't work, but *why* didn't it work?")
    ),
    card(card_header("Understanding Chitosan", class = "bg-secondary", style = "font-size: 20px"),
         markdown("chitosan sucks")
    ),
    card(card_header("Nozzles, Testbenches, and Desiccation Chambers: Oh my", class = "bg-primary", style = "font-size: 20px"),
         markdown("lots of 3d printing")
    ),
    card(card_header("Final Protocol", class = "bg-secondary", style = "font-size: 20px"),
         markdown("3d printing bad")
    )
  ),
  ##### TRIALS NAV PANEL #####
  nav_panel("Trials",
    tabsetPanel(
      ##### TOMATO INOCULANT METHOD #####
      tabPanel("Inoculant Method Trial",
        tabsetPanel(
          tabPanel("Exploratory",
            div(style = "padding: 10px",
            markdown("> Quick tip: this tab uses **accordions**! Click or tap the accordion panel title to expand/shrink the panel
                   and switch between different exploratory graph types.")
            ),
          accordion(
            accordion_panel(title = "Density and Distribution",
              markdown("
                A critical component of an exploratory data analysis is the creation of **probability density function** (PDF) plots
                and **cumulative distribution function** (CDF) plots. They tell us the shape the data takes and helps inform if we need to 
                apply a mathematical correction or use a certain type of distribution in our statistical model. I'm only
                making dedicated PDF and CDF plots for our response variables and not our explanatory variables. However,
                we can see the shape of our explanatory variables using histograms (a couple accordion panels down).
                "),
              card(card_header("Fluorescence", class = "bg-primary", style = "font-size: 20px"),
                layout_sidebar(sidebar=sidebar(
                  selectInput("tim_fluoro_dist_var", "Variable", choices = tim_fluoro_vars,
                               selected = "gsw"),
                  checkboxGroupInput("tim_fluoro_dists", "Distributions", choices=dists, 
                                      selected=c("normal", "lognormal", "gamma")),
                  sliderInput("tim_fluoro_len", "Length to Test Distributions Over", min=1,
                              max=500, value=100)
                  ), # end sidebar - but not sidebar LAYOUT
                  div(
                    layout_column_wrap(
                      div(
                        htmlOutput("tim_fluoro_pdf_title"),
                        plotOutput("tim_fluoro_pdf")
                      ),
                      div(
                        htmlOutput("tim_fluoro_cdf_title"),
                        plotOutput("tim_fluoro_cdf")
                      )
                    )
                  ),
                  div(
                    htmlOutput("tim_fluoro_KS_title"),
                    verbatimTextOutput("tim_fluoro_KS")
                  ),
                  div(style="border-left: 5px solid", 
                    markdown("
                      > A note on PhiPS2 distributions: PhiPS2 is a **unitless ratio** on a scale of 0-1, so we don't need to create PDF and CDF plots and perform KS tests
                      (you still have the option to, but this is an instance where we use our *statistical reasoning*).
                      Instead, we logit transform PhiPS2 and use the logit transformed version in our models. <br>
                      > For a more comprehensive explanation of PhiPS2, check out 
                      [Genty *et al*., 1989](https://www.sciencedirect.com/science/article/abs/pii/S0304416589800169) or
                      for a simpler explanation, the [chlorophyll fluorescence wikipedia page](https://en.wikipedia.org/wiki/Chlorophyll_fluorescence)."
                  ))
                ) # end sidebar layout
              ), # end fluorescence card
              card(card_header("Height", class = "bg-primary", style = "font-size: 20px"),
                   layout_sidebar(sidebar=sidebar(
                     selectInput("tim_height_dist_var", "Variable", choices = tim_height_vars,
                                 selected = "Height"),
                     checkboxGroupInput("tim_height_dists", "Distributions", choices=dists, 
                                        selected=c("normal", "lognormal", "gamma")),
                     sliderInput("tim_height_len", "Length to Test Distributions Over", min=1,
                                 max=500, value=100)
                     ), # end sidebar - but not sidebar LAYOUT
                     div(
                       layout_column_wrap(
                         div(
                           htmlOutput("tim_height_pdf_title"),
                           plotOutput("tim_height_pdf")
                         ),
                         div(
                           htmlOutput("tim_height_cdf_title"),
                           plotOutput("tim_height_cdf")
                         )
                       )
                     ),
                     div(
                       htmlOutput("tim_height_KS_title"),
                       verbatimTextOutput("tim_height_KS")
                     )
                   ) # end sidebar layout
              ), # end height card
              card(card_header("Destructive Sampling", class = "bg-primary", style = "font-size: 20px"),
                 layout_sidebar(sidebar=sidebar(
                    selectInput("tim_ds_dist_var", "Variable", choices = tim_ds_vars,
                                selected = "AG_Length"),
                    checkboxGroupInput("tim_ds_dists", "Distributions", choices=dists, 
                                       selected=c("normal", "lognormal", "gamma")),
                    sliderInput("tim_ds_len", "Length to Test Distributions Over", min=1,
                                max=500, value=100)
                  ), # end sidebar - but not sidebar LAYOUT
                  div(
                   layout_column_wrap(
                     div(
                       htmlOutput("tim_ds_pdf_title"),
                       plotOutput("tim_ds_pdf")
                     ),
                     div(
                       htmlOutput("tim_ds_cdf_title"),
                       plotOutput("tim_ds_cdf")
                     )
                   )
                  ),
                  div(
                    htmlOutput("tim_ds_KS_title"),
                    verbatimTextOutput("tim_ds_KS")
                  ),
                  markdown("
                    > It's important to note that the root:shoot ratios **RS_Length** and **RS_Mass** are unitless ratios, so we will end up
                    logit transforming them for use in models. **RS_Length** is calculated as BG_Length/AG_Length and 
                    **RS_Mass** is calculated as BG_Mass/AG_Mass.
                  ")
                ) # end sidebar layout
              ) # end DS card
            ), # end dist accordion panel
            accordion_panel(title="Histograms",
              card(card_header("Fluorescence Histogram", class = "bg-primary", style = "font-size: 20px"),
                layout_sidebar(sidebar=sidebar(
                  selectInput("tim_fluoro_hist_var", "Select X Variable",
                              choices = tim_fluoro_vars, selected = "AmbientHumidity"),
                  selectInput("tim_fluoro_hist_color", "Select Color Variable",
                              choices = tim_fluoro_vars_d, selected = "Treatment"),
                  sliderInput("tim_fluoro_hist_bins", "Number of Bins",
                              value = 30, min = 2, max = 100)
                  ), # end sidebar
                  plotOutput("tim_fluoro_hist")
              )), # end fluoro hist card
              card(card_header("Height Histogram", class = "bg-primary", style = "font-size: 20px"),
                layout_sidebar(sidebar=sidebar(
                  selectInput("tim_height_hist_var", "Select X Variable",
                              choices = tim_height_vars, selected = "Height"),
                  selectInput("tim_height_hist_color", "Select Color Variable",
                              choices = tim_height_vars_d, selected = "Treatment"),
                  sliderInput("tim_height_hist_bins", "Number of Bins",
                              value = 30, min = 2, max = 100)
                  ), # end sidebar
                  plotOutput("tim_height_hist")
              )), # end height hist card
              card(card_header("Destructive Sampling Histogram", class = "bg-primary", style = "font-size: 20px"),
                   layout_sidebar(sidebar=sidebar(
                     selectInput("tim_ds_hist_var", "Select X Variable",
                                 choices = tim_ds_vars, selected = "AG_Length"),
                     selectInput("tim_ds_hist_color", "Select Color Variable",
                                 choices = tim_ds_vars_d, selected = "Treatment"),
                     sliderInput("tim_ds_hist_bins", "Number of Bins",
                                 value = 30, min = 2, max = 100)
                   ), # end sidebar
                   plotOutput("tim_ds_hist")
                   )) # end ds hist card
            ), # end hist accordion panel
              accordion_panel(title = "Scatter Plots",
                card(card_header("Fluorescence Scatter", class = "bg-primary", style = "font-size: 20px"),
                    layout_sidebar(sidebar = sidebar(
                      selectInput("tim_fluoro_scatter_x","X Variable",
                                  choices = all_tim_fluoro_vars, selected = "AmbientHumidity"),
                      selectInput("tim_fluoro_scatter_y","Y Variable",
                                  choices = all_tim_fluoro_vars, selected = "gsw"),
                      selectInput("tim_fluoro_scatter_col","Color Variable",
                                  choices = all_tim_fluoro_vars, selected = "Treatment"),
                      selectInput("tim_fluoro_scatter_shape", "Shape Variable",
                                  choices = tim_fluoro_vars_d, selected = "Treatment"),
                      sliderInput("tim_fluoro_scatter_jit", "Jitter Amount",
                                  min=0, max=10, value =3),
                      sliderInput("tim_fluoro_scatter_size", "Point Size",
                                  min = 1, max=10, value = 3),
                      checkboxInput("tim_fluoro_scatter_fwrap", "Individual Plot Per Treatment", FALSE)
                    ), # end sidebar
                    card_body(plotOutput("tim_fluoro_scatter"))
                    ) # end sidebar layout
                ), # end fluoro scatter plot
                card(card_header("Height Scatter", class = "bg-primary", style = "font-size: 20px"),
                    layout_sidebar(sidebar = sidebar(
                      selectInput("tim_height_scatter_x","X Variable",
                                  choices = all_tim_height_vars, selected = "DaysFromGermination"),
                      selectInput("tim_height_scatter_y","Y Variable",
                                  choices = all_tim_height_vars, selected = "Height"),
                      selectInput("tim_height_scatter_col","Color Variable",
                                  choices = all_tim_height_vars, selected = "Treatment"),
                      selectInput("tim_height_scatter_shape", "Shape Variable",
                                  choices = tim_height_vars_d, selected = "Treatment"),
                      sliderInput("tim_height_scatter_jit", "Jitter Amount",
                                  min=0, max=10, value =5),
                      sliderInput("tim_height_scatter_size", "Point Size",
                                  min = 1, max=10, value = 3),
                      checkboxInput("tim_height_scatter_fwrap", "Individual Plot Per Treatment", FALSE)
                    ), # end sidebar
                    card_body(plotOutput("tim_height_scatter"))
                    ) # end sidebar layout
                ), # end fluoro scatter plot
                card(card_header("Destructive Sampling Scatter", class = "bg-primary", style = "font-size: 20px"),
                    layout_sidebar(sidebar = sidebar(
                      selectInput("tim_ds_scatter_x","X Variable",
                                  choices = all_tim_ds_vars, selected = "AG_Length"),
                      selectInput("tim_ds_scatter_y","Y Variable",
                                  choices = all_tim_ds_vars, selected = "AG_Mass"),
                      selectInput("tim_ds_scatter_col","Color Variable",
                                  choices = all_tim_ds_vars, selected = "Treatment"),
                      selectInput("tim_ds_scatter_shape", "Shape Variable",
                                  choices = tim_ds_vars_d, selected = "Treatment"),
                      sliderInput("tim_ds_scatter_jit", "Jitter Amount",
                                  min=0, max=10, value =3),
                      sliderInput("tim_ds_scatter_size", "Point Size",
                                  min = 1, max=10, value = 3),
                      checkboxInput("tim_ds_scatter_fwrap", "Individual Plot Per Treatment", FALSE)
                    ), # end sidebar
                    card_body(plotOutput("tim_ds_scatter"))
                    ) # end sidebar layout
                ), # end fluoro scatter plot
            ), # end scatter plot accordion panel
            accordion_panel(title="Box Plots",
              card(card_header("Fluorescence Boxplot", class = "bg-primary", style = "font-size: 20px"),
                   layout_sidebar(sidebar = sidebar(
                     selectInput("tim_fluoro_box_x","X Variable",
                                 choices = tim_fluoro_vars_d, selected = "Treatment"),
                     selectInput("tim_fluoro_box_y","Y Variable",
                                 choices = tim_fluoro_vars, selected = "gsw")
                   ), # end sidebar
                   plotOutput("tim_fluoro_box")
                   )),
              card(card_header("Height Boxplot", class = "bg-primary", style = "font-size: 20px"),
                   layout_sidebar(sidebar = sidebar(
                     selectInput("tim_height_box_x","X Variable",
                                 choices = tim_height_vars_d, selected = "Treatment"),
                     selectInput("tim_height_box_y","Y Variable",
                                 choices = tim_height_vars, selected = "Height")
                   ), # end sidebar
                   plotOutput("tim_height_box")
                   )),
              card(card_header("Destructive Sampling Boxplot", class = "bg-primary", style = "font-size: 20px"),
                   layout_sidebar(sidebar = sidebar(
                     selectInput("tim_ds_box_x","X Variable",
                                 choices = tim_ds_vars_d, selected = "Treatment"),
                     selectInput("tim_ds_box_y","Y Variable",
                                 choices = tim_ds_vars, selected = "AG_Length")
                   ), # end sidebar
                   plotOutput("tim_ds_box")
                   ))
            ) # end boxplot accordion panel
          ) # end accordion
          ), # end exploratory tab panel
        tabPanel("Statistics",
          accordion(
            accordion_panel("Fluorescence",
              card(card_header("Stomatal conductance (gsw)", class = "bg-primary", style = "font-size: 20px"),
                selectInput("tim_gsw_mod_var", "Predictor Variable",
                            choices = fluoro_mod_var_names, selected = "AmbientHumidity"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("tim_gsw_mod_summary")
                    )
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("tim_gsw_mod_call")
                         ),
                    value_box(
                      title = "GSW Model AIC",
                      value = textOutput("tim_gsw_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "GSW Model p-value",
                      value = textOutput("tim_gsw_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "GSW Model R^2",
                      value = textOutput("tim_gsw_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("tim_gsw_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ), # end div
              ), # end gsw stats card
              card(card_header("Photosystem II Efficiency (PhiPS2)", class = "bg-primary", style = "font-size: 20px"),
                   selectInput("tim_ps2_mod_var", "Predictor Variable",
                               choices = fluoro_mod_var_names, selected = "AmbientHumidity"),
                   div(layout_columns(col_widths = c(7,5),
                        div(
                          card(card_header("Model Summary"),
                               verbatimTextOutput("tim_ps2_mod_summary")
                          )
                        ),# end model summary div
                        div(# value boxes for AIC and r^2
                          card(card_header("Model Call", class = "bg-primary"),
                               verbatimTextOutput("tim_ps2_mod_call")
                          ),
                          value_box(
                            title = "PhiPS2 Model AIC",
                            value = textOutput("tim_ps2_aic"),
                            theme = "bg-primary",
                            width = 0.2
                          ),
                          value_box(
                            title = "PhiPS2 Model p-value",
                            value = textOutput("tim_ps2_p"),
                            width = 0.2
                          ),
                          value_box(
                            title = "PhiPS2 Model R^2",
                            value = textOutput("tim_ps2_r2"),
                            theme = "bg-secondary",
                            width = 0.2
                          ),
                          card(card_header("Treatment Letters", class="bg-secondary"),
                               verbatimTextOutput("tim_ps2_letters")
                          )
                        ) # end value box div
                   ) # end column wrap
                ) # end div
              ), # end ps2 stats card
              card(card_header("Multivariate", class = "bg-secondary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(6,6),
                  div(
                    markdown("
                      Maybe instead of looking at a single environmental variable, we want to look at all of them (or at least the ones
                      that actually make an impact on our target response variables). To accomplish this, we can use **principal component analysis** (PCA).
                      First, we scale our environmental variables so they all have an equal influence on the output, then use the **rda** function from the **vegan** package
                      to reduce the dimensionality."
                    ),
                    card(card_header("PCA Summary"), 
                       verbatimTextOutput("tim_fluoro_pca_summary")
                    )
                  ),
                  plotOutput("tim_fluoro_pca")
                )),
                div(layout_column_wrap(
                  card(card_header("Multivariate Stomatal Conductance (GSW)", class = "bg-primary", style = "font-size: 20px"),
                       div(# value boxes for AIC and r^2
                         card(card_header("Model Summary", class = "bg-primary"),
                              verbatimTextOutput("tim_pcr_gsw_summary"),
                              max_height = 500
                         ),
                         value_box(
                           title = "Multivariate GSW AIC",
                           value = textOutput("tim_pcr_gsw_aic"),
                           theme = "bg-primary",
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate GSW p-value",
                           value = textOutput("tim_pcr_gsw_p"),
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate GSW R^2",
                           value = textOutput("tim_pcr_gsw_r2"),
                           theme = "bg-secondary",
                           width = 0.2
                         ),
                         card(card_header("Treatment Letters", class="bg-secondary"),
                              verbatimTextOutput("tim_pcr_gsw_letters")
                         )
                       ), # end value box div
                       plotOutput("tim_pcr_gsw_pred")
                       ),
                  card(card_header("Multivariate Photosystem II Efficiency (PhiPS2)", class = "bg-primary", style = "font-size: 20px"),
                       div(# value boxes for AIC and r^2
                         card(card_header("Model Summary", class = "bg-primary"),
                              verbatimTextOutput("tim_pcr_ps2_summary"),
                              max_height = 500
                         ),
                         value_box(
                           title = "Multivariate PhiPS2 AIC",
                           value = textOutput("tim_pcr_ps2_aic"),
                           theme = "bg-primary",
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate PhiPS2 p-value",
                           value = textOutput("tim_pcr_ps2_p"),
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate PhiPS2 R^2",
                           value = textOutput("tim_pcr_ps2_r2"),
                           theme = "bg-secondary",
                           width = 0.2
                         ),
                         card(card_header("Treatment Letters", class="bg-secondary"),
                              verbatimTextOutput("tim_pcr_ps2_letters")
                         )
                       ), # end value box div
                       plotOutput("tim_pcr_ps2_pred")
                  ), # end phips2 card
                )) # end column wrap and div
              ) # end multivariate card
            ),
            accordion_panel("Height",
              card(card_header("Height", class = "bg-primary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("tim_height_mod_summary")
                    ),
                    plotOutput("tim_height_pred")
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("tim_height_mod_call")
                         ),
                    value_box(
                      title = "Height Model AIC",
                      value = textOutput("tim_height_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "Height Model p-value",
                      value = textOutput("tim_height_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "Height Model R^2",
                      value = textOutput("tim_height_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("tim_height_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ) # end div
              ) # end height stats card
            ), # end height accordion panel
            accordion_panel("Destructive Sampling",
              card(card_header("Root:Shoot Length", class = "bg-primary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("tim_rs_length_mod_summary")
                    ),
                    plotOutput("tim_rs_length_annotated")
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("tim_rs_length_mod_call")
                         ),
                    value_box(
                      title = "R:S Length Model AIC",
                      value = textOutput("tim_rs_length_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "R:S Length Model p-value",
                      value = textOutput("tim_rs_length_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "R:S Length Model R^2",
                      value = textOutput("tim_rs_length_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("tim_rs_length_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ) # end div
              ), # end RS length stats card
              card(card_header("Root:Shoot Mass", class = "bg-primary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("tim_rs_mass_mod_summary")
                    ),
                    plotOutput("tim_rs_mass_annotated")
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("tim_rs_mass_mod_call")
                         ),
                    value_box(
                      title = "R:S Mass Model AIC",
                      value = textOutput("tim_rs_mass_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "R:S Mass Model p-value",
                      value = textOutput("tim_rs_mass_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "R:S Mass Model R^2",
                      value = textOutput("tim_rs_mass_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("tim_rs_mass_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ) # end div
              ) # end RS length stats card
            ), # end ds accordion panel
          ), # end stats accordion
          card(card_header("A Brief Explanation of Statistical Terminology", class = "bg-primary", style = "font-size: 20px"),
            markdown("Many of the people using this app may not be familiar with the statistical terminology used within, so
                      here is a brief overview of the highlighted values above.
                      <br>
                      - **AIC**: The *Akaike information criterion* is an estimator of prediction error for a statistical model, and is most useful
                      for **comparing models**. We generally select the model with the *lowest* AIC value, as it indicates the lowest prediction error.
                      - **p-value**: This is the probability of the observed result occurring by random chance, if there is no effect. (This is a greatly simplified explanation of a highly contentious way of 
                      reporting statistical significance. For a more comprehensive explanation, see the [p-value Wikipedia page](https://en.wikipedia.org/wiki/P-value).)
                      - **R^2**: The coefficient of determination (pronounced 'R squared') is the proportion of variance in the *response* variable
                      that is explained by the *predictor* variables in the model.
                     ")
          )
        ), # end stats tab panel
        tabPanel("Data",
         card(card_header("Fluorescence Data", class = "bg-primary", style = "font-size: 20px"),
              div(dataTableOutput("tim_fluoro_DT")),
              markdown("This dataset is a combination of data from the LI-COR Li-600
           and PhotosynQ MultispeQ V2.0s. For the sake of this app running
           efficiently, the data has been pared down to strictly what is needed.
           The full datasets can be found [on my github](https://www.github.com/zachpeagler/Thesis/data/TIP24).")
         ),
         card(card_header("Height Data", class = "bg-primary", style = "font-size: 20px"),
              dataTableOutput("tim_height_DT")
         ),
         card(card_header("Destructive Sampling Data", class = "bg-primary", style = "font-size: 20px"),
              dataTableOutput("tim_ds_DT")
         )
        ), # end data tab panel
        tabPanel("Info",
          div(style = "padding: 10px", align = "center",
              markdown("#### **Tomato Inoculant Method Trial**")
              ),
          card(card_header("Hypothesis and Objectives", class = "bg-primary", style = "font-size: 20px"),
            markdown("
            This trial accompanies the following hypothesis and objectives laid out in my thesis: 
            <br>
            **Objective 3** – Determine the effect of biostimulant inoculation method 
            (liquid, BG, uninoculated granule) on plant growth and fluorescence parameters. 
            <br>
            - **Hypothesis 3.1** – Because these bacteria have been shown to increase tomato
            health and encapsulation will aid in bacterial delivery, inoculation of tomato plants with 
            BGs will increase plant growth and fluorescence parameters more than liquid inoculation or uninoculated granules.
            <br>
            > It should be noted that these hypotheses are technically *predictive hypotheses*, as not only do they hypothesize a 
            change, they also specify a prediction for that change. I.E. Fluorescence parameters will not only *change*, they will *increase*.
            This is a very minor distinction, but important to those in the science realm (nerds). 
            <br>
            ")
               ), # end hypothesis and objective card
          div(
            layout_column_wrap(
              card(img(src = "tim_1.jpg"),
                   markdown("Tomato plants at six days after germination.")
                   ),
              card(img(src = "tim_2.jpg"),
                   markdown("Tomato plants at thirty-five days after germination.")
                  )
            )
          ),
          card(card_header("Methods", class = "bg-secondary", style = "font-size: 20px"),
            markdown("
            This experiment involved cultivating 144 tomato plants (cultivar BHN 589) for 40 days in a greenhouse.
            There were 4 treatments (control, liquid inoculation, uninoculated chitosan granule, inoculated chitosan granule)
            with 36 replicates per group. The inoculation included the entire microbial consortium listed below at 1x106 CFU/g
            each. Seeds were surface sterilized before planting, and inoculation was performed at the time of germination.
            For liquid inoculants, approximately 10 mL of the liquid microbial consortium was pipetted directly atop the seed.
            For granular treatments, approximately 5 grams of granules were nestled in contact with the seed.
            Inoculants were **not re-applied** throughout the trial. Plants were grown in 6-inch diameter plastic pots with
            Miracle-Gro potting soil. No fertilizer was applied. Plants were top-watered every other day and allowed to 
            grow for 40 days, with regular height measurements taken with a ruler and fluorescence measurements taken 
            with a Li-COR Li-600. At the end of the growing period, plants were destructively sampled for aboveground
            and belowground length and mass, as quantified with a ruler for length and an OHAUS Scout SCA210 for mass. 
            The statistical power for this trial was 0.98 for an effect size of 0.4 with 4 groups and 36 replicates per group at a 0.05 significance level.
            <br>
            ##### **Microbial Consortium** <br>
            - *Azospirillum brasilense Sp7*- PGPB that benefits the plant via nitrogen fixation, siderophore production,
            and by increasing lateral root growth ([Sahoo et al, 2014](https://pubmed.ncbi.nlm.nih.gov/24414168/); [Li et al, 2005](https://pubmed.ncbi.nlm.nih.gov/16121231/)),
            and has been shown to increase plant stress tolerance ([Casanovas et al, 2002](https://www.jstor.org/stable/23787082?seq=1)).
            It has been shown to increase crop yield and plant nitrogen, phosphorous, and potassium content
            ([Askary et al, 2009](https://www.researchgate.net/publication/347438334_Influence_of_the_Co-inoculation_Azospirillum_brasilense_and_Rhizobium_meliloti_plus_24-D_on_Grain_Yield_and_N_P_K_Content_of_Triticum_aestivum_Cv_Baccros_and_Mahdavi)).
            It has also been reported to work well with Methylobacterium oryzae ([Madhaiyan et al, 2009](https://www.researchgate.net/publication/225966871_Effect_of_co-inoculation_of_methylotrophic_Methylobacterium_oryzae_with_Azospirillum_brasilense_and_Burkholderia_pyrrocinia_on_the_growth_and_nutrient_uptake_of_tomato_red_pepper_and_rice)). <br>
            - *Azotobacter chroococcum 43* - PGPB that operates via nitrogen fixation, phosphate solubilization,
            and vitamin, indole acetic acid (IAA), gibberellin (GA), hydrogen cyanide (HCN), siderophore,
            and cytokinin (CK) production ([Abd El-Fattah et al, 2013](https://www.researchgate.net/publication/259130343_Effect_of_carrier_materials_sterilization_method_and_storage_temperature_on_survival_and_biological_activities_of_Azotobacter_chroococcum_inoculant); [Revillas et al, 2000](https://pubmed.ncbi.nlm.nih.gov/11021581/); [Wani et al, 2007](https://www.researchgate.net/publication/240762870_Co-inoculation_of_nitrogen-fixing_and_phosphate-solubilizing_bacteria_to_promote_growth_yield_and_nutrient_uptake_in_chickpea)).
            Shown to increase germination rates and aboveground biomass and crop quality and yield in maize ([Zahir et al, 2005](https://www.researchgate.net/publication/233130106_Precursor_L-tryptophan-Inoculum_Azotobacter_Interaction_for_Improving_Yields_and_Nitrogen_Uptake_of_Maize)). <br>
            - *Bacillus subtilis* - PGPB that has been shown to improve fruit quality and yield in tomato
            ([Mena-Violante & Olalde-Portugal, 2007](https://www.researchgate.net/publication/222326135_Alteration_of_tomato_fruit_quality_by_root_inoculation_with_plant_growth-promoting_rhizobacteria_PGPR_Bacillus_subtilis_BEB-13bs); [Kokalis-Burelle et al, 2002](https://link.springer.com/article/10.1023/A:1014464716261))
            and shown to increase metabolite production ([Sharaf-Eldin et al, 2008](https://pubmed.ncbi.nlm.nih.gov/18622904/)).
            Shown to solubilize phosphate, fix nitrogen, produce IAA, CK, GA, HCN, and antibiotics,
            as well as exhibiting phytase activity ([Ahmad et al, 2008](https://pubmed.ncbi.nlm.nih.gov/16735107/); [Arkhipova et al, 2005](https://link.springer.com/article/10.1007/s11104-004-5047-x); [Yao et al, 2006](https://www.researchgate.net/publication/233193377_Effect_of_FZB_24_Bacillus_subtilis_as_biofertilizer_on_cotton_yields_in_field_tests)).
            It has been used as a biocontrol agent against aphids and pathogenic bacteria ([Kokalis-Burelle et al, 2002](https://link.springer.com/article/10.1023/A:1014464716261)). <br>
            - *Methylobacterium oryzae CBMB20* - PGPB that has been shown to improve fruit quality and yield in tomato
            in both foliar and chitosan encapsulated inoculations ([Chanratana et al., 2019](https://www.researchgate.net/profile/Aritra-Choudhury/publication/323564168_Evaluation_of_chitosan_and_alginate_immobilized_Methylobacterium_oryzae_CBMB20_on_tomato_plant_growth/links/5a9e6fcfa6fdcc214af2b315/Evaluation-of-chitosan-and-alginate-immobilized-Methylobacterium-oryzae-CBMB20-on-tomato-plant-growth.pdf)). 
            Operates through phytohormone (auxin and cytokinin) production, stress reduction via ACC deaminase production, 
            increased nutrient availability through nitrogen fixation, and as a biopesticide ([Chauhan et al., 2015](https://www.sciencedirect.com/science/article/abs/pii/S0929139315300159)). <br>
            - *Pseudomonas putida 90* - PGPB that increases plant growth by solubilizing phosphate and producing
            IAA and siderophores ([Hariprasad & Niranjana, 2009](https://link.springer.com/article/10.1007/s11104-008-9754-6)). Shown to inhibit ethylene production ([Mayak et al, 1999](https://pubmed.ncbi.nlm.nih.gov/10552131/)).
            Shown to significantly increase tomato fruit macro- and micronutrient content ([He et al, 2019](https://pubmed.ncbi.nlm.nih.gov/30955229/)). 
            Shown to increase potassium, magnesium, and calcium uptake and decrease sodium uptake ([Yao et al, 2010](https://www.sciencedirect.com/science/article/abs/pii/S1164556309001046)). 
            Also shown to increase root and shoot growth ([Glick et al, 1997](https://www.researchgate.net/publication/223543401_Early_development_of_canola_seedlings_in_the_presence_of_the_plant_growth-promoting_rhizobacterium_Pseudomonas_putida_GR12-2); [Hall et al, 1996](https://ui.adsabs.harvard.edu/abs/1996IsJPS..44...37H/abstract)). <br>
            ")
          ), # end methods card
          card(card_header("Variables", class = "bg-primary", style = "font-size: 20px"),
            markdown("
            ##### **Explanatory Variables**
            - **Treatment** is the inoculation timing of the tomato. Options are Control, Uninoculated BG, Liquid Inoculant, and Inoculated BG. <br>
            - **Time** is the time at which the measurement was taken. <br>
            - **Date** is the date at which the measurement was taken. <br>
            - **DaysFromGermination** is the number of days from germination (2025-05-01) to the date of measurement. <br>
            - **Row** is the row of the tomato. (A:H) <br>
            - **Pot** is the pot number of the tomato. (1:13) <br>
            - **Plant** is a combination of *Row* and *Pot*, and acts as an ID for every individual plant. (A1: D12) <br>
            ###### **Fluorescence**
            - **AmbientHumidity** is the relative humidity (%) at the time of measurement. <br>
            - **AmbientLight** is the ambient light level (lumens) at the time of measurement. <br>
            - **AmbientPressure** is the ambient pressure (kPa) at the time of measurement. <br>
            - **LeafTemperature** is the temperature (Celcius) of the leaf. <br>
            ---
            ##### **Response Variables**
            ###### **Fluorescence**
            - **gsw** is the stomatal conductance (mol m-2 s-1) of the leaf. Stomatal conductance refers to the
            rate at which molecules are moving through the leaf's stomates, and is indicitave of photosynthesis.<br>
            - **PhiPS2** is the efficiency of Photosystem II. It is unitless. (0:1) <br>
            ###### **Height**
            - **Height** is the height in centimeters of the tomato plants. <br>
            ###### **Destructive Sampling**
            - **Aboveground Length** is the length in centimeters of the aboveground portion of the tomato plant after 40 days of growth.
            - **Aboveground Mass** is the mass in grams of the aboveground portion of the tomato plant after 40 days of growth.
            - **Belowground Length** is the length in centimeters of the belowground portion of the tomato plant (the roots) after 40 days of growth.
            - **Belowground Mass** is the mass in grams of the aboveground portion of the tomato plant (the roots) after 40 days of growth.
            "),
            div(style="border-left: 5px solid", 
              markdown(
                "> For a more comprehensive explanation of PhiPS2, check out 
                [Genty *et al*., 1989](https://www.sciencedirect.com/science/article/abs/pii/S0304416589800169) or
                for a simpler explanation, the [chlorophyll fluorescence wikipedia page](https://en.wikipedia.org/wiki/Chlorophyll_fluorescence)."
              )
            )
          ) # end variable card
          ) # end info tab Panel
        ) # end ILT tabsetpanel
      ),
      ##### TOMATO INOCULANT LOCATION #####
      tabPanel("Inoculant Location Trial",
        tabsetPanel( # interior ILT tabsetpanel
          tabPanel("Exploratory",
            div(style = "padding: 10px",
            markdown("> Quick tip: click/tap the **gear** icon in the top right corner to change the color palette of all
                     the graphs across the entire app. Among some personal favorites of mine are bilbao, lipari, lajolla, and oslo.")
            ),
          accordion(
            accordion_panel(title = "Density and Distribution",
              markdown("
                A critical component of an exploratory data analysis is the creation of **probability density function** (PDF) plots
                and **cumulative distribution function** (CDF) plots. They tell us the shape the data takes and helps inform if we need to 
                apply a mathematical correction or use a certain type of distribution in our statistical model. I'm only
                making dedicated PDF and CDF plots for our response variables and not our explanatory variables. However,
                we can see the shape of our explanatory variables using histograms (a couple accordion panels down).
                "),
              card(card_header("Fluorescence", class = "bg-primary", style = "font-size: 20px"),
                layout_sidebar(sidebar=sidebar(
                  selectInput("til_fluoro_dist_var", "Variable", choices = til_fluoro_vars,
                               selected = "gsw"),
                  checkboxGroupInput("til_fluoro_dists", "Distributions", choices=dists, 
                                      selected=c("normal", "lognormal", "gamma")),
                  sliderInput("til_fluoro_len", "Length to Test Distributions Over", min=1,
                              max=500, value=100)
                  ), # end sidebar - but not sidebar LAYOUT
                  div(
                    layout_column_wrap(
                      plotOutput("til_fluoro_pdf"),
                      plotOutput("til_fluoro_cdf")
                    )
                  ),
                  div(
                    markdown("###### **One-sample Kolmogorov-Smirnov tests for selected fluorescence variable against selected distributions**"),
                    verbatimTextOutput("til_fluoro_KS")
                  ),
                  div(style="border-left: 5px solid", 
                    markdown("
                      > A note on PhiPS2 distributions: PhiPS2 is a **unitless ratio** on a scale of 0-1, so we don't need to create PDF and CDF plots and perform KS tests
                      (you still have the option to, but this is an instance where we use our *statistical reasoning*).
                      Instead, we logit transform PhiPS2 and use the logit transformed version in our models. <br>
                      > For a more comprehensive explanation of PhiPS2, check out 
                      [Genty *et al*., 1989](https://www.sciencedirect.com/science/article/abs/pii/S0304416589800169) or
                      for a simpler explanation, the [chlorophyll fluorescence wikipedia page](https://en.wikipedia.org/wiki/Chlorophyll_fluorescence)."
                  ))
                ) # end sidebar layout
              ), # end fluorescence card
              card(card_header("Fruit", class = "bg-primary", style = "font-size: 20px"),
                 layout_sidebar(sidebar=sidebar(
                    selectInput("til_fruit_dist_var", "Variable", choices = til_fruit_vars,
                                selected = "Mass"),
                    checkboxGroupInput("til_fruit_dists", "Distributions", choices=dists, 
                                       selected=c("normal", "lognormal", "gamma")),
                    sliderInput("til_fruit_len", "Length to Test Distributions Over", min=1,
                                max=500, value=100)
                  ), # end sidebar - but not sidebar LAYOUT
                  div(
                   layout_column_wrap(
                     plotOutput("til_fruit_pdf"),
                     plotOutput("til_fruit_cdf")
                   )
                  ),
                  div(
                   markdown("###### **One-sample Kolmogorov-Smirnov tests for selected fruit variable against selected distributions**"),
                   verbatimTextOutput("til_fruit_KS")
                  ),
                  div(style="border-left: 5px solid", 
                      markdown("
                        > Sugar and blossom end-rot are both ratios (similar to PhiPS2), so we'll 
                        end up logit transforming them for use in our models.
                      ")
                  )
                ) # end sidebar layout
              ) # end phips2 card
            ), # end dist accordion panel
            accordion_panel(title="Histograms",
              card(card_header("Fluorescence Histogram", class = "bg-primary", style = "font-size: 20px"),
                layout_sidebar(sidebar=sidebar(
                  selectInput("til_fluoro_hist_var", "Select X Variable",
                              choices = til_fluoro_vars, selected = "AmbientHumidity"),
                  selectInput("til_fluoro_hist_color", "Select Color Variable",
                              choices = til_fluoro_vars_d, selected = "Treatment"),
                  sliderInput("til_fluoro_hist_bins", "Number of Bins",
                              value = 30, min = 2, max = 100)
                  ), # end sidebar
                  plotOutput("til_fluoro_hist")
              )), # gsw hist card
              card(card_header("Fruit Histogram", class = "bg-primary", style = "font-size: 20px"),
                layout_sidebar(sidebar=sidebar(
                  selectInput("til_fruit_hist_var", "Select Variable",
                              choices = til_fruit_vars, selected = "Ripeness"),
                  selectInput("til_fruit_hist_color", "Select Color Variable",
                              choices = til_fruit_vars_d, selected = "Treatment"),
                  sliderInput("til_fruit_hist_bins", "Number of Bins",
                              value = 30, min = 2, max = 100)
                  ), # end sidebar
                  plotOutput("til_fruit_hist")
                )) # ps2 hist card
              ), # end hist accordion panel
              accordion_panel(title = "Scatter Plots",
                card(card_header("Fluorescence Scatter", class = "bg-primary", style = "font-size: 20px"),
                    layout_sidebar(sidebar = sidebar(
                      selectInput("til_fluoro_scatter_x","X Variable",
                                  choices = all_til_fluoro_vars, selected = "AmbientHumidity"),
                      selectInput("til_fluoro_scatter_y","Y Variable",
                                  choices = all_til_fluoro_vars, selected = "gsw"),
                      selectInput("til_fluoro_scatter_col","Color Variable",
                                  choices = all_til_fluoro_vars, selected = "Treatment"),
                      selectInput("til_fluoro_scatter_shape", "Shape Variable",
                                  choices = til_fluoro_vars_d, selected = "Treatment"),
                      sliderInput("til_fluoro_scatter_jit", "Jitter Amount",
                                  min=0, max=10, value =3),
                      sliderInput("til_fluoro_scatter_size", "Point Size",
                                  min = 1, max=10, value = 3),
                      checkboxInput("til_fluoro_scatter_fwrap", "Individual Plot Per Treatment", FALSE)
                    ), # end sidebar
                    card_body(plotOutput("til_fluoro_scatter"))
                    ) # end sidebar layout
                ), # end gsw scatter plot
                card(card_header("Fruit Scatter", class = "bg-primary", style = "font-size: 20px"),
                    layout_sidebar(sidebar = sidebar(
                      selectInput("til_fruit_scatter_x","X Variable",
                                  choices = all_til_fruit_vars, selected = "Mass"),
                      selectInput("til_fruit_scatter_y","Y Variable",
                                  choices = all_til_fruit_vars, selected = "pSugar"),
                      selectInput("til_fruit_scatter_col","Color Variable",
                                  choices = all_til_fruit_vars, selected = "Treatment"),
                      selectInput("til_fruit_scatter_shape", "Shape Variable",
                                  choices = til_fruit_vars_d, selected = "Treatment"),
                      sliderInput("til_fruit_scatter_jit", "Jitter Amount",
                                  min=0, max=10, value =0),
                      sliderInput("til_fruit_scatter_size", "Point Size",
                                  min = 1, max=10, value = 3),
                      checkboxInput("til_fruit_scatter_fwrap", "Individual Plot Per Treatment", FALSE)
                    ), # end sidebar
                    card_body(plotOutput("til_fruit_scatter"))
                    ) # end sidebar layout
                ) # end fruit scatter plot
            ), # end scatterplot accordion panel
            accordion_panel(title="Box Plots",
              card(card_header("Fluorescence Boxplot", class = "bg-primary", style = "font-size: 20px"),
                   layout_sidebar(sidebar = sidebar(
                     selectInput("til_fluoro_box_x","X Variable",
                                 choices = til_fluoro_vars_d, selected = "Treatment"),
                     selectInput("til_fluoro_box_y","Y Variable",
                                 choices = til_fluoro_vars, selected = "gsw")
                   ), # end sidebar
                   plotOutput("til_fluoro_box")
                   )),
              card(card_header("Fruit Boxplot", class = "bg-primary", style = "font-size: 20px"),
                   layout_sidebar(sidebar = sidebar(
                     selectInput("til_fruit_box_x","X Variable",
                                 choices = til_fruit_vars_d, selected = "Treatment"),
                     selectInput("til_fruit_box_y","Y Variable",
                                 choices = til_fruit_vars, selected = "Mass")
                   ), # end sidebar
                   plotOutput("til_fruit_box")
                   )),
              card(card_header("Fruit Summary Boxplot", class = "bg-primary", style = "font-size: 20px"),
                   layout_sidebar(sidebar = sidebar(
                     selectInput("til_fruit_sum_box_x","X Variable",
                                 choices = til_fruit_sum_vars_d, selected = "Treatment"),
                     selectInput("til_fruit_sum_box_y","Y Variable",
                                 choices = til_fruit_sum_vars, selected = "Mass_sum")
                   ), # end sidebar
                   plotOutput("til_fruit_sum_box")
                   ))
            ) # end boxplot accordion panel
          ) # end accordion
          ), # end exploratory tab panel
        tabPanel("Statistics",
          accordion(
            accordion_panel("Fluorescence",
              card(card_header("Stomatal conductance (gsw)", class = "bg-primary", style = "font-size: 20px"),
                selectInput("til_gsw_mod_var", "Predictor Variable",
                            choices = fluoro_mod_var_names, selected = "AmbientHumidity"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("til_gsw_mod_summary")
                    )
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("til_gsw_mod_call")
                         ),
                    value_box(
                      title = "GSW Model AIC",
                      value = textOutput("til_gsw_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "GSW Model p-value",
                      value = textOutput("til_gsw_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "GSW Model R^2",
                      value = textOutput("til_gsw_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("til_gsw_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ), # end div
              ), # end gsw stats card
              card(card_header("Photosystem II Efficiency (PhiPS2", class = "bg-primary", style = "font-size: 20px"),
                   selectInput("til_ps2_mod_var", "Predictor Variable",
                               choices = fluoro_mod_var_names, selected = "AmbientHumidity"),
                   div(layout_columns(col_widths = c(7,5),
                        div(
                          card(card_header("Model Summary"),
                               verbatimTextOutput("til_ps2_mod_summary")
                          )
                        ),# end model summary div
                        div(# value boxes for AIC and r^2
                          card(card_header("Model Call", class = "bg-primary"),
                               verbatimTextOutput("til_ps2_mod_call")
                          ),
                          value_box(
                            title = "PhiPS2 Model AIC",
                            value = textOutput("til_ps2_aic"),
                            theme = "bg-primary",
                            width = 0.2
                          ),
                          value_box(
                            title = "PhiPS2 Model p-value",
                            value = textOutput("til_ps2_p"),
                            width = 0.2
                          ),
                          value_box(
                            title = "PhiPS2 Model R^2",
                            value = textOutput("til_ps2_r2"),
                            theme = "bg-secondary",
                            width = 0.2
                          ),
                          card(card_header("Treatment Letters", class="bg-secondary"),
                               verbatimTextOutput("til_ps2_letters")
                          )
                        ) # end value box div
                   ) # end column wrap
                ) # end div
              ), # end ps2 stats card
              card(card_header("Multivariate", class = "bg-secondary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(6,6),
                  div(
                    markdown("
                      Maybe instead of looking at a single environmental variable, we want to look at all of them (or at least the ones
                      that actually make an impact on our target response variables). To accomplish this, we can use **principal component analysis** (PCA).
                      First, we scale our environmental variables so they all have an equal influence on the output, then use the **rda** function from the **vegan** package
                      to reduce the dimensionality."
                    ),
                    card(card_header("PCA Summary"), 
                       verbatimTextOutput("til_fluoro_pca_summary")
                    )
                  ),
                  plotOutput("til_fluoro_pca")
                )),
                div(layout_column_wrap(
                  card(card_header("Multivariate Stomatal Conductance (GSW)", class = "bg-primary", style = "font-size: 20px"),
                       div(# value boxes for AIC and r^2
                         card(card_header("Model Summary", class = "bg-primary"),
                              verbatimTextOutput("til_pcr_gsw_summary"),
                              max_height = 500
                         ),
                         value_box(
                           title = "Multivariate GSW AIC",
                           value = textOutput("til_pcr_gsw_aic"),
                           theme = "bg-primary",
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate GSW p-value",
                           value = textOutput("til_pcr_gsw_p"),
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate GSW R^2",
                           value = textOutput("til_pcr_gsw_r2"),
                           theme = "bg-secondary",
                           width = 0.2
                         ),
                         card(card_header("Treatment Letters", class="bg-secondary"),
                              verbatimTextOutput("til_pcr_gsw_letters")
                         )
                       ), # end value box div
                       plotOutput("til_pcr_gsw_pred")
                       ),
                  card(card_header("Multivariate Photosystem II Efficiency", class = "bg-primary", style = "font-size: 20px"),
                       div(# value boxes for AIC and r^2
                         card(card_header("Model Summary", class = "bg-primary"),
                              verbatimTextOutput("til_pcr_ps2_summary"),
                              max_height = 500
                         ),
                         value_box(
                           title = "Multivariate PhiPS2 AIC",
                           value = textOutput("til_pcr_ps2_aic"),
                           theme = "bg-primary",
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate PhiPS2 p-value",
                           value = textOutput("til_pcr_ps2_p"),
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate PhiPS2 R^2",
                           value = textOutput("til_pcr_ps2_r2"),
                           theme = "bg-secondary",
                           width = 0.2
                         ),
                         card(card_header("Treatment Letters", class="bg-secondary"),
                              verbatimTextOutput("til_pcr_ps2_letters")
                         )
                       ), # end value box div
                       plotOutput("til_pcr_ps2_pred")
                  ), # end phips2 card
                )) # end column wrap and div
              ) # end multivariate card
            ),
            accordion_panel("Fruit",
              div(
                markdown("
                > Because our fruit are **pseudoreplicates** (we applied our treatments to the plants, not the individual fruit)
                when we go to make fruit models, we have to average out the values between the plants, rather than looking at the individual fruit
                as a sampling unit. If we didn't do this, our models would have a drastically increased sample size that wouldn't reflect
                our actual experimental design.
                ")
              ),
              card(card_header("Mass", class = "bg-primary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("til_mass_mod_summary")
                    ),
                    plotOutput("til_mass_annotated")
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("til_mass_mod_call")
                         ),
                    value_box(
                      title = "Mass Model AIC",
                      value = textOutput("til_mass_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "Mass Model p-value",
                      value = textOutput("til_mass_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "Mass Model R^2",
                      value = textOutput("til_mass_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("til_mass_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ) # end div
              ), # end mass stats card
              card(card_header("Sugar", class = "bg-primary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("til_sug_mod_summary")
                    ),
                    plotOutput("til_sug_annotated")
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("til_sug_mod_call")
                         ),
                    value_box(
                      title = "Sugar Model AIC",
                      value = textOutput("til_sug_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "Sugar Model p-value",
                      value = textOutput("til_sug_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "Sugar Model R^2",
                      value = textOutput("til_sug_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("til_sug_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ) # end div
              ), # end sugar stats card
              card(card_header("Blossom End-Rot", class = "bg-primary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("til_ber_mod_summary")
                    ),
                    plotOutput("til_ber_annotated")
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("til_ber_mod_call")
                         ),
                    value_box(
                      title = "BER Model AIC",
                      value = textOutput("til_ber_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "BER Model p-value",
                      value = textOutput("til_ber_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "BER Model R^2",
                      value = textOutput("til_ber_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("til_ber_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ) # end div
              ), # end ber stats card
              card(card_header("Fruit Count", class = "bg-primary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("til_fc_mod_summary")
                    ),
                    plotOutput("til_fc_annotated")
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("til_fc_mod_call")
                         ),
                    value_box(
                      title = "Fruit Count Model AIC",
                      value = textOutput("til_fc_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    card(card_header("Fruit Count Model R^2", class = "bg-secondary"),
                         verbatimTextOutput("til_fc_r2")
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("til_fc_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ) # end div
              ) # end fruit count stats card
            ) # end fruit accordion panel
          ) # end stats accordion
        ), # end stats tab panel
        tabPanel("Data",
         card(card_header("Fluorescence Data", class = "bg-primary", style = "font-size: 20px"),
              div(dataTableOutput("til_fluoro_DT")),
              markdown("This dataset is a combination of data from the LI-COR Li-600
           and PhotosynQ MultispeQ V2.0s. For the sake of this app running
           efficiently, the data has been pared down to strictly what is needed.
           The full datasets can be found [on my github](https://www.github.com/zachpeagler/Thesis/data/TIP24).")
         ),
         card(card_header("Fruit Data", class = "bg-primary", style = "font-size: 20px"),
              dataTableOutput("til_fruit_DT")
         ),
         card(card_header("Fruit Summary Data", class = "bg-primary", style = "font-size: 20px"),
              dataTableOutput("til_fruit_sum_DT")
         )
        ), # end data tab panel
        tabPanel("Info",
          div(style = "padding: 10px", align = "center",
              markdown("#### **Tomato Inoculant Location Trial**")
              ),
          card(card_header("Hypothesis and Objectives", class = "bg-primary", style = "font-size: 20px"),
               markdown("
            This trial accompanies the following hypothesis and objectives laid out in my thesis: <br>
            **Objective 1.** Determine the effect of M. oryzae inoculation method on tomato plant growth and crop quality and yield. <br>
            - **Hypothesis 1.1** – Because of the mass effect, combined soil and foliar M. oryzae application will 
            increase salt stressed tomato plant fluorescence parameters, fruit yield, and fruit quality more than either soil or foliar inoculations.
            <br>
            > It should be noted that these hypotheses are technically *predictive hypotheses*, as not only do they hypothesize a 
            change, they also specify a prediction for that change. I.E. Fluorescence parameters will not only *change*, they will *increase*.
            This is a very minor distinction, but important to those in the science realm (nerds). <br>
            ")
          ), # end hypothesis and objective card
          card(card_header("Methods", class = "bg-secondary", style = "font-size: 20px"),
               layout_column_wrap(
               markdown("
            This trial took place in the hydroponic greenhouse at the KSU field station (34.0622° N, 84.6034° W).
            Tomato seeds were germinated in Oasis cubes and fertilized for three weeks before being transplanted
            into Dutch buckets containing Mother Earth™ Coco + Perlite Mix, copper-lined root barriers, and
            Mother Earth™ Hydroton Original clay pellets. The sample size (n) was 32 tomato plants across four rows (A to D)
            with eight plants per row. Row A was the control, with no treatment applied. Row B was subjected to soil 
            inoculation with BGs at transplantation by placing a few BGs (~5 g) beneath the roots when transplanting. 
            Row C was subjected to foliar inoculation with a spray outside the greenhouse prior to transplantation. 
            Row D was subjected to both soil and foliar treatments as described. The unit of replication for bacterial
            treatments are the tomato plants. The unit of replication for sugar and weight measurements are individual 
            tomatoes. However, because the individual tomatoes are pseudoreplicates (we aren’t applying the treatment 
            to the individual tomatoes, but rather the plant that those fruit comes from) they were summarized by plant prior to analysis.
            <br>
            Plants were inoculated at the time of transplantation. A nutrient mix composed of 1.32 g/L Jack’s 0-12-26,
            0.075 g/L magnesium sulfate, and 0.912 g/L calcium nitrate was mixed in 1000-liter batches and fed into 
            the Dutch buckets. To salt stress to the plants, sodium chloride was added in 50g batches until the nutrient 
            solution reached an electrical conductivity of 3.5 mS/cm. 
            <br>
            Fluorescence measurements were taken biweekly with a LI-COR LI-600 and two PhotosynQ MultispeQ V2.0s over the course of the trial.
            Fruit were harvested upon ripening, as determined by color and firmness. Upon harvesting, fruit were taken back to the lab for analysis, 
            where the mass (grams), penetrometer (kg), and sugar (%) were measured. Fruit were also assessed for blossom end-rot.
            The data table is formatted in a tidy format with each row corresponding to one fruit and each column representing a variable.<br>
            
            ##### **Microbe** <br>
            - *Methylobacterium oryzae CBMB20* - PGPB that has been shown to improve fruit quality and yield in tomato
            in both foliar and chitosan encapsulated inoculations ([Chanratana et al., 2019](https://www.researchgate.net/profile/Aritra-Choudhury/publication/323564168_Evaluation_of_chitosan_and_alginate_immobilized_Methylobacterium_oryzae_CBMB20_on_tomato_plant_growth/links/5a9e6fcfa6fdcc214af2b315/Evaluation-of-chitosan-and-alginate-immobilized-Methylobacterium-oryzae-CBMB20-on-tomato-plant-growth.pdf)). 
            Operates through phytohormone (auxin and cytokinin) production, stress reduction via ACC deaminase production, 
            increased nutrient availability through nitrogen fixation, and as a biopesticide ([Chauhan et al., 2015](https://www.sciencedirect.com/science/article/abs/pii/S0929139315300159)). <br>
            "),
               div(
                   card(img(src = "til_1.jpg"),
                       markdown("Tomato plants three weeks after germination")
                   ),
                   card(img(src = "til_2.jpg"),
                       markdown("Tomato plants three months after germination.")
                   )
               ), # end tomato growth pictures div
            ) # end layout column wrap
          ), # end methods card
          
          card(card_header("Variables", class = "bg-primary", style = "font-size: 20px"),
            markdown("
            ##### **Explanatory Variables**
            - **Treatment** is the inoculation timing of the tomato. Options are Control, Germination, Transplantation, and Germ+Trans. <br>
            - **Row** is the row of the tomato. (A:D) <br>
            - **Pot** is the pot number of the tomato. (1:12) <br>
            - **Plant** is a combination of *Row* and *Pot*, and acts as an ID for every individual plant. (A1: D12) <br>
            - **Date** is the date at which the measurement was taken. <br>
            ###### **Fluorescence**
            - **Time** is the time at which the measurement was taken. <br>
            - **AmbientHumidity** is the relative humidity (%) at the time of measurement. <br>
            - **AmbientLight** is the ambient light level (lumens) at the time of measurement. <br>
            - **AmbientPressure** is the ambient pressure (kPa) at the time of measurement. <br>
            - **LeafTemperature** is the temperature (Celcius) of the leaf. <br>
            ###### **Fruit**
            - **Penetrometer** corresponds to the force in kilograms it takes to penetrate the flesh of the tomato (~0.5:~4) <br>
            - **Ripeness** is the **Penetrometer** value mapped from 0:1 and reversed, so that riper fruit are closer to 1 and unripe fruit are closer to 0. (0:1) <br>
            ---
            ##### **Response Variables**
            ###### **Fluorescence**
            - **gsw** is the stomatal conductance (mol m-2 s-1) of the leaf. Stomatal conductance refers to the
            rate at which molecules are moving through the leaf's stomates, and is indicitave of photosynthesis.<br>
            - **PhiPS2** is the efficiency of Photosystem II. It is unitless. (0:1) <br>
            ###### **Fruit**
            - **Mass** is the mass of the tomato (grams), measured on an Ohaus Scout. (~10:~400) <br>
            - **BER** corresponds to whether or not the tomato has blossom end rot, a disease caused by calcium deficiency that renders the fruit unmarketable. (0,1) <br>
            - **pSugar** is the average of two measurements of the tomato juice's sugar concentration taken on a Fisher BRIX Refractometer (~2:~12) <br>
            - **SugarGrams** is the grams of sugar in each tomato, calculated as **pSugar** x **Mass** <br>
            
            > It's important to note that **only** the Li-600 can measure gsw, while both
            the Li-600 and the MultispeQ can measure PhiPS2. Also, even though both devices can 
            measure PhiPS2, they do so **in different ways**. For our purposes, this is fine
            so long as the measurements from each device correlate. <br>
            "),
            div(style="border-left: 5px solid", 
              markdown(
                "> For a more comprehensive explanation of PhiPS2, check out 
                [Genty *et al*., 1989](https://www.sciencedirect.com/science/article/abs/pii/S0304416589800169) or
                for a simpler explanation, the [chlorophyll fluorescence wikipedia page](https://en.wikipedia.org/wiki/Chlorophyll_fluorescence)."
              )
            )
          ) # end variable card
          ) # end info tab Panel
        ) # end ILT tabsetpanel
      ),
      ##### TOMATO INOCULANT TIMING #####
      tabPanel("Inoculant Timing Trial",
      tabsetPanel(
        tabPanel("Exploratory",
          div(style = "padding: 10px",
            markdown("> Quick tip: this tab uses **accordions**! Click or tap the accordion panel title to expand/shrink the panel
                   and switch between different exploratory graph types.")
          ),
          accordion(
            accordion_panel(title = "Density and Distribution",
              markdown("
                A critical component of an exploratory data analysis is the creation of **probability density function** (PDF) plots
                and **cumulative distribution function** (CDF) plots. They tell us the shape the data takes and helps inform if we need to 
                apply a mathematical correction or use a certain type of distribution in our statistical model. I'm only
                making dedicated PDF and CDF plots for our response variables and not our explanatory variables. However,
                we can see the shape of our explanatory variables using histograms (a couple accordion panels down).
                "),
              card(card_header("Fluorescence", class = "bg-primary", style = "font-size: 20px"),
                layout_sidebar(sidebar=sidebar(
                  selectInput("tit_fluoro_dist_var", "Variable", choices = tit_fluoro_vars,
                               selected = "gsw"),
                  checkboxGroupInput("tit_fluoro_dists", "Distributions", choices=dists, 
                                      selected=c("normal", "lognormal", "gamma")),
                  sliderInput("tit_fluoro_len", "Length to Test Distributions Over", min=1,
                              max=500, value=100)
                  ), # end sidebar - but not sidebar LAYOUT
                  div(
                    layout_column_wrap(
                      plotOutput("tit_fluoro_pdf"),
                      plotOutput("tit_fluoro_cdf")
                    )
                  ),
                  div(
                    markdown("###### **One-sample Kolmogorov-Smirnov tests for selected fluorescence variable against selected distributions**"),
                    verbatimTextOutput("tit_fluoro_KS")
                  ),
                  div(style="border-left: 5px solid", 
                    markdown("
                      > A note on PhiPS2 distributions: PhiPS2 is a **unitless ratio** on a scale of 0-1, so we don't need to create PDF and CDF plots and perform KS tests
                      (you still have the option to, but this is an instance where we use our *statistical reasoning*).
                      Instead, we logit transform PhiPS2 and use the logit transformed version in our models. <br>
                      > For a more comprehensive explanation of PhiPS2, check out 
                      [Genty *et al*., 1989](https://www.sciencedirect.com/science/article/abs/pii/S0304416589800169) or
                      for a simpler explanation, the [chlorophyll fluorescence wikipedia page](https://en.wikipedia.org/wiki/Chlorophyll_fluorescence)."
                  ))
                ) # end sidebar layout
              ), # end fluorescence card
              card(card_header("Fruit", class = "bg-primary", style = "font-size: 20px"),
                 layout_sidebar(sidebar=sidebar(
                    selectInput("tit_fruit_dist_var", "Variable", choices = tit_fruit_vars,
                                selected = "Mass"),
                    checkboxGroupInput("tit_fruit_dists", "Distributions", choices=dists, 
                                       selected=c("normal", "lognormal", "gamma")),
                    sliderInput("tit_fruit_len", "Length to Test Distributions Over", min=1,
                                max=500, value=100)
                  ), # end sidebar - but not sidebar LAYOUT
                  div(
                   layout_column_wrap(
                     plotOutput("tit_fruit_pdf"),
                     plotOutput("tit_fruit_cdf")
                   )
                  ),
                  div(
                   markdown("###### **One-sample Kolmogorov-Smirnov tests for selected fruit variable against selected distributions**"),
                   verbatimTextOutput("tit_fruit_KS")
                  ),
                  div(style="border-left: 5px solid", 
                      markdown("
                        > Sugar and blossom end-rot are both ratios (similar to PhiPS2), so we'll 
                        end up logit transforming them for use in our models.
                      ")
                  )
                ) # end sidebar layout
              ) # end phips2 card
            ), # end dist accordion panel
            accordion_panel(title="Histograms",
              card(card_header("Fluorescence Histogram", class = "bg-primary", style = "font-size: 20px"),
                layout_sidebar(sidebar=sidebar(
                  selectInput("tit_fluoro_hist_var", "Select X Variable",
                              choices = tit_fluoro_vars, selected = "AmbientHumidity"),
                  selectInput("tit_fluoro_hist_color", "Select Color Variable",
                              choices = tit_fluoro_vars_d, selected = "Treatment"),
                  sliderInput("tit_fluoro_hist_bins", "Number of Bins",
                              value = 30, min = 2, max = 100)
                  ), # end sidebar
                  plotOutput("tit_fluoro_hist")
              )), # gsw hist card
              card(card_header("Fruit Histogram", class = "bg-primary", style = "font-size: 20px"),
                layout_sidebar(sidebar=sidebar(
                  selectInput("tit_fruit_hist_var", "Select Variable",
                              choices = tit_fruit_vars, selected = "Ripeness"),
                  selectInput("tit_fruit_hist_color", "Select Color Variable",
                              choices = tit_fruit_vars_d, selected = "Treatment"),
                  sliderInput("tit_fruit_hist_bins", "Number of Bins",
                              value = 30, min = 2, max = 100)
                  ), # end sidebar
                  plotOutput("tit_fruit_hist")
                )) # ps2 hist card
              ), # end hist accordion panel
              accordion_panel(title = "Scatter Plots",
                card(card_header("Fluorescence Scatter", class = "bg-primary", style = "font-size: 20px"),
                    layout_sidebar(sidebar = sidebar(
                      selectInput("tit_fluoro_scatter_x","X Variable",
                                  choices = all_tit_fluoro_vars, selected = "AmbientHumidity"),
                      selectInput("tit_fluoro_scatter_y","Y Variable",
                                  choices = all_tit_fluoro_vars, selected = "gsw"),
                      selectInput("tit_fluoro_scatter_col","Color Variable",
                                  choices = all_tit_fluoro_vars, selected = "Treatment"),
                      selectInput("tit_fluoro_scatter_shape", "Shape Variable",
                                  choices = tit_fluoro_vars_d, selected = "Treatment"),
                      sliderInput("tit_fluoro_scatter_jit", "Jitter Amount",
                                  min=0, max=10, value =3),
                      sliderInput("tit_fluoro_scatter_size", "Point Size",
                                  min = 1, max=10, value = 3),
                      checkboxInput("tit_fluoro_scatter_fwrap", "Individual Plot Per Treatment", FALSE)
                    ), # end sidebar
                    card_body(plotOutput("tit_fluoro_scatter"))
                    ) # end sidebar layout
                ), # end gsw scatter plot
                card(card_header("Fruit Scatter", class = "bg-primary", style = "font-size: 20px"),
                    layout_sidebar(sidebar = sidebar(
                      selectInput("tit_fruit_scatter_x","X Variable",
                                  choices = all_tit_fruit_vars, selected = "Mass"),
                      selectInput("tit_fruit_scatter_y","Y Variable",
                                  choices = all_tit_fruit_vars, selected = "pSugar"),
                      selectInput("tit_fruit_scatter_col","Color Variable",
                                  choices = all_tit_fruit_vars, selected = "Treatment"),
                      selectInput("tit_fruit_scatter_shape", "Shape Variable",
                                  choices = tit_fruit_vars_d, selected = "Treatment"),
                      sliderInput("tit_fruit_scatter_jit", "Jitter Amount",
                                  min=0, max=10, value =0),
                      sliderInput("tit_fruit_scatter_size", "Point Size",
                                  min = 1, max=10, value = 3),
                      checkboxInput("tit_fruit_scatter_fwrap", "Individual Plot Per Treatment", FALSE)
                    ), # end sidebar
                    card_body(plotOutput("tit_fruit_scatter"))
                    ) # end sidebar layout
                ) # end fruit scatter plot
            ), # end scatterplot accordion panel
            accordion_panel(title="Box Plots",
              card(card_header("Fluorescence Boxplot", class = "bg-primary", style = "font-size: 20px"),
                   layout_sidebar(sidebar = sidebar(
                     selectInput("tit_fluoro_box_x","X Variable",
                                 choices = tit_fluoro_vars_d, selected = "Treatment"),
                     selectInput("tit_fluoro_box_y","Y Variable",
                                 choices = tit_fluoro_vars, selected = "gsw")
                   ), # end sidebar
                   plotOutput("tit_fluoro_box")
                   )),
              card(card_header("Fruit Boxplot", class = "bg-primary", style = "font-size: 20px"),
                   layout_sidebar(sidebar = sidebar(
                     selectInput("tit_fruit_box_x","X Variable",
                                 choices = tit_fruit_vars_d, selected = "Treatment"),
                     selectInput("tit_fruit_box_y","Y Variable",
                                 choices = tit_fruit_vars, selected = "Mass")
                   ), # end sidebar
                   plotOutput("tit_fruit_box")
                   )),
              card(card_header("Fruit Summary Boxplot", class = "bg-primary", style = "font-size: 20px"),
                   layout_sidebar(sidebar = sidebar(
                     selectInput("tit_fruit_sum_box_x","X Variable",
                                 choices = tit_fruit_sum_vars_d, selected = "Treatment"),
                     selectInput("tit_fruit_sum_box_y","Y Variable",
                                 choices = tit_fruit_sum_vars, selected = "Mass_sum")
                   ), # end sidebar
                   plotOutput("tit_fruit_sum_box")
                   ))
            ) # end boxplot accordion panel
          ) # end accordion
        ), # end exploratory tab panel
        tabPanel("Statistics",
          accordion(
            accordion_panel("Fluorescence",
              card(card_header("Stomatal conductance (gsw)", class = "bg-primary", style = "font-size: 20px"),
                selectInput("tit_gsw_mod_var", "Predictor Variable",
                            choices = fluoro_mod_var_names, selected = "AmbientHumidity"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("tit_gsw_mod_summary")
                    )
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("tit_gsw_mod_call")
                         ),
                    value_box(
                      title = "GSW Model AIC",
                      value = textOutput("tit_gsw_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "GSW Model p-value",
                      value = textOutput("tit_gsw_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "GSW Model R^2",
                      value = textOutput("tit_gsw_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("tit_gsw_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ), # end div
              ), # end gsw stats card
              card(card_header("Photosystem II Efficiency (PhiPS2", class = "bg-primary", style = "font-size: 20px"),
                   selectInput("tit_ps2_mod_var", "Predictor Variable",
                               choices = fluoro_mod_var_names, selected = "AmbientHumidity"),
                   div(layout_columns(col_widths = c(7,5),
                        div(
                          card(card_header("Model Summary"),
                               verbatimTextOutput("tit_ps2_mod_summary")
                          )
                        ),# end model summary div
                        div(# value boxes for AIC and r^2
                          card(card_header("Model Call", class = "bg-primary"),
                               verbatimTextOutput("tit_ps2_mod_call")
                          ),
                          value_box(
                            title = "PhiPS2 Model AIC",
                            value = textOutput("tit_ps2_aic"),
                            theme = "bg-primary",
                            width = 0.2
                          ),
                          value_box(
                            title = "PhiPS2 Model p-value",
                            value = textOutput("tit_ps2_p"),
                            width = 0.2
                          ),
                          value_box(
                            title = "PhiPS2 Model R^2",
                            value = textOutput("tit_ps2_r2"),
                            theme = "bg-secondary",
                            width = 0.2
                          ),
                          card(card_header("Treatment Letters", class="bg-secondary"),
                               verbatimTextOutput("tit_ps2_letters")
                          )
                        ) # end value box div
                   ) # end column wrap
                ) # end div
              ), # end ps2 stats card
              card(card_header("Multivariate", class = "bg-secondary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(6,6),
                  div(
                    markdown("
                      Maybe instead of looking at a single environmental variable, we want to look at all of them (or at least the ones
                      that actually make an impact on our target response variables). To accomplish this, we can use **principal component analysis** (PCA).
                      First, we scale our environmental variables so they all have an equal influence on the output, then use the **rda** function from the **vegan** package
                      to reduce the dimensionality."
                    ),
                    card(card_header("PCA Summary"), 
                       verbatimTextOutput("tit_fluoro_pca_summary")
                    )
                  ),
                  plotOutput("tit_fluoro_pca")
                )),
                div(layout_column_wrap(
                  card(card_header("Multivariate Stomatal Conductance (GSW)", class = "bg-primary", style = "font-size: 20px"),
                       div(# value boxes for AIC and r^2
                         card(card_header("Model Summary", class = "bg-primary"),
                              verbatimTextOutput("tit_pcr_gsw_summary"),
                              max_height = 500
                         ),
                         value_box(
                           title = "Multivariate GSW AIC",
                           value = textOutput("tit_pcr_gsw_aic"),
                           theme = "bg-primary",
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate GSW p-value",
                           value = textOutput("tit_pcr_gsw_p"),
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate GSW R^2",
                           value = textOutput("tit_pcr_gsw_r2"),
                           theme = "bg-secondary",
                           width = 0.2
                         ),
                         card(card_header("Treatment Letters", class="bg-secondary"),
                              verbatimTextOutput("tit_pcr_gsw_letters")
                         )
                       ), # end value box div
                       plotOutput("tit_pcr_gsw_pred")
                       ),
                  card(card_header("Multivariate Photosystem II Efficiency", class = "bg-primary", style = "font-size: 20px"),
                       div(# value boxes for AIC and r^2
                         card(card_header("Model Summary", class = "bg-primary"),
                              verbatimTextOutput("tit_pcr_ps2_summary"),
                              max_height = 500
                         ),
                         value_box(
                           title = "Multivariate PhiPS2 AIC",
                           value = textOutput("tit_pcr_ps2_aic"),
                           theme = "bg-primary",
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate PhiPS2 p-value",
                           value = textOutput("tit_pcr_ps2_p"),
                           width = 0.2
                         ),
                         value_box(
                           title = "Multivariate PhiPS2 R^2",
                           value = textOutput("tit_pcr_ps2_r2"),
                           theme = "bg-secondary",
                           width = 0.2
                         ),
                         card(card_header("Treatment Letters", class="bg-secondary"),
                              verbatimTextOutput("tit_pcr_ps2_letters")
                         )
                       ), # end value box div
                       plotOutput("tit_pcr_ps2_pred")
                  ), # end phips2 card
                )) # end column wrap and div
              ) # end multivariate card
            ),
            accordion_panel("Fruit",
              div(
                markdown("
                > Because our fruit are **pseudoreplicates** (we applied our treatments to the plants, not the individual fruit)
                when we go to make fruit models, we have to average out the values between the plants, rather than looking at the individual fruit
                as a sampling unit. If we didn't do this, our models would have a drastically increased sample size that wouldn't reflect
                our actual experimental design.
                ")
              ),
              card(card_header("Mass", class = "bg-primary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("tit_mass_mod_summary")
                    ),
                    plotOutput("tit_mass_annotated")
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("tit_mass_mod_call")
                         ),
                    value_box(
                      title = "Mass Model AIC",
                      value = textOutput("tit_mass_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "Mass Model p-value",
                      value = textOutput("tit_mass_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "Mass Model R^2",
                      value = textOutput("tit_mass_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("tit_mass_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ) # end div
              ), # end mass stats card
              card(card_header("Sugar", class = "bg-primary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("tit_sug_mod_summary")
                    ),
                    plotOutput("tit_sug_annotated")
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("tit_sug_mod_call")
                         ),
                    value_box(
                      title = "Sugar Model AIC",
                      value = textOutput("tit_sug_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "Sugar Model p-value",
                      value = textOutput("tit_sug_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "Sugar Model R^2",
                      value = textOutput("tit_sug_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("tit_sug_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ) # end div
              ), # end sugar stats card
              card(card_header("Blossom End-Rot", class = "bg-primary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("tit_ber_mod_summary")
                    ),
                    plotOutput("tit_ber_annotated")
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("tit_ber_mod_call")
                         ),
                    value_box(
                      title = "BER Model AIC",
                      value = textOutput("tit_ber_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    value_box(
                      title = "BER Model p-value",
                      value = textOutput("tit_ber_p"),
                      width = 0.2
                    ),
                    value_box(
                      title = "BER Model R^2",
                      value = textOutput("tit_ber_r2"),
                      theme = "bg-secondary",
                      width = 0.2
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("tit_ber_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ) # end div
              ), # end ber stats card
              card(card_header("Fruit Count", class = "bg-primary", style = "font-size: 20px"),
                div(layout_columns(col_widths = c(7,5),
                  div(
                    card(card_header("Model Summary"),
                         verbatimTextOutput("tit_fc_mod_summary")
                    ),
                    plotOutput("tit_fc_annotated")
                  ),# end model summary div
                  div(# value boxes for AIC and r^2
                    card(card_header("Model Call", class = "bg-primary"),
                         verbatimTextOutput("tit_fc_mod_call")
                         ),
                    value_box(
                      title = "Fruit Count Model AIC",
                      value = textOutput("tit_fc_aic"),
                      theme = "bg-primary",
                      width = 0.2
                    ),
                    card(card_header("Fruit Count Model R^2", class = "bg-secondary"),
                         verbatimTextOutput("tit_fc_r2")
                    ),
                    card(card_header("Treatment Letters", class="bg-secondary"),
                         verbatimTextOutput("tit_fc_letters")
                    )
                  ) # end value box div
                ) # end column wrap
                ) # end div
              ) # end fruit count stats card
            ) # end fruit accordion panel
          ) # end stats accordion
        ), # end stats tab panel
        tabPanel("Data",
         card(card_header("Fluorescence Data", class = "bg-primary", style = "font-size: 20px"),
              div(dataTableOutput("tit_fluoro_DT")),
              markdown("This dataset is a combination of data from the LI-COR Li-600
           and PhotosynQ MultispeQ V2.0s. For the sake of this app running
           efficiently, the data has been pared down to strictly what is needed.
           The full datasets can be found [on my github](https://www.github.com/zachpeagler/Thesis/data/TIP24).")
         ),
         card(card_header("Fruit Data", class = "bg-primary", style = "font-size: 20px"),
              dataTableOutput("tit_fruit_DT")
         ),
         card(card_header("Fruit Summary Data", class = "bg-primary", style = "font-size: 20px"),
              dataTableOutput("tit_fruit_sum_DT")
         )
        ), # end data tab panel
        tabPanel("Info",
          div(style = "padding: 10px", align = "center",
              markdown("#### **Tomato Inoculant Timing Trial**")
              ),
          card(card_header("Hypothesis and Objectives", class = "bg-primary", style = "font-size: 20px"),
            markdown("
            This trial accompanies the following hypothesis and objectives laid out in my thesis: <br>
            **Objective 2** – Determine the effect of biostimulant granule (BG) 
            inoculation timing on tomato plant growth and crop quality and yield.
            - **Hypothesis 2.1** – Because of the priority effect and the mass effect,
            BGs with the full microbial consortium applied at both germination and
            transplantation will increase fluorescence parameters, fruit yield, 
            and fruit quality more than either germination or transplantation inoculations.
            > It should be noted that these hypotheses are technically *predictive hypotheses*, as not only do they hypothesize a 
            change, they also specify a prediction for that change. I.E. Fluorescence parameters will not only *change*, they will *increase*.
            This is a very minor distinction, but important to those in the science realm (nerds). <br>
            ")
               ), # end hypothesis and objective card
          div(
            layout_column_wrap(
              card(img(src = "TIP24_1.jpg"),
                  markdown("Tomato plants six days after germination")
                  ),
              card(img(src = "TIP24_2.jpg"),
                  markdown("Tomato plants one month after germination.")
              ),
              card(img(src = "TIP24_3.jpg"),
                  markdown("Tomato plants four months after germination.")
              ),
            ) # end column wrap
          ), # end tomato growth pictures div
          card(card_header("Methods", class = "bg-secondary", style = "font-size: 20px"),
            markdown("
            The space available for the tomato inoculant timing trial was greater 
            than that available for the tomato inoculant location trial, allowing for more replicates per group,
            thus increasing statistical power to 0.59 for an effect size of 0.4 with 4 groups
            and 12 replicates per group at a 0.05 significance level. The plants were grown 
            in the hydroponic greenhouse at the KSU Field Station (34.0622° N, 84.6034° W). 
            Tomato seeds were germinated in Oasis cubes and fertilized for three weeks before being 
            transplanted into Dutch buckets containing Mother Earth™ Coco + Perlite Mix, copper-lined root barriers,
            and Mother Earth™ Hydroton Original clay pellets. A nutrient mix composed of 1.32 g/L Jack’s 0-12-26,
            0.075 g/L magnesium sulfate, and 0.912 g/L calcium nitrate was mixed in 1000-liter batches
            and fed into the Dutch buckets. <br>
            The treatments include inoculation at germination, inoculation at transplantation,
            inoculation at both germination and transplantation, and a control no inoculation group.
            All inoculation groups used the entire microbial consortium listed below encapsulated in chitosan.
            For inoculation at germination, a single BG was placed atop the seed in the Oasis cube.
            For inoculation at transplantation, approximately 5 grams of BGs were placed in contact with the plant roots.
            The joint inoculant group effectively received a double dose, as they received inoculation at both germination and transplantation.
            Each group had 12 replicates, for a total sample size of 48 tomato plants. 
            Plants were germinated on May 1st, 2024 and transplanted on May 23rd, 2024. 
            Plants were allowed to grow until October 19th, 2024, at which point they were taken down with samples collected for stomatal density measurements.
            Biweekly measurements were taken with a Li-COR Li-600 and two PhotosynQ MultispeQ V2.0s.
            Fruit were harvested when ripe, as assessed by color and firmness, and were taken back to the lab for analysis.
            Weight measurements were performed with an OHAUS Scout SCA210, and blossom-end rot was assessed by visual observation
            of the fruit. Sugar concentration was measured with a Fisherbrand Brix refractometer.
            Firmness was measured with an OA Supplies Fruit Pressure Tester (penetrometer). <br>
            The fruit data is formatted in a tidy format with each row corresponding to one fruit and each column representing a variable. 
            The fluorescence data is formatted in a tidy format with each row corresponding to a single plant measurement and each column representing a variable.
            <br>
            ##### **Microbial Consortium** <br>
            - *Azospirillum brasilense Sp7*- PGPB that benefits the plant via nitrogen fixation, siderophore production,
            and by increasing lateral root growth ([Sahoo et al, 2014](https://pubmed.ncbi.nlm.nih.gov/24414168/); [Li et al, 2005](https://pubmed.ncbi.nlm.nih.gov/16121231/)),
            and has been shown to increase plant stress tolerance ([Casanovas et al, 2002](https://www.jstor.org/stable/23787082?seq=1)).
            It has been shown to increase crop yield and plant nitrogen, phosphorous, and potassium content
            ([Askary et al, 2009](https://www.researchgate.net/publication/347438334_Influence_of_the_Co-inoculation_Azospirillum_brasilense_and_Rhizobium_meliloti_plus_24-D_on_Grain_Yield_and_N_P_K_Content_of_Triticum_aestivum_Cv_Baccros_and_Mahdavi)).
            It has also been reported to work well with Methylobacterium oryzae ([Madhaiyan et al, 2009](https://www.researchgate.net/publication/225966871_Effect_of_co-inoculation_of_methylotrophic_Methylobacterium_oryzae_with_Azospirillum_brasilense_and_Burkholderia_pyrrocinia_on_the_growth_and_nutrient_uptake_of_tomato_red_pepper_and_rice)). <br>
            - *Azotobacter chroococcum 43* - PGPB that operates via nitrogen fixation, phosphate solubilization,
            and vitamin, indole acetic acid (IAA), gibberellin (GA), hydrogen cyanide (HCN), siderophore,
            and cytokinin (CK) production ([Abd El-Fattah et al, 2013](https://www.researchgate.net/publication/259130343_Effect_of_carrier_materials_sterilization_method_and_storage_temperature_on_survival_and_biological_activities_of_Azotobacter_chroococcum_inoculant); [Revillas et al, 2000](https://pubmed.ncbi.nlm.nih.gov/11021581/); [Wani et al, 2007](https://www.researchgate.net/publication/240762870_Co-inoculation_of_nitrogen-fixing_and_phosphate-solubilizing_bacteria_to_promote_growth_yield_and_nutrient_uptake_in_chickpea)).
            Shown to increase germination rates and aboveground biomass and crop quality and yield in maize ([Zahir et al, 2005](https://www.researchgate.net/publication/233130106_Precursor_L-tryptophan-Inoculum_Azotobacter_Interaction_for_Improving_Yields_and_Nitrogen_Uptake_of_Maize)). <br>
            - *Bacillus subtilis* - PGPB that has been shown to improve fruit quality and yield in tomato
            ([Mena-Violante & Olalde-Portugal, 2007](https://www.researchgate.net/publication/222326135_Alteration_of_tomato_fruit_quality_by_root_inoculation_with_plant_growth-promoting_rhizobacteria_PGPR_Bacillus_subtilis_BEB-13bs); [Kokalis-Burelle et al, 2002](https://link.springer.com/article/10.1023/A:1014464716261))
            and shown to increase metabolite production ([Sharaf-Eldin et al, 2008](https://pubmed.ncbi.nlm.nih.gov/18622904/)).
            Shown to solubilize phosphate, fix nitrogen, produce IAA, CK, GA, HCN, and antibiotics,
            as well as exhibiting phytase activity ([Ahmad et al, 2008](https://pubmed.ncbi.nlm.nih.gov/16735107/); [Arkhipova et al, 2005](https://link.springer.com/article/10.1007/s11104-004-5047-x); [Yao et al, 2006](https://www.researchgate.net/publication/233193377_Effect_of_FZB_24_Bacillus_subtilis_as_biofertilizer_on_cotton_yields_in_field_tests)).
            It has been used as a biocontrol agent against aphids and pathogenic bacteria ([Kokalis-Burelle et al, 2002](https://link.springer.com/article/10.1023/A:1014464716261)). <br>
            - *Methylobacterium oryzae CBMB20* - PGPB that has been shown to improve fruit quality and yield in tomato
            in both foliar and chitosan encapsulated inoculations ([Chanratana et al., 2019](https://www.researchgate.net/profile/Aritra-Choudhury/publication/323564168_Evaluation_of_chitosan_and_alginate_immobilized_Methylobacterium_oryzae_CBMB20_on_tomato_plant_growth/links/5a9e6fcfa6fdcc214af2b315/Evaluation-of-chitosan-and-alginate-immobilized-Methylobacterium-oryzae-CBMB20-on-tomato-plant-growth.pdf)). 
            Operates through phytohormone (auxin and cytokinin) production, stress reduction via ACC deaminase production, 
            increased nutrient availability through nitrogen fixation, and as a biopesticide ([Chauhan et al., 2015](https://www.sciencedirect.com/science/article/abs/pii/S0929139315300159)). <br>
            - *Pseudomonas putida 90* - PGPB that increases plant growth by solubilizing phosphate and producing
            IAA and siderophores ([Hariprasad & Niranjana, 2009](https://link.springer.com/article/10.1007/s11104-008-9754-6)). Shown to inhibit ethylene production ([Mayak et al, 1999](https://pubmed.ncbi.nlm.nih.gov/10552131/)).
            Shown to significantly increase tomato fruit macro- and micronutrient content ([He et al, 2019](https://pubmed.ncbi.nlm.nih.gov/30955229/)). 
            Shown to increase potassium, magnesium, and calcium uptake and decrease sodium uptake ([Yao et al, 2010](https://www.sciencedirect.com/science/article/abs/pii/S1164556309001046)). 
            Also shown to increase root and shoot growth ([Glick et al, 1997](https://www.researchgate.net/publication/223543401_Early_development_of_canola_seedlings_in_the_presence_of_the_plant_growth-promoting_rhizobacterium_Pseudomonas_putida_GR12-2); [Hall et al, 1996](https://ui.adsabs.harvard.edu/abs/1996IsJPS..44...37H/abstract)). <br>
            ")
            ), # end methods card
          card(card_header("Variables", class = "bg-primary", style = "font-size: 20px"),
            markdown("
            ##### **Explanatory Variables**
            - **Treatment** is the inoculation timing of the tomato. Options are Control, Germination, Transplantation, and Germ+Trans. <br>
            - **Row** is the row of the tomato. (A:D) <br>
            - **Pot** is the pot number of the tomato. (1:12) <br>
            - **Plant** is a combination of *Row* and *Pot*, and acts as an ID for every individual plant. (A1: D12) <br>
            ###### **Fluorescence**
            - **Time** is the time at which the measurement was taken <br>
            - **Date** is the date at which the measurement was taken <br>
            - **DaysFromGermination** is the number of days from germination (2025-05-01) to the date of measurement. <br>
            - **AmbientHumidity** is the relative humidity (%) at the time of measurement. <br>
            - **AmbientLight** is the ambient light level (lumens) at the time of measurement. <br>
            - **AmbientPressure** is the ambient pressure (kPa) at the time of measurement. <br>
            - **LeafTemperature** is the temperature (Celcius) of the leaf. <br>
            ###### **Fruit**
            - **DateHarvest** (date) is the date the fruit was harvested (August 2024:October 2024) <br>
            - **DateAnalysis** (date) is the date the fruit was analyzed in the lab (August 2024:October 2024) <br>
            - **DaysFromHarvestToAnalysis** (int) is the number of days from harvest to analysis (fruit only). <br>
            - **Penetrometer** corresponds to the force in kilograms it takes to penetrate the flesh of the tomato (~0.5:~4) <br>
            - **Ripeness** is the **Penetrometer** value mapped from 0:1 and reversed, so that riper fruit are closer to 1 and unripe fruit are closer to 0. (0:1) <br>
            ---
            ##### **Response Variables**
            ###### **Fluorescence**
            - **gsw** is the stomatal conductance (mol m-2 s-1) of the leaf. Stomatal conductance refers to the
            rate at which molecules are moving through the leaf's stomates, and is indicitave of photosynthesis.<br>
            - **PhiPS2** is the efficiency of Photosystem II. It is unitless. (0:1) <br>
            ###### **Fruit**
            - **Mass** is the mass of the tomato (grams), measured on an Ohaus Scout. (~10:~400) <br>
            - **BER** corresponds to whether or not the tomato has blossom end rot, a disease caused by calcium deficiency that renders the fruit unmarketable. (0,1) <br>
            - **pSugar** is the average of two measurements of the tomato juice's sugar concentration taken on a Fisher BRIX Refractometer (~2:~12) <br>
            - **SugarGrams** is the grams of sugar in each tomato, calculated as **pSugar** x **Mass** <br>
            
            > It's important to note that **only** the Li-600 can measure gsw, while both
            the Li-600 and the MultispeQ can measure PhiPS2. Also, even though both devices can 
            measure PhiPS2, they do so **in different ways**. For our purposes, this is fine
            so long as the measurements from each device correlate. <br>
            "),
            div(style="border-left: 5px solid", 
              markdown(
                "> For a more comprehensive explanation of PhiPS2, check out 
                [Genty *et al*., 1989](https://www.sciencedirect.com/science/article/abs/pii/S0304416589800169) or
                for a simpler explanation, the [chlorophyll fluorescence wikipedia page](https://en.wikipedia.org/wiki/Chlorophyll_fluorescence)."
              )
            )
          ) # end variable card
          ) # end info tab Panel
        ) # end tabset Panel
      ) # end TIT tabpanel
    )# end "Tomato Inoculant Trials" tabsetpanel
  ),
  ##### ABOUT NAV PANEL #####
  nav_panel("About",
    card(card_header("About the app", class = "bg-primary", style = "font-size: 20px"),
      div(layout_columns(col_widths = c(8,4),
       markdown(
         "This app is built using **R** 4.4.3 and uses a myriad of packages, 
         including **shiny**, **ggplot2**, **bslib**, **bsicons**, **MASS**, **MuMIn**, and **lmerTest**.
         It also uses several custom functions for calculating multiple Kolmorogov-Smirnov tests simultaneous and producing
         PDF, CDF, and prediction plots. These functions are from my package **ztils**, which you can check out on
         [CRAN](https://cran.r-project.org/web/packages/ztils/index.html) or view the source code on [Github](https://github.com/zachpeagler/ztils).
         This app was originally intended to be containerized with Docker and hosted to an Amazon Web Services
         Elastic Kubernetes Cluster, but with the recent advent of shinylive, it is now deployed as a static webpage on Github Pages. This significantly
         reduces cost at the price of performance. As a broke college student, this is a tradeoff I'm willing to accept.
         Instead of the app running on the server, the app runs locally in the browser using WebR. 
         The diagram to the right illustrates the shinylive architecture (in broad strokes). I briefly considered using Quarto to develop this app,
         but Quarto's integration with Shiny has a few features lacking that I really wanted to implement in this app, so I stuck with base R. The switch
         to shinylive also came at the cost of custom fonts, much to my chagrin. While it's still possible to use custom fonts with shinylive, becayuse shinylive is incompatible with **curl** it's not possible
         to fetch them remotely using **showtext** and **sysfonts** (i.e. using the font_add_google() command) and would've required multiple megabytes of space and a whole bunch of custom CSS.
         The source code for this app can be found on the app [Github repository](github.com/zachpeagler/tomato-inoculant-app), accessible via that link or the GitHub icon in the top right corner of the app.
         "
       ),
       card(img(src="shinylive-webr.jpg"),
            markdown("Image courtesy of [Posit](https://shiny.posit.co/py/docs/shinylive.html)"))
      )),
    ), # end about the app card
    card(card_header("About the author", class="bg-primary", style = "font-size: 20px"),
      div(layout_columns(col_widths = c(3,6,3),
        card(img(src="me_1.png")),
        markdown(
          "Zach Peagler graduated from Kennesaw State University (KSU) in 2022 with a 
          Bachelor's of Science in Biology with a Minor in Chemistry. He then (assuming this presentation goes well)
          graduated from KSU with a Master's of Science in Integrative Biology in May 2025. 
          <br>
          In his free time, he enjoys reading, software development, woodworking, and spending time with his wife and two dogs.
          "
        ),
        card(img(src="me_2.png"))
      ) # end column layout
      ) # end div
    ), # end about the author card
    card(card_header("Acknowledgements", class="bg-secondary", style = "font-size: 20px"),
      markdown("I would like to thank my wonderful mentor, Dr. Mario Bretfeld, and my committee,
               Dr. Chris Cornelison, Dr. Kyle Gabriel, and Dr. Matt Weand. I would also like to thank
               the undergraduate researchers who helped with this project, without whom this would not
               have been possible: Minh Phung, Maddie Cusick, Jacob Erasmus, Serena N'guessan, Kenadi Morgan,
               and Collin Williams")
    ),
    card(card_header("References", class="bg-primary", style = "font-size: 20px"),
      markdown(" ##### **References**
      - Abd El-Fattah, D.A., *et al*. 2013. *Effect of carrier materials, sterilization method, and storage temperature on survival and biological activities of* Azotobacter chroococcum *inoculants*. Ann. Agric. Sci. 58:111-118. <br>
      - Ahmad, F., *et al*. 2008. *Screening of free-living rhizospheric bacteria for their multiple plant growth promoting activities*. Microbiological Research, 163(2):173-181. <br>
      - Arkhipova, T., *et al*. 2005. *Ability of bacterium* Bacillus subtilis *to produce cytokinins and to influence the growth and endogenous hormone content of lettuce plants*. Plant Soil. 272:201-209. <br>
      - Askary, M. *et al*. 2009. *Influence of the co-inoculation* Azospirillum brasilense *and* Rhizobium meliloti *plus 2,4-D on Grain Yield and N, P, K content of* Triticum aestivum. American-Eurasian J. Agric. & Environ. Sci., 5(3):296-307 <br>
      - Casanovas, E.M., *et al*. 2002. *Azospirillum inoculation mitigates water stress effects in Maize Seedlings*. Cereal Res. Comm. 30:343-350. <br>
      - Chanratana, M., *et al.* 2018. *Evaluation of chitosan and alginate immobilized* Methylobacterium oryzae CBMB20 *on tomato plant growth*. Archives of Agronomy and Soil Sci. 64(11):1489-1502. 
      - Chauhan, H., *et al.* (2015). *Novel plant growth promoting rhizobacteria—Prospects and potential*. Applied Soil Ecology, 95, 38–53. doi:10.1016/j.apsoil.2015.05.011 <br>
      - Glick, B. R., *et al*. 1997. *Early development of canola seedlings in the presence of the plant growth-promoting rhizobacterium* Pseudomonas putida GR12-2. Soil Biology and Biochemistry, 28(8):1233-1239 DOI 10.1016/S0038-0717(97)00026-6 <br>
      - Hall, J.A., *et al*. 1996. *Root elongation in various crops by the plant growth promoting rhizobacteria* Pseudomonas putida GR12-2. Isr. J. Plant Sci. 44:37-42. <br>
      - Hariprasad, P., Niranjana, S.R. 2009. *Isolation and characterization of phosphate solubilizing rhizobacteria to improve plant health of tomato*. Plant Soil. 316:13-24. <br>
      - He, Y., *et al*. 2019. *Co-inoculation of* Bacillus sp. *and* Pseudomonas putida *at different development stages acts as a biostimulant to promoted growth, yield and nutrient uptake of tomato*. J. Appl. Micro. DOI: 10.1111/jam.14273. <br>
      - Kokalis-Burelle, N., *et al*. 2002. *Field evaluation of plant growth-promoting rhizobacteria amended transplant mixes and soil solarization for tomato and pepper production in Florida*. Plant Soil. 238:257-266. <br>
      - Li, Q., *et al*. 2005. *The effect of native and ACC deaminase-containing* Azospirillum brasilense Cd1843 *on the rooting of carnation cuttings*. Can. J. Microbiol. 51:511-514. <br>
      - Madhaiyan, M. *et al*. 2009. *Effect of co-inoculation of methylotrophic* Methylobacterium oryzae *with* Azospirillum brasilense *and* Burkholderia pyrrocinia *on the growth and nutrient uptake of tomato, red pepper and rice*. Plant Soil 328:71-82 DOI 10.1007/s11104-009-0083-1 <br>
      - Mayak, S. et al. 1999. *Effect of wild-type and mutant plant growth-promoting rhizobacteria on the rooting of mung bean cuttings*. J. Plant Growth Regul. 18:49-53. <br>
      - Mena-Violante, H., Olalde-Portugal, V. 2007. *Alteration of tomato fruit quality by root inoculation with plant growth-promoting rhizobacteria (PGPR)*: Bacillus subtilis BEB-13s. Sci. Hortic-Amsterdam 113:103-106. <br>
      - Revillas, J.J., *et al*. 2000. *Production of B-Group vitamins by two Azotobacter strains with phenolic compounds as sole carbon source under diazotrophic and adiazotrophic conditions*. J. Appl. Microbiol. 89:486-493. <br>
      - Sahoo, R.K., *et al*. 2014. *Phenotypic and molecular characterization of efficient native Azospirillum strains from rice fields for crop improvement*. Protoplasma. 251(4):943-953. <br>
      - Sharaf-Eldin, M., *et al*. 2008. Bacillus subtilis FZB24 *affects flower quantity and quality of Saffron* (Crocus sativus). Planta Med. 74:1316-1320. <br>
      - Yao, A.V., *et al*. 2006. *Effect of* FZB 24 Bacillus subtilis *as a biofertilizer on cotton yields in field tests*. Arch. Phytopathol. Plant Prot. 39:323-328. <br>
      - Yao, Y., *et al*. 2010. *Growth promotion and protection against salt stress by* Pseudomonas putida Rs-198 *on cotton*. European J. Soil Biol. 46:49-54. <br>
      - Zahir, Z.A., *et al*. 2005. *Precursor (L-tryptophan)-inoculum (Azotobacter) interaction for improving yields and nitrogen uptake in maize*. J. Plant Nutr. 28:805-817. <br>
      ")
    )
  ),
  nav_spacer(),
  nav_item(gear),
  nav_item(link_github)
)

##### SERVER #####
server <- function(input, output) {
# Reactive Expressions
### you might say "don't make a thousand individual reactive expressions!!! make a reactive values
### object and store them all in that!!" and to that i say "no"
### these are all "lazy" so they should (in theory) be more optimized than updating
### all the inputs in a single reactive values object. less updates = faster. right?
#### this is a really stupid way of doing this, and i'll freely admit that.
#### but it works and is actually pretty fast? so uhhhh... it stays
## global reactive expressions
  Rpalette <- reactive({input$palette})
## tim reactive expressions
### fluorescence
  Rtim_fluoro_dist_var <- reactive({input$tim_fluoro_dist_var})
  Rtim_fluoro_dists <- reactive({input$tim_fluoro_dists})
  Rtim_fluoro_len <- reactive({input$tim_fluoro_len})
  Rtim_fluoro_hist_var <- reactive({input$tim_fluoro_hist_var})
  Rtim_fluoro_hist_color <- reactive({input$tim_fluoro_hist_color})
  Rtim_fluoro_hist_bins <- reactive({input$tim_fluoro_hist_bins})
  Rtim_fluoro_scatter_x <- reactive({input$tim_fluoro_scatter_x})
  Rtim_fluoro_scatter_y <- reactive({input$tim_fluoro_scatter_y})
  Rtim_fluoro_scatter_col <- reactive({input$tim_fluoro_scatter_col})
  Rtim_fluoro_scatter_shape <- reactive({input$tim_fluoro_scatter_shape})
  Rtim_fluoro_scatter_jit <- reactive({input$tim_fluoro_scatter_jit * 0.1})
  Rtim_fluoro_scatter_fwrap <- reactive({input$tim_fluoro_scatter_fwrap})
  Rtim_fluoro_scatter_size <- reactive({input$tim_fluoro_scatter_size})
  Rtim_fluoro_box_x <- reactive({input$tim_fluoro_box_x})
  Rtim_fluoro_box_y <- reactive({input$tim_fluoro_box_y})
  Rtim_gsw_mod_var <- reactive({input$tim_gsw_mod_var})
  Rtim_gsw_mod <- reactive({
    lm(log(gsw) ~ Treatment + data_tim_fluoro[[Rtim_gsw_mod_var()]], data = data_tim_fluoro)
  })
  Rtim_gsw_mod_sum <- reactive({summary(Rtim_gsw_mod())})
  Rtim_ps2_mod_var <- reactive({input$tim_ps2_mod_var})
  Rtim_ps2_mod <- reactive({
    lm(LogitPhiPS2 ~ Treatment + data_tim_fluoro[[Rtim_ps2_mod_var()]], data = data_tim_fluoro)
  })
  Rtim_ps2_mod_sum <- reactive({summary(Rtim_ps2_mod())})
### height
  Rtim_height_dist_var <- reactive({input$tim_height_dist_var})
  Rtim_height_dists <- reactive({input$tim_height_dists})
  Rtim_height_len <- reactive({input$tim_height_len})
  Rtim_height_hist_var <- reactive({input$tim_height_hist_var})
  Rtim_height_hist_color <- reactive({input$tim_height_hist_color})
  Rtim_height_hist_bins <- reactive({input$tim_height_hist_bins})
  Rtim_height_scatter_x <- reactive({input$tim_height_scatter_x})
  Rtim_height_scatter_y <- reactive({input$tim_height_scatter_y})
  Rtim_height_scatter_col <- reactive({input$tim_height_scatter_col})
  Rtim_height_scatter_shape <- reactive({input$tim_height_scatter_shape})
  Rtim_height_scatter_jit <- reactive({input$tim_height_scatter_jit * 0.1})
  Rtim_height_scatter_fwrap <- reactive({input$tim_height_scatter_fwrap})
  Rtim_height_scatter_size <- reactive({input$tim_height_scatter_size})
  Rtim_height_box_x <- reactive({input$tim_height_box_x})
  Rtim_height_box_y <- reactive({input$tim_height_box_y})
### ds
  Rtim_ds_dist_var <- reactive({input$tim_ds_dist_var})
  Rtim_ds_dists <- reactive({input$tim_ds_dists})
  Rtim_ds_len <- reactive({input$tim_ds_len})
  Rtim_ds_hist_var <- reactive({input$tim_ds_hist_var})
  Rtim_ds_hist_color <- reactive({input$tim_ds_hist_color})
  Rtim_ds_hist_bins <- reactive({input$tim_ds_hist_bins})
  Rtim_ds_scatter_x <- reactive({input$tim_ds_scatter_x})
  Rtim_ds_scatter_y <- reactive({input$tim_ds_scatter_y})
  Rtim_ds_scatter_col <- reactive({input$tim_ds_scatter_col})
  Rtim_ds_scatter_shape <- reactive({input$tim_ds_scatter_shape})
  Rtim_ds_scatter_jit <- reactive({input$tim_ds_scatter_jit * 0.1})
  Rtim_ds_scatter_fwrap <- reactive({input$tim_ds_scatter_fwrap})
  Rtim_ds_scatter_size <- reactive({input$tim_ds_scatter_size})
  Rtim_ds_box_x <- reactive({input$tim_ds_box_x})
  Rtim_ds_box_y <- reactive({input$tim_ds_box_y})
## til reactive expressions
### fluorescence
  Rtil_fluoro_dist_var <- reactive({input$til_fluoro_dist_var})
  Rtil_fluoro_dists <- reactive({input$til_fluoro_dists})
  Rtil_fluoro_len <- reactive({input$til_fluoro_len})
  Rtil_fluoro_hist_var <- reactive({input$til_fluoro_hist_var})
  Rtil_fluoro_hist_color <- reactive({input$til_fluoro_hist_color})
  Rtil_fluoro_hist_bins <- reactive({input$til_fluoro_hist_bins})
  Rtil_fluoro_scatter_x <- reactive({input$til_fluoro_scatter_x})
  Rtil_fluoro_scatter_y <- reactive({input$til_fluoro_scatter_y})
  Rtil_fluoro_scatter_col <- reactive({input$til_fluoro_scatter_col})
  Rtil_fluoro_scatter_shape <- reactive({input$til_fluoro_scatter_shape})
  Rtil_fluoro_scatter_jit <- reactive({input$til_fluoro_scatter_jit * 0.1})
  Rtil_fluoro_scatter_fwrap <- reactive({input$til_fluoro_scatter_fwrap})
  Rtil_fluoro_scatter_size <- reactive({input$til_fluoro_scatter_size})
  Rtil_fluoro_box_x <- reactive({input$til_fluoro_box_x})
  Rtil_fluoro_box_y <- reactive({input$til_fluoro_box_y})
  Rtil_gsw_mod_var <- reactive({input$til_gsw_mod_var})
  Rtil_gsw_mod <- reactive({
    lm(log(gsw) ~ Treatment + data_til_fluoro[[Rtil_gsw_mod_var()]], data = data_til_fluoro)
  })
  Rtil_gsw_mod_sum <- reactive({summary(Rtil_gsw_mod())})
  Rtil_ps2_mod_var <- reactive({input$til_ps2_mod_var})
  Rtil_ps2_mod <- reactive({
    lm(LogitPhiPS2 ~ Treatment + data_til_fluoro[[Rtil_ps2_mod_var()]], data = data_til_fluoro)
  })
  Rtil_ps2_mod_sum <- reactive({summary(Rtil_ps2_mod())})
### fruit
  Rtil_fruit_dist_var <- reactive({input$til_fruit_dist_var})
  Rtil_fruit_dists <- reactive({input$til_fruit_dists})
  Rtil_fruit_len <- reactive({input$til_fruit_len})
  Rtil_fruit_hist_var <- reactive({input$til_fruit_hist_var})
  Rtil_fruit_hist_color <- reactive({input$til_fruit_hist_color})
  Rtil_fruit_hist_bins <- reactive({input$til_fruit_hist_bins})
  Rtil_fruit_scatter_x <- reactive({input$til_fruit_scatter_x})
  Rtil_fruit_scatter_y <- reactive({input$til_fruit_scatter_y})
  Rtil_fruit_scatter_col <- reactive({input$til_fruit_scatter_col})
  Rtil_fruit_scatter_shape <- reactive({input$til_fruit_scatter_shape})
  Rtil_fruit_scatter_jit <- reactive({input$til_fruit_scatter_jit * 0.1})
  Rtil_fruit_scatter_fwrap <- reactive({input$til_fruit_scatter_fwrap})
  Rtil_fruit_scatter_size <- reactive({input$til_fruit_scatter_size})
  Rtil_fruit_mod_var <- reactive({input$til_fruit_mod_var})
  Rtil_fruit_box_x <- reactive({input$til_fruit_box_x})
  Rtil_fruit_box_y <- reactive({input$til_fruit_box_y})
  Rtil_fruit_sum_box_x <- reactive({input$til_fruit_sum_box_x})
  Rtil_fruit_sum_box_y <- reactive({input$til_fruit_sum_box_y})
## tit reactive expressions
### fluorescence
  Rtit_fluoro_dist_var <- reactive({input$tit_fluoro_dist_var})
  Rtit_fluoro_dists <- reactive({input$tit_fluoro_dists})
  Rtit_fluoro_len <- reactive({input$tit_fluoro_len})
  Rtit_fluoro_hist_var <- reactive({input$tit_fluoro_hist_var})
  Rtit_fluoro_hist_color <- reactive({input$tit_fluoro_hist_color})
  Rtit_fluoro_hist_bins <- reactive({input$tit_fluoro_hist_bins})
  Rtit_fluoro_scatter_x <- reactive({input$tit_fluoro_scatter_x})
  Rtit_fluoro_scatter_y <- reactive({input$tit_fluoro_scatter_y})
  Rtit_fluoro_scatter_col <- reactive({input$tit_fluoro_scatter_col})
  Rtit_fluoro_scatter_shape <- reactive({input$tit_fluoro_scatter_shape})
  Rtit_fluoro_scatter_jit <- reactive({input$tit_fluoro_scatter_jit * 0.1})
  Rtit_fluoro_scatter_fwrap <- reactive({input$tit_fluoro_scatter_fwrap})
  Rtit_fluoro_scatter_size <- reactive({input$tit_fluoro_scatter_size})
  Rtit_fluoro_box_x <- reactive({input$tit_fluoro_box_x})
  Rtit_fluoro_box_y <- reactive({input$tit_fluoro_box_y})
  Rtit_gsw_mod_var <- reactive({input$tit_gsw_mod_var})
  Rtit_gsw_mod <- reactive({
    lm(log(gsw) ~ Treatment + data_tit_fluoro[[Rtit_gsw_mod_var()]], data = data_tit_fluoro)
    })
  Rtit_gsw_mod_sum <- reactive({summary(Rtit_gsw_mod())})
  Rtit_ps2_mod_var <- reactive({input$tit_ps2_mod_var})
  Rtit_ps2_mod <- reactive({
    lm(LogitPhiPS2 ~ Treatment + data_tit_fluoro[[Rtit_ps2_mod_var()]], data = data_tit_fluoro)
  })
  Rtit_ps2_mod_sum <- reactive({summary(Rtit_ps2_mod())})
### fruit
  Rtit_fruit_dist_var <- reactive({input$tit_fruit_dist_var})
  Rtit_fruit_dists <- reactive({input$tit_fruit_dists})
  Rtit_fruit_len <- reactive({input$tit_fruit_len})
  Rtit_fruit_hist_var <- reactive({input$tit_fruit_hist_var})
  Rtit_fruit_hist_color <- reactive({input$tit_fruit_hist_color})
  Rtit_fruit_hist_bins <- reactive({input$tit_fruit_hist_bins})
  Rtit_fruit_scatter_x <- reactive({input$tit_fruit_scatter_x})
  Rtit_fruit_scatter_y <- reactive({input$tit_fruit_scatter_y})
  Rtit_fruit_scatter_col <- reactive({input$tit_fruit_scatter_col})
  Rtit_fruit_scatter_shape <- reactive({input$tit_fruit_scatter_shape})
  Rtit_fruit_scatter_jit <- reactive({input$tit_fruit_scatter_jit * 0.1})
  Rtit_fruit_scatter_fwrap <- reactive({input$tit_fruit_scatter_fwrap})
  Rtit_fruit_scatter_size <- reactive({input$tit_fruit_scatter_size})
  Rtit_fruit_mod_var <- reactive({input$tit_fruit_mod_var})
  Rtit_fruit_box_x <- reactive({input$tit_fruit_box_x})
  Rtit_fruit_box_y <- reactive({input$tit_fruit_box_y})
  Rtit_fruit_sum_box_x <- reactive({input$tit_fruit_sum_box_x})
  Rtit_fruit_sum_box_y <- reactive({input$tit_fruit_sum_box_y})
# Outputs
##### TIM OUTPUTS #####
### Distributions
#### Fluorescence
##### ks
  output$tim_fluoro_KS_title <- renderText({
    markdown(paste("###### **One-sample Kolmogorov-Smirnov tests for ", Rtim_fluoro_dist_var(), " against selected distributions**"))
  })
  output$tim_fluoro_KS <- renderPrint({
    multiKS_cont(data_tim_fluoro[[Rtim_fluoro_dist_var()]], Rtim_fluoro_dists())
  })
##### pdf
  output$tim_fluoro_pdf_title <- renderText({
    markdown(paste("##### **PDF for ", Rtim_fluoro_dist_var(), " over selected distributions**"))
  })
  output$tim_fluoro_pdf <- renderPlot({
    multiPDF_plot(data_tim_fluoro[[Rtim_fluoro_dist_var()]], Rtim_fluoro_len(), Rtim_fluoro_dists(), palette = Rpalette(), var_name = gettext(Rtim_fluoro_dist_var()))
  })
##### cdf
  output$tim_fluoro_cdf <- renderPlot({
    multiCDF_plot(data_tim_fluoro[[Rtim_fluoro_dist_var()]], Rtim_fluoro_len(), Rtim_fluoro_dists(), palette = Rpalette(), var_name = gettext(Rtim_fluoro_dist_var()))
  })
  output$tim_fluoro_cdf_title <- renderText({
    markdown(paste("##### **CDF for ", Rtim_fluoro_dist_var(), " over selected distributions**"))
  })
#### Height
##### ks
  output$tim_height_KS_title <- renderText({
    markdown(paste("###### **One-sample Kolmogorov-Smirnov tests for ", Rtim_height_dist_var(), " against selected distributions**"))
  })
  output$tim_height_KS <- renderPrint({
    multiKS_cont(data_tim_height[[Rtim_height_dist_var()]], Rtim_height_dists())
  })
##### pdf
  output$tim_height_pdf <- renderPlot({
    multiPDF_plot(data_tim_height[[Rtim_height_dist_var()]], Rtim_height_len(), Rtim_height_dists(), palette = Rpalette(), var_name = gettext(Rtim_height_dist_var()))
  })
  output$tim_height_pdf_title <- renderText({
    markdown(paste("##### **PDF for ", Rtim_height_dist_var(), " over selected distributions**"))
  })
##### cdf
  output$tim_height_cdf <- renderPlot({
    multiCDF_plot(data_tim_height[[Rtim_height_dist_var()]], Rtim_height_len(), Rtim_height_dists(), palette = Rpalette(), var_name = gettext(Rtim_height_dist_var()))
  })
  output$tim_height_cdf_title <- renderText({
    markdown(paste("##### **CDF for ", Rtim_height_dist_var(), " over selected distributions**"))
  })
#### DS
##### ks
  output$tim_ds_KS <- renderPrint({
    multiKS_cont(data_tim_ds[[Rtim_ds_dist_var()]], Rtim_ds_dists())
  })
  output$tim_ds_KS_title <- renderText({
    markdown(paste("###### **One-sample Kolmogorov-Smirnov tests for ", Rtim_ds_dist_var(), " against selected distributions**"))
  })
##### pdf
  output$tim_ds_pdf <- renderPlot({
    multiPDF_plot(data_tim_ds[[Rtim_ds_dist_var()]], Rtim_ds_len(), Rtim_ds_dists(), palette = Rpalette(), var_name = gettext(Rtim_ds_dist_var()))
  })
  output$tim_ds_pdf_title <- renderText({
    markdown(paste("##### **PDF for ", Rtim_ds_dist_var(), " over selected distributions**"))
  })
##### cdf
  output$tim_ds_cdf <- renderPlot({
    multiCDF_plot(data_tim_ds[[Rtim_ds_dist_var()]], Rtim_ds_len(), Rtim_ds_dists(), palette = Rpalette(), var_name = gettext(Rtim_ds_dist_var()))
  })
  output$tim_ds_cdf_title <- renderText({
    markdown(paste("##### **CDF for ", Rtim_ds_dist_var(), " over selected distributions**"))
  })
### Exploratory
#### hists
##### fluoro
  output$tim_fluoro_hist <- renderPlot({
    ggplot(data_tim_fluoro, aes(x=.data[[Rtim_fluoro_hist_var()]], 
                                color = .data[[Rtim_fluoro_hist_color()]],
                                fill = .data[[Rtim_fluoro_hist_color()]]))+
      geom_histogram(bins = Rtim_fluoro_hist_bins())+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top"
      )
  })
##### height
  output$tim_height_hist <- renderPlot({
    ggplot(data_tim_height, aes(x=.data[[Rtim_height_hist_var()]], 
                                color = .data[[Rtim_height_hist_color()]],
                                fill = .data[[Rtim_height_hist_color()]]))+
      geom_histogram(bins = Rtim_height_hist_bins())+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top"
      )
  })
##### ds
  output$tim_ds_hist <- renderPlot({
    ggplot(data_tim_ds, aes(x=.data[[Rtim_ds_hist_var()]], 
                            color = .data[[Rtim_ds_hist_color()]],
                            fill = .data[[Rtim_ds_hist_color()]]))+
      geom_histogram(bins = Rtim_ds_hist_bins())+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold")
      )
  })
#### scatters
##### fluoro
  output$tim_fluoro_scatter <- renderPlot({
    gs <- ggplot(data=data_tim_fluoro, aes(x=.data[[Rtim_fluoro_scatter_x()]], y=.data[[Rtim_fluoro_scatter_y()]],
                                           color = .data[[Rtim_fluoro_scatter_col()]], shape = .data[[Rtim_fluoro_scatter_shape()]]))+
      geom_jitter(width=Rtim_fluoro_scatter_jit(), height=Rtim_fluoro_scatter_jit()*0.5, size = Rtim_fluoro_scatter_size())+
      ylab(gettext(Rtim_fluoro_scatter_y()))+
      xlab(gettext(Rtim_fluoro_scatter_x()))+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top"
        )
    if (Rtim_fluoro_scatter_x() %in% tim_fluoro_vars_d) {
      gs <- gs + scale_x_discrete(guide=guide_axis(check.overlap=TRUE))
    } else {
      gs <- gs + scale_x_continuous(guide=guide_axis(check.overlap=TRUE))
    }
    if (Rtim_fluoro_scatter_fwrap() == TRUE){
      gs <- gs + facet_wrap(~Treatment)
    }
    if (Rtim_fluoro_scatter_col() %in% tim_fluoro_vars_d) {
      gs <- gs + scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    } else {
      gs <- gs + scale_color_scico(begin=0.9, end=0.2, palette=Rpalette())
    }
    return(gs)
  })
##### height
  output$tim_height_scatter <- renderPlot({
    gs <- ggplot(data=data_tim_height, aes(x=.data[[Rtim_height_scatter_x()]], y=.data[[Rtim_height_scatter_y()]],
                                           color = .data[[Rtim_height_scatter_col()]], shape = .data[[Rtim_height_scatter_shape()]]))+
      geom_jitter(width=Rtim_height_scatter_jit(), height=Rtim_height_scatter_jit()*0.5, size = Rtim_height_scatter_size())+
      ylab(gettext(Rtim_height_scatter_y()))+
      xlab(gettext(Rtim_height_scatter_x()))+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top"
      )
    if (Rtim_height_scatter_x() %in% tim_height_vars_d) {
      gs <- gs + scale_x_discrete(guide=guide_axis(check.overlap=TRUE))
    } else {
      gs <- gs + scale_x_continuous(guide=guide_axis(check.overlap=TRUE))
    }
    if (Rtim_height_scatter_fwrap() == TRUE){
      gs <- gs + facet_wrap(~Treatment)
    }
    if (Rtim_height_scatter_col() %in% tim_height_vars_d) {
      gs <- gs + scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    } else {
      gs <- gs + scale_color_scico(begin=0.9, end=0.2, palette=Rpalette())
    }
    return(gs)
  })
##### ds
  output$tim_ds_scatter <- renderPlot({
    gs <- ggplot(data=data_tim_ds, aes(x=.data[[Rtim_ds_scatter_x()]], y=.data[[Rtim_ds_scatter_y()]],
                                           color = .data[[Rtim_ds_scatter_col()]], shape = .data[[Rtim_ds_scatter_shape()]]))+
      geom_jitter(width=Rtim_ds_scatter_jit(), height=Rtim_ds_scatter_jit()*0.5, size = Rtim_ds_scatter_size())+
      ylab(gettext(Rtim_ds_scatter_y()))+
      xlab(gettext(Rtim_ds_scatter_x()))+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top"
      )
    if (Rtim_ds_scatter_x() %in% tim_ds_vars_d) {
      gs <- gs + scale_x_discrete(guide=guide_axis(check.overlap=TRUE))
    } else {
      gs <- gs + scale_x_continuous(guide=guide_axis(check.overlap=TRUE))
    }
    if (Rtim_ds_scatter_fwrap() == TRUE){
      gs <- gs + facet_wrap(~Treatment)
    }
    if (Rtim_ds_scatter_col() %in% tim_ds_vars_d) {
      gs <- gs + scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    } else {
      gs <- gs + scale_color_scico(begin=0.9, end=0.2, palette=Rpalette())
    }
    return(gs)
  })
#### boxplots
##### fluoro box
  output$tim_fluoro_box <- renderPlot({
    ggplot(data=data_tim_fluoro, aes(x=.data[[Rtim_fluoro_box_x()]], y=.data[[Rtim_fluoro_box_y()]],
                                     color = .data[[Rtim_fluoro_box_x()]], fill = .data[[Rtim_fluoro_box_x()]]))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width = 0.4,  alpha = 0.8)+
      ylab(gettext(Rtim_fluoro_box_y()))+
      xlab(gettext(Rtim_fluoro_box_x()))+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = element_text(size=font_sizes[2], face= "bold")
      )
  })
##### height box
  output$tim_height_box <- renderPlot({
    ggplot(data=data_tim_height, aes(x=.data[[Rtim_height_box_x()]], y=.data[[Rtim_height_box_y()]],
                                     color = .data[[Rtim_height_box_x()]], fill = .data[[Rtim_height_box_x()]]))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width = 0.4,  alpha = 0.8)+
      ylab(gettext(Rtim_height_box_y()))+
      xlab(gettext(Rtim_height_box_x()))+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = element_text(size=font_sizes[2], face= "bold")
      )
  })
##### ds box
  output$tim_ds_box <- renderPlot({
    ggplot(data=data_tim_ds, aes(x=.data[[Rtim_ds_box_x()]], y=.data[[Rtim_ds_box_y()]],
                                     color = .data[[Rtim_ds_box_x()]], fill = .data[[Rtim_ds_box_x()]]))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width = 0.4,  alpha = 0.8)+
      ylab(gettext(Rtim_ds_box_y()))+
      xlab(gettext(Rtim_ds_box_x()))+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = element_text(size=font_sizes[2], face= "bold")
      )
  })
### Statistics
#### fluoro
##### gsw
  output$tim_gsw_mod_summary <- renderPrint({
    Rtim_gsw_mod_sum()
  })
  output$tim_gsw_mod_call <- renderPrint({
    Rtim_gsw_mod()$call
  })
  output$tim_gsw_aic <- renderText({
    round(AIC(Rtim_gsw_mod()), 0)
  })
  output$tim_gsw_p <- renderText({
    p_from_modsum(Rtim_gsw_mod_sum())
  })
  output$tim_gsw_r2 <- renderText({
    r1 <- round(Rtim_gsw_mod_sum()$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tim_gsw_letters <- renderPrint({
    cld(glht(Rtim_gsw_mod(), linfct = mcp(Treatment = "Tukey")))
  })
##### phips2
  output$tim_ps2_mod_summary <- renderPrint({
    Rtim_ps2_mod_sum()
  })
  output$tim_ps2_mod_call <- renderPrint({
    Rtim_ps2_mod()$call
  })
  output$tim_ps2_aic <- renderText({
    round(AIC(Rtim_ps2_mod()), 0)
  })
  output$tim_ps2_p <- renderText({
    p_from_modsum(Rtim_ps2_mod_sum())
  })
  output$tim_ps2_r2 <- renderText({
    r1 <- round(Rtim_ps2_mod_sum()$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tim_ps2_letters <- renderPrint({
    cld(glht(Rtim_ps2_mod(), linfct = mcp(Treatment = "Tukey")))
  })
##### PCA
  output$tim_fluoro_pca <- renderPlot({
    pca_plot(mod_data_tim_fluoro$Treatment, mod_data_tim_fluoro[,c(12:18)])+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      labs(title = "PCA for Fluorescence Variables")+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8)
      )
  })
  output$tim_fluoro_pca_summary <- renderPrint({
    summary(tim_pca)
  })
  output$tim_pcr_gsw_summary <- renderPrint({
    tim_pcr_gsw_sum
  })
  output$tim_pcr_gsw_aic <- renderText({
    round(AIC(tim_pcr_gsw), 0)
  })
  output$tim_pcr_gsw_p <- renderText({
    p_from_modsum(tim_pcr_gsw_sum)
  })
  output$tim_pcr_gsw_r2 <- renderText({
    r1 <- round(tim_pcr_gsw_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tim_pcr_gsw_letters <- renderPrint({
    cld(glht(tim_pcr_gsw, linfct = mcp(Treatment = "Tukey")))
  })
  output$tim_pcr_gsw_pred <- renderPlot({
    predict_plot(tim_pcr_gsw_pmod, tim_pcr_data, gsw, PC1, Treatment, 100, correction = "exponential")+
      ylim(0,1.5)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
  })
  output$tim_pcr_ps2_summary <- renderPrint({
    tim_pcr_ps2_sum
  })
  output$tim_pcr_ps2_aic <- renderText({
    round(AIC(tim_pcr_ps2), 0)
  })
  output$tim_pcr_ps2_p <- renderText({
    p_from_modsum(tim_pcr_ps2_sum)
  })
  output$tim_pcr_ps2_r2 <- renderText({
    r1 <- round(tim_pcr_ps2_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tim_pcr_ps2_letters <- renderPrint({
    cld(glht(tim_pcr_ps2, linfct = mcp(Treatment = "Tukey")))
  })
  output$tim_pcr_ps2_pred <- renderPlot({
    predict_plot(tim_pcr_ps2_pmod, tim_pcr_data, PhiPS2, PC1, Treatment, 100, correction = "logit")+
      ylim(0.5,0.8)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
  })
#### height stats
  output$tim_height_mod_summary <- renderPrint({
    tim_height_mod_sum
  })
  output$tim_height_mod_call <- renderPrint({
    tim_height_mod$call
  })
  output$tim_height_aic <- renderText({
    round(AIC(tim_height_mod), 0)
  })
  output$tim_height_p <- renderText({
    p_from_modsum(tim_height_mod_sum)
  })
  output$tim_height_r2 <- renderText({
    r1 <- round(tim_height_mod_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tim_height_letters <- renderPrint({
    tim_height_letters
  })
  output$tim_height_pred <- renderPlot({
    predict_plot(tim_height_mod, data_tim_height, Height, DaysFromGermination, Treatment, 100, correction = "exponential")+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
  })
#### ds stats
##### RS_Length
  output$tim_rs_length_mod_summary <- renderPrint({
    tim_rs_length_mod_sum
  })
  output$tim_rs_length_mod_call <- renderPrint({
    tim_rs_length_mod$call
  })
  output$tim_rs_length_aic <- renderText({
    round(AIC(tim_rs_length_mod), 0)
  })
  output$tim_rs_length_p <- renderText({
    p_from_modsum(tim_rs_length_mod_sum)
  })
  output$tim_rs_length_r2 <- renderText({
    r1 <- round(tim_rs_length_mod_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tim_rs_length_letters <- renderPrint({
    tim_rs_length_letters
  })
  output$tim_rs_length_annotated <- renderPlot({
    ggplot(data=data_tim_ds, aes(x=Treatment, y=RS_Length,
                                            color = Treatment, fill = Treatment))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab("Root:Shoot Length")+
      xlab("Treatment")+
      annotate("text", x=1:4, y=1.5, label = tim_rs_length_letters$mcletters$Letters, size=6)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = element_text(size=font_sizes[2], face= "bold")
      )
  })
##### RS_Mass
  output$tim_rs_mass_mod_summary <- renderPrint({
    tim_rs_mass_mod_sum
  })
  output$tim_rs_mass_mod_call <- renderPrint({
    tim_rs_mass_mod$call
  })
  output$tim_rs_mass_aic <- renderText({
    round(AIC(tim_rs_mass_mod), 0)
  })
  output$tim_rs_mass_p <- renderText({
    p_from_modsum(tim_rs_mass_mod_sum)
  })
  output$tim_rs_mass_r2 <- renderText({
    r1 <- round(tim_rs_mass_mod_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tim_rs_mass_letters <- renderPrint({
    tim_rs_mass_letters
  })
  output$tim_rs_mass_annotated <- renderPlot({
    ggplot(data=data_tim_ds, aes(x=Treatment, y=RS_Mass,
                                 color = Treatment, fill = Treatment))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab("Root:Shoot Mass")+
      xlab("Treatment")+
      annotate("text", x=1:4, y=.8, label = tim_rs_mass_letters$mcletters$Letters, size=6)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = element_text(size=font_sizes[2], face= "bold")
      )
  })
### Data
  output$tim_fluoro_DT <- renderDataTable({
    data_tim_fluoro
  })
  output$tim_height_DT <- renderDataTable({
    data_tim_height
  })
  output$tim_ds_DT <- renderDataTable({
    data_tim_ds
  })
##### TIL OUTPUTS #####
### Distributions
#### Fluorescence
  # ks
  output$til_fluoro_KS <- renderPrint({
    if (Rtil_fluoro_dist_var() == "gsw") {
      multiKS_cont(na.omit(data_til_fluoro[[Rtil_fluoro_dist_var()]]), Rtil_fluoro_dists())
    } else {
      multiKS_cont(data_til_fluoro[[Rtil_fluoro_dist_var()]], Rtil_fluoro_dists())
    }
  })
  # pdf
  output$til_fluoro_pdf <- renderPlot({
    if (Rtil_fluoro_dist_var() == "gsw") {
      multiPDF_plot(na.omit(data_til_fluoro[[Rtil_fluoro_dist_var()]]), Rtil_fluoro_len(), Rtil_fluoro_dists(), palette = Rpalette(), var_name = gettext(Rtil_fluoro_dist_var()))
    } else {
      multiPDF_plot(data_til_fluoro[[Rtil_fluoro_dist_var()]], Rtil_fluoro_len(), Rtil_fluoro_dists(), palette = Rpalette(), var_name = gettext(Rtil_fluoro_dist_var()))
    }
  })
  # cdf
  output$til_fluoro_cdf <- renderPlot({
    if (Rtil_fluoro_dist_var() == "gsw") {
      multiCDF_plot(na.omit(data_til_fluoro[[Rtil_fluoro_dist_var()]]), Rtil_fluoro_len(), Rtil_fluoro_dists(), palette = Rpalette(), var_name = gettext(Rtil_fluoro_dist_var()))
    } else {
      multiCDF_plot(data_til_fluoro[[Rtil_fluoro_dist_var()]], Rtil_fluoro_len(), Rtil_fluoro_dists(), palette = Rpalette(), var_name = gettext(Rtil_fluoro_dist_var()))
    }
  })
#### Fruit
  # ks
  output$til_fruit_KS <- renderPrint({
    if (Rtil_fruit_dist_var() %in% fruit_lab_vars) {
      multiKS_cont(na.omit(data_til_fruit[[Rtil_fruit_dist_var()]]), Rtil_fruit_dists())
    } else {
      multiKS_cont(data_til_fruit[[Rtil_fruit_dist_var()]], Rtil_fruit_dists())
    }
  })
  # pdf
  output$til_fruit_pdf <- renderPlot({
    if (Rtil_fruit_dist_var() %in% fruit_lab_vars) {
      multiPDF_plot(na.omit(data_til_fruit[[Rtil_fruit_dist_var()]]), Rtil_fruit_len(), Rtil_fruit_dists(), palette = Rpalette(), var_name = gettext(Rtil_fruit_dist_var()))
    } else {
      multiPDF_plot(data_til_fruit[[Rtil_fruit_dist_var()]], Rtil_fruit_len(), Rtil_fruit_dists(), palette = Rpalette(), var_name = gettext(Rtil_fruit_dist_var()))
    }
  })
  # cdf
  output$til_fruit_cdf <- renderPlot({
    if (Rtil_fruit_dist_var() %in% fruit_lab_vars) {
      multiCDF_plot(na.omit(data_til_fruit[[Rtil_fruit_dist_var()]]), Rtil_fruit_len(), Rtil_fruit_dists(), palette = Rpalette(), var_name = gettext(Rtil_fruit_dist_var()))
    } else {
      multiCDF_plot(data_til_fruit[[Rtil_fruit_dist_var()]], Rtil_fruit_len(), Rtil_fruit_dists(), palette = Rpalette(), var_name = gettext(Rtil_fruit_dist_var()))
    }
  })
### Exploratory
  # hists
  output$til_fluoro_hist <- renderPlot({
    gh <- ggplot(data_til_fluoro, aes(x=.data[[Rtil_fluoro_hist_var()]], color = .data[[Rtil_fluoro_hist_color()]],
                                      fill = .data[[Rtil_fluoro_hist_color()]]))+
      geom_histogram(bins = Rtil_fluoro_hist_bins())+
      theme_bw() +
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
      )
    gh <- gh + scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    gh <- gh + scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    return(gh)
  })
  output$til_fruit_hist <- renderPlot({
    ph <- ggplot(data_til_fruit, aes(x=.data[[Rtil_fruit_hist_var()]], color = .data[[Rtil_fruit_hist_color()]],
                                     fill = .data[[Rtil_fruit_hist_color()]]))+
      geom_histogram(bins = Rtil_fruit_hist_bins()) + 
      theme_bw() +
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
      )
    ph <- ph + scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    ph <- ph + scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    return(ph)
  })
  # scatters
  output$til_fluoro_scatter <- renderPlot({
    gs <- ggplot(data=data_til_fluoro, aes(x=.data[[Rtil_fluoro_scatter_x()]], y=.data[[Rtil_fluoro_scatter_y()]],
                                           color = .data[[Rtil_fluoro_scatter_col()]], shape = .data[[Rtil_fluoro_scatter_shape()]]))+
      geom_jitter(width=Rtil_fluoro_scatter_jit(), height=Rtil_fluoro_scatter_jit()*0.5, size = Rtil_fluoro_scatter_size())+
      ylab(gettext(Rtil_fluoro_scatter_y()))+
      xlab(gettext(Rtil_fluoro_scatter_x()))+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top"
      )
    if (Rtil_fluoro_scatter_x() %in% til_fluoro_vars_d) {
      gs <- gs + scale_x_discrete(guide=guide_axis(check.overlap=TRUE))
    } else {
      gs <- gs + scale_x_continuous(guide=guide_axis(check.overlap=TRUE))
    }
    if (Rtil_fluoro_scatter_fwrap() == TRUE){
      gs <- gs + facet_wrap(~Treatment)
    }
    if (Rtil_fluoro_scatter_col() %in% til_fluoro_vars_d) {
      gs <- gs + scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    } else {
      gs <- gs + scale_color_scico(begin=0.9, end=0.2, palette=Rpalette())
    }
    return(gs)
  })
  output$til_fruit_scatter <- renderPlot({
    ps <- ggplot(data=data_til_fruit, aes(x=.data[[Rtil_fruit_scatter_x()]], y=.data[[Rtil_fruit_scatter_y()]],
                                          color = .data[[Rtil_fruit_scatter_col()]], shape = .data[[Rtil_fruit_scatter_shape()]]))+
      geom_jitter(width=Rtil_fruit_scatter_jit(), height=Rtil_fruit_scatter_jit()*0.5, size = Rtil_fruit_scatter_size())+
      ylab(gettext(Rtil_fruit_scatter_y()))+
      xlab(gettext(Rtil_fruit_scatter_x()))+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position = "bottom",
        legend.title.position = "top"
      )
    if (Rtil_fruit_scatter_x() %in% til_fruit_vars_d) {
      ps <- ps + scale_x_discrete(guide=guide_axis(check.overlap=TRUE))
    } else {
      ps <- ps + scale_x_continuous(guide=guide_axis(check.overlap=TRUE))
    }
    if (Rtil_fruit_scatter_fwrap() == TRUE){
      ps <- ps + facet_wrap(~Treatment)
    }
    if (Rtil_fruit_scatter_col() %in% til_fruit_vars_d) {
      ps <- ps + scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    } else {
      ps <- ps + scale_color_scico(begin=0.9, end=0.2, palette=Rpalette())
    }
    return(ps)
  })
  # boxplots
  ## fluoro box
  output$til_fluoro_box <- renderPlot({
    ggplot(data=data_til_fluoro, aes(x=.data[[Rtil_fluoro_box_x()]], y=.data[[Rtil_fluoro_box_y()]],
                                     color = .data[[Rtil_fluoro_box_x()]], fill = .data[[Rtil_fluoro_box_x()]]))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width = 0.4,  alpha = 0.8)+
      ylab(gettext(Rtil_fluoro_box_y()))+
      xlab(gettext(Rtil_fluoro_box_x()))+
      scale_color_scico_d(begin=0.9, end=0.1, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.1, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position = "bottom",
        legend.title.position = "top"
      )
  })
  ## fruit box
  output$til_fruit_box <- renderPlot({
    ggplot(data=data_til_fruit, aes(x=.data[[Rtil_fruit_box_x()]], y=.data[[Rtil_fruit_box_y()]],
                                    color = .data[[Rtil_fruit_box_x()]], fill = .data[[Rtil_fruit_box_x()]]))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab(gettext(Rtil_fruit_box_y()))+
      xlab(gettext(Rtil_fruit_box_x()))+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position = "bottom",
        legend.title.position = "top"
      )
  })
  ## fruit sum box
  output$til_fruit_sum_box <- renderPlot({
    ggplot(data=data_til_fruit_summary, aes(x=.data[[Rtil_fruit_sum_box_x()]], y=.data[[Rtil_fruit_sum_box_y()]],
                                            color = .data[[Rtil_fruit_sum_box_x()]], fill = .data[[Rtil_fruit_sum_box_x()]]))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab(gettext(Rtil_fruit_sum_box_y()))+
      xlab(gettext(Rtil_fruit_sum_box_x()))+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position = "bottom",
        legend.title.position = "top"
      )
  })
  ### Statistics
  #### fluoro
  ##### gsw
  output$til_gsw_mod_summary <- renderPrint({
    Rtil_gsw_mod_sum()
  })
  output$til_gsw_mod_call <- renderPrint({
    Rtil_gsw_mod()$call
  })
  output$til_gsw_aic <- renderText({
    round(AIC(Rtil_gsw_mod()), 0)
  })
  output$til_gsw_p <- renderText({
    p_from_modsum(Rtil_gsw_mod_sum())
  })
  output$til_gsw_r2 <- renderText({
    r1 <- round(Rtil_gsw_mod_sum()$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$til_gsw_letters <- renderPrint({
    cld(glht(Rtil_gsw_mod(), linfct = mcp(Treatment = "Tukey")))
  })
  ##### phips2
  output$til_ps2_mod_summary <- renderPrint({
    Rtil_ps2_mod_sum()
  })
  output$til_ps2_mod_call <- renderPrint({
    Rtil_ps2_mod()$call
  })
  output$til_ps2_aic <- renderText({
    round(AIC(Rtil_ps2_mod()), 0)
  })
  output$til_ps2_p <- renderText({
    p_from_modsum(Rtil_ps2_mod_sum())
  })
  output$til_ps2_r2 <- renderText({
    r1 <- round(Rtil_ps2_mod_sum()$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$til_ps2_letters <- renderPrint({
    cld(glht(Rtil_ps2_mod(), linfct = mcp(Treatment = "Tukey")))
  })
  ##### PCA
  output$til_fluoro_pca <- renderPlot({
    pca_plot(mod_data_til_fluoro$Treatment, mod_data_til_fluoro[,c(13:19)])+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      labs(title = "PCA for Fluorescence Variables")+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8)
      )
  })
  output$til_fluoro_pca_summary <- renderPrint({
    summary(til_pca)
  })
  output$til_pcr_gsw_summary <- renderPrint({
    til_pcr_gsw_sum
  })
  output$til_pcr_gsw_aic <- renderText({
    round(AIC(til_pcr_gsw), 0)
  })
  output$til_pcr_gsw_p <- renderText({
    p_from_modsum(til_pcr_gsw_sum)
  })
  output$til_pcr_gsw_r2 <- renderText({
    r1 <- round(til_pcr_gsw_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$til_pcr_gsw_letters <- renderPrint({
    cld(glht(til_pcr_gsw, linfct = mcp(Treatment = "Tukey")))
  })
  output$til_pcr_gsw_pred <- renderPlot({
    predict_plot(til_pcr_gsw_pmod, til_pcr_data, gsw, PC1, Treatment, 100, correction = "exponential")+
      ylim(0,1.5)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
  })
  output$til_pcr_ps2_summary <- renderPrint({
    til_pcr_ps2_sum
  })
  output$til_pcr_ps2_aic <- renderText({
    round(AIC(til_pcr_ps2), 0)
  })
  output$til_pcr_ps2_p <- renderText({
    p_from_modsum(til_pcr_ps2_sum)
  })
  output$til_pcr_ps2_r2 <- renderText({
    r1 <- round(til_pcr_ps2_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$til_pcr_ps2_letters <- renderPrint({
    cld(glht(til_pcr_ps2, linfct = mcp(Treatment = "Tukey")))
  })
  output$til_pcr_ps2_pred <- renderPlot({
    predict_plot(til_pcr_ps2_pmod, til_pcr_data, PhiPS2, PC1, Treatment, 100, correction = "logit")+
      ylim(0.5,0.8)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
  })
  #### fruit
  ##### mass
  output$til_mass_mod_summary <- renderPrint({
    til_mass_mod_sum
  })
  output$til_mass_mod_call <- renderPrint({
    til_mass_mod$call
  })
  output$til_mass_aic <- renderText({
    round(AIC(til_mass_mod), 0)
  })
  output$til_mass_p <- renderText({
    p_from_modsum(til_mass_mod_sum)
  })
  output$til_mass_r2 <- renderText({
    r1 <- round(til_mass_mod_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$til_mass_letters <- renderPrint({
    til_mass_letters
  })
  output$til_mass_annotated <- renderPlot({
    ggplot(data=data_til_fruit_summary, aes(x=Treatment, y=Mass_mean,
                                            color = Treatment, fill = Treatment))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab("Mean Mass (g)")+
      xlab("Treatment")+
      annotate("text", x=1:4, y=120, label = til_mass_letters$mcletters$Letters, size=6)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = element_text(size=font_sizes[2], face= "bold")
      )
  })
  ##### sugar
  output$til_sug_mod_summary <- renderPrint({
    til_sug_mod_sum
  })
  output$til_sug_mod_call <- renderPrint({
    til_sug_mod$call
  })
  output$til_sug_aic <- renderText({
    round(AIC(til_sug_mod), 0)
  })
  output$til_sug_p <- renderText({
    p_from_modsum(til_sug_mod_sum)
  })
  output$til_sug_r2 <- renderText({
    r1 <- round(til_sug_mod_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$til_sug_letters <- renderPrint({
    til_sug_letters
  })
  output$til_sug_annotated <- renderPlot({
    predict_plot(til_sug_mod, data_til_fruit_summary, pSugar_mean, Mass_mean, Treatment, 100, correction = "logit")+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
  })
  ##### ber
  output$til_ber_mod_summary <- renderPrint({
    til_ber_mod_sum
  })
  output$til_ber_mod_call <- renderPrint({
    til_ber_mod$call
  })
  output$til_ber_aic <- renderText({
    round(AIC(til_ber_mod), 0)
  })
  output$til_ber_p <- renderText({
    p_from_modsum(til_ber_mod_sum)
  })
  output$til_ber_r2 <- renderText({
    r1 <- round(til_ber_mod_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$til_ber_letters <- renderPrint({
    til_ber_letters
  })
  output$til_ber_annotated <- renderPlot({
    ggplot(data=data_til_fruit_summary, aes(x=Treatment, y=pBER*100,
                                            color = Treatment, fill = Treatment))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab("Mean Blossom End-Rot (%)")+
      xlab("Treatment")+
      annotate("text", x=1:4, y=75, label = til_ber_letters$mcletters$Letters, size=6)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = element_text(size=font_sizes[2], face= "bold")
      )
  })
  ##### count
  output$til_fc_mod_summary <- renderPrint({
    til_fc_mod_sum
  })
  output$til_fc_mod_call <- renderPrint({
    til_fc_mod$call
  })
  output$til_fc_aic <- renderText({
    round(AIC(til_fc_mod), 0)
  })
  output$til_fc_r2 <- renderPrint({
    r.squaredGLMM(til_fc_mod)
  })
  output$til_fc_letters <- renderPrint({
    til_fc_letters
  })
  output$til_fc_annotated <- renderPlot({
    ggplot(data=data_til_fruit_summary, aes(x=Treatment, y=Fruit_sum,
                                            color = Treatment, fill = Treatment))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab("Fruit Count")+
      xlab("Treatment")+
      annotate("text", x=1:4, y=100, label = til_fc_letters$mcletters$Letters, size=6)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = element_text(size=font_sizes[2], face= "bold")
      )
  })
  ### Data
  output$til_fluoro_DT <- renderDataTable({
    data_til_fluoro
  })
  output$til_fruit_DT <- renderDataTable({
    data_til_fruit
  })
  output$til_fruit_sum_DT <- renderDataTable({
    data_til_fruit_summary
  })
##### TIT OUTPUTS #####
### Distributions
#### Fluorescence
  # ks
  output$tit_fluoro_KS <- renderPrint({
    if (Rtit_fluoro_dist_var() == "gsw") {
      multiKS_cont(na.omit(data_tit_fluoro[[Rtit_fluoro_dist_var()]]), Rtit_fluoro_dists())
    } else {
      multiKS_cont(data_tit_fluoro[[Rtit_fluoro_dist_var()]], Rtit_fluoro_dists())
    }
  })
  # pdf
  output$tit_fluoro_pdf <- renderPlot({
    if (Rtit_fluoro_dist_var() == "gsw") {
      multiPDF_plot(na.omit(data_tit_fluoro[[Rtit_fluoro_dist_var()]]), Rtit_fluoro_len(), Rtit_fluoro_dists(), palette = Rpalette(), var_name = gettext(Rtit_fluoro_dist_var()))
    } else {
      multiPDF_plot(data_tit_fluoro[[Rtit_fluoro_dist_var()]], Rtit_fluoro_len(), Rtit_fluoro_dists(), palette = Rpalette(), var_name = gettext(Rtit_fluoro_dist_var()))
    }
    })
  # cdf
  output$tit_fluoro_cdf <- renderPlot({
    if (Rtit_fluoro_dist_var() == "gsw") {
      multiCDF_plot(na.omit(data_tit_fluoro[[Rtit_fluoro_dist_var()]]), Rtit_fluoro_len(), Rtit_fluoro_dists(), palette = Rpalette(), var_name = gettext(Rtit_fluoro_dist_var()))
    } else {
      multiCDF_plot(data_tit_fluoro[[Rtit_fluoro_dist_var()]], Rtit_fluoro_len(), Rtit_fluoro_dists(), palette = Rpalette(), var_name = gettext(Rtit_fluoro_dist_var()))
    }
  })
#### Fruit
  # ks
  output$tit_fruit_KS <- renderPrint({
    if (Rtit_fruit_dist_var() %in% fruit_lab_vars) {
      multiKS_cont(na.omit(data_tit_fruit[[Rtit_fruit_dist_var()]]), Rtit_fruit_dists())
    } else {
      multiKS_cont(data_tit_fruit[[Rtit_fruit_dist_var()]], Rtit_fruit_dists())
    }
  })
  # pdf
  output$tit_fruit_pdf <- renderPlot({
    if (Rtit_fruit_dist_var() %in% fruit_lab_vars) {
      multiPDF_plot(na.omit(data_tit_fruit[[Rtit_fruit_dist_var()]]), Rtit_fruit_len(), Rtit_fruit_dists(), palette = Rpalette(), var_name = gettext(Rtit_fruit_dist_var()))
    } else {
      multiPDF_plot(data_tit_fruit[[Rtit_fruit_dist_var()]], Rtit_fruit_len(), Rtit_fruit_dists(), palette = Rpalette(), var_name = gettext(Rtit_fruit_dist_var()))
    }
  })
  # cdf
  output$tit_fruit_cdf <- renderPlot({
    if (Rtit_fruit_dist_var() %in% fruit_lab_vars) {
      multiCDF_plot(na.omit(data_tit_fruit[[Rtit_fruit_dist_var()]]), Rtit_fruit_len(), Rtit_fruit_dists(), palette = Rpalette(), var_name = gettext(Rtit_fruit_dist_var()))
    } else {
      multiCDF_plot(data_tit_fruit[[Rtit_fruit_dist_var()]], Rtit_fruit_len(), Rtit_fruit_dists(), palette = Rpalette(), var_name = gettext(Rtit_fruit_dist_var()))
    }
  })
### Exploratory
# hists
  output$tit_fluoro_hist <- renderPlot({
    gh <- ggplot(data_tit_fluoro, aes(x=.data[[Rtit_fluoro_hist_var()]], color = .data[[Rtit_fluoro_hist_color()]],
                               fill = .data[[Rtit_fluoro_hist_color()]]))+
      geom_histogram(bins = Rtit_fluoro_hist_bins())+
      theme_bw() +
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold")
      )
    gh <- gh + scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    gh <- gh + scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    return(gh)
  })
  output$tit_fruit_hist <- renderPlot({
    ph <- ggplot(data_tit_fruit, aes(x=.data[[Rtit_fruit_hist_var()]], color = .data[[Rtit_fruit_hist_color()]],
                               fill = .data[[Rtit_fruit_hist_color()]]))+
      geom_histogram(bins = Rtit_fruit_hist_bins()) + 
      theme_bw() +
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold")
      )
    ph <- ph + scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    ph <- ph + scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    return(ph)
  })
# scatters
  output$tit_fluoro_scatter <- renderPlot({
    gs <- ggplot(data=data_tit_fluoro, aes(x=.data[[Rtit_fluoro_scatter_x()]], y=.data[[Rtit_fluoro_scatter_y()]],
                                    color = .data[[Rtit_fluoro_scatter_col()]], shape = .data[[Rtit_fluoro_scatter_shape()]]))+
      geom_jitter(width=Rtit_fluoro_scatter_jit(), height=Rtit_fluoro_scatter_jit()*0.5, size = Rtit_fluoro_scatter_size())+
      ylab(gettext(Rtit_fluoro_scatter_y()))+
      xlab(gettext(Rtit_fluoro_scatter_x()))+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position="bottom",
        legend.title.position = "top"
      )
    if (Rtit_fluoro_scatter_x() %in% tit_fluoro_vars_d) {
      gs <- gs + scale_x_discrete(guide=guide_axis(check.overlap=TRUE))
    } else {
      gs <- gs + scale_x_continuous(guide=guide_axis(check.overlap=TRUE))
    }
    if (Rtit_fluoro_scatter_fwrap() == TRUE){
      gs <- gs + facet_wrap(~Treatment)
    }
    if (Rtit_fluoro_scatter_col() %in% tit_fluoro_vars_d) {
      gs <- gs + scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    } else {
      gs <- gs + scale_color_scico(begin=0.9, end=0.2, palette=Rpalette())
    }
    return(gs)
  })
  output$tit_fruit_scatter <- renderPlot({
    ps <- ggplot(data=data_tit_fruit, aes(x=.data[[Rtit_fruit_scatter_x()]], y=.data[[Rtit_fruit_scatter_y()]],
                                    color = .data[[Rtit_fruit_scatter_col()]], shape = .data[[Rtit_fruit_scatter_shape()]]))+
      geom_jitter(width=Rtit_fruit_scatter_jit(), height=Rtit_fruit_scatter_jit()*0.5, size = Rtit_fruit_scatter_size())+
      ylab(gettext(Rtit_fruit_scatter_y()))+
      xlab(gettext(Rtit_fruit_scatter_x()))+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position = "bottom",
        legend.title.position = "top"
      )
    if (Rtit_fruit_scatter_x() %in% tit_fruit_vars_d) {
      ps <- ps + scale_x_discrete(guide=guide_axis(check.overlap=TRUE))
    } else {
      ps <- ps + scale_x_continuous(guide=guide_axis(check.overlap=TRUE))
    }
    if (Rtit_fruit_scatter_fwrap() == TRUE){
      ps <- ps + facet_wrap(~Treatment)
    }
    if (Rtit_fruit_scatter_col() %in% tit_fruit_vars_d) {
      ps <- ps + scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())
    } else {
      ps <- ps + scale_color_scico(begin=0.9, end=0.2, palette=Rpalette())
    }
    return(ps)
  })
# boxplots
  ## fluoro box
  output$tit_fluoro_box <- renderPlot({
    ggplot(data=data_tit_fluoro, aes(x=.data[[Rtit_fluoro_box_x()]], y=.data[[Rtit_fluoro_box_y()]],
                                          color = .data[[Rtit_fluoro_box_x()]], fill = .data[[Rtit_fluoro_box_x()]]))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width = 0.4,  alpha = 0.8)+
      ylab(gettext(Rtit_fluoro_box_y()))+
      xlab(gettext(Rtit_fluoro_box_x()))+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position = "bottom",
        legend.title.position = "top"
      )
  })
  ## fruit box
  output$tit_fruit_box <- renderPlot({
    ggplot(data=data_tit_fruit, aes(x=.data[[Rtit_fruit_box_x()]], y=.data[[Rtit_fruit_box_y()]],
                                     color = .data[[Rtit_fruit_box_x()]], fill = .data[[Rtit_fruit_box_x()]]))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab(gettext(Rtit_fruit_box_y()))+
      xlab(gettext(Rtit_fruit_box_x()))+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position = "bottom",
        legend.title.position = "top"
      )
  })
  ## fruit sum box
  output$tit_fruit_sum_box <- renderPlot({
    ggplot(data=data_tit_fruit_summary, aes(x=.data[[Rtit_fruit_sum_box_x()]], y=.data[[Rtit_fruit_sum_box_y()]],
                                    color = .data[[Rtit_fruit_sum_box_x()]], fill = .data[[Rtit_fruit_sum_box_x()]]))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab(gettext(Rtit_fruit_sum_box_y()))+
      xlab(gettext(Rtit_fruit_sum_box_x()))+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
        legend.position = "bottom",
        legend.title.position = "top"
      )
  })
### Statistics
#### fluoro
##### gsw
  output$tit_gsw_mod_summary <- renderPrint({
    Rtit_gsw_mod_sum()
  })
  output$tit_gsw_mod_call <- renderPrint({
    Rtit_gsw_mod()$call
  })
  output$tit_gsw_aic <- renderText({
    round(AIC(Rtit_gsw_mod()), 0)
  })
  output$tit_gsw_p <- renderText({
    p_from_modsum(Rtit_gsw_mod_sum())
  })
  output$tit_gsw_r2 <- renderText({
    r1 <- round(Rtit_gsw_mod_sum()$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tit_gsw_letters <- renderPrint({
    cld(glht(Rtit_gsw_mod(), linfct = mcp(Treatment = "Tukey")))
  })
##### phips2
  output$tit_ps2_mod_summary <- renderPrint({
    Rtit_ps2_mod_sum()
  })
  output$tit_ps2_mod_call <- renderPrint({
    Rtit_ps2_mod()$call
  })
  output$tit_ps2_aic <- renderText({
    round(AIC(Rtit_ps2_mod()), 0)
  })
  output$tit_ps2_p <- renderText({
    p_from_modsum(Rtit_ps2_mod_sum())
  })
  output$tit_ps2_r2 <- renderText({
    r1 <- round(Rtit_ps2_mod_sum()$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tit_ps2_letters <- renderPrint({
    cld(glht(Rtit_ps2_mod(), linfct = mcp(Treatment = "Tukey")))
  })
##### PCA
  output$tit_fluoro_pca <- renderPlot({
    pca_plot(mod_data_tit_fluoro$Treatment, mod_data_tit_fluoro[,c(13:19)])+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      labs(title = "PCA for Fluorescence Variables")+
    theme(
      text = element_text(size=font_sizes[3]),
      axis.title = element_text(size=font_sizes[2], face= "bold"),
      title = element_text(size=font_sizes[1], face="bold", lineheight = .8)
    )
  })
  output$tit_fluoro_pca_summary <- renderPrint({
    summary(tit_pca)
  })
  output$tit_pcr_gsw_summary <- renderPrint({
    tit_pcr_gsw_sum
  })
  output$tit_pcr_gsw_aic <- renderText({
    round(AIC(tit_pcr_gsw), 0)
  })
  output$tit_pcr_gsw_p <- renderText({
    p_from_modsum(tit_pcr_gsw_sum)
  })
  output$tit_pcr_gsw_r2 <- renderText({
    r1 <- round(tit_pcr_gsw_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tit_pcr_gsw_letters <- renderPrint({
    cld(glht(tit_pcr_gsw, linfct = mcp(Treatment = "Tukey")))
  })
  output$tit_pcr_gsw_pred <- renderPlot({
    predict_plot(tit_pcr_gsw_pmod, tit_pcr_data, gsw, PC1, Treatment, 100, correction = "exponential")+
      ylim(0,1.5)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
  })
  output$tit_pcr_ps2_summary <- renderPrint({
    tit_pcr_ps2_sum
  })
  output$tit_pcr_ps2_aic <- renderText({
    round(AIC(tit_pcr_ps2), 0)
  })
  output$tit_pcr_ps2_p <- renderText({
    p_from_modsum(tit_pcr_ps2_sum)
  })
  output$tit_pcr_ps2_r2 <- renderText({
    r1 <- round(tit_pcr_ps2_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tit_pcr_ps2_letters <- renderPrint({
    cld(glht(tit_pcr_ps2, linfct = mcp(Treatment = "Tukey")))
  })
  output$tit_pcr_ps2_pred <- renderPlot({
    predict_plot(tit_pcr_ps2_pmod, tit_pcr_data, PhiPS2, PC1, Treatment, 100, correction = "logit")+
      ylim(0.5,0.8)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
  })
#### fruit
##### mass
  output$tit_mass_mod_summary <- renderPrint({
    tit_mass_mod_sum
  })
  output$tit_mass_mod_call <- renderPrint({
    tit_mass_mod$call
  })
  output$tit_mass_aic <- renderText({
    round(AIC(tit_mass_mod), 0)
  })
  output$tit_mass_p <- renderText({
    p_from_modsum(tit_mass_mod_sum)
  })
  output$tit_mass_r2 <- renderText({
    r1 <- round(tit_mass_mod_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tit_mass_letters <- renderPrint({
    tit_mass_letters
  })
  output$tit_mass_annotated <- renderPlot({
    ggplot(data=data_tit_fruit_summary, aes(x=Treatment, y=Mass_mean,
                                    color = Treatment, fill = Treatment))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab("Mean Mass (g)")+
      xlab("Treatment")+
      annotate("text", x=1:4, y=200, label = tit_mass_letters$mcletters$Letters, size=6)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = element_text(size=font_sizes[2], face= "bold")
      )
  })
##### sugar
  output$tit_sug_mod_summary <- renderPrint({
    tit_sug_mod_sum
  })
  output$tit_sug_mod_call <- renderPrint({
    tit_sug_mod$call
  })
  output$tit_sug_aic <- renderText({
    round(AIC(tit_sug_mod), 0)
  })
  output$tit_sug_p <- renderText({
    p_from_modsum(tit_sug_mod_sum)
  })
  output$tit_sug_r2 <- renderText({
    r1 <- round(tit_sug_mod_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tit_sug_letters <- renderPrint({
    tit_sug_letters
  })
  output$tit_sug_annotated <- renderPlot({
    predict_plot(tit_sug_mod, data_tit_fruit_summary, pSugar_mean, Mass_mean, Treatment, 100, correction = "logit")+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())
  })
##### ber
  output$tit_ber_mod_summary <- renderPrint({
    tit_ber_mod_sum
  })
  output$tit_ber_mod_call <- renderPrint({
    tit_ber_mod$call
  })
  output$tit_ber_aic <- renderText({
    round(AIC(tit_ber_mod), 0)
  })
  output$tit_ber_p <- renderText({
    p_from_modsum(tit_ber_mod_sum)
  })
  output$tit_ber_r2 <- renderText({
    r1 <- round(tit_ber_mod_sum$adj.r.squared, 4)
    r2 <- r1 *100
    return(paste0(r1, "  (", r2, "%)"))
  })
  output$tit_ber_letters <- renderPrint({
    tit_ber_letters
  })
  output$tit_ber_annotated <- renderPlot({
    ggplot(data=data_tit_fruit_summary, aes(x=Treatment, y=pBER*100,
                                            color = Treatment, fill = Treatment))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab("Mean Blossom End-Rot (%)")+
      xlab("Treatment")+
      annotate("text", x=1:4, y=22, label = tit_ber_letters$mcletters$Letters, size=6)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = element_text(size=font_sizes[2], face= "bold")
      )
  })
##### count
  output$tit_fc_mod_summary <- renderPrint({
    tit_fc_mod_sum
  })
  output$tit_fc_mod_call <- renderPrint({
    tit_fc_mod$call
  })
  output$tit_fc_aic <- renderText({
    round(AIC(tit_fc_mod), 0)
  })
  output$tit_fc_r2 <- renderPrint({
    r.squaredGLMM(tit_fc_mod)
  })
  output$tit_fc_letters <- renderPrint({
    tit_fc_letters
  })
  output$tit_fc_annotated <- renderPlot({
    ggplot(data=data_tit_fruit_summary, aes(x=Treatment, y=Fruit_sum,
                                            color = Treatment, fill = Treatment))+
      geom_jitter(width = 0.1, height = 0)+
      geom_boxplot(width=0.4, alpha = 0.8)+
      ylab("Fruit Count")+
      xlab("Treatment")+
      annotate("text", x=1:4, y=30, label = tit_fc_letters$mcletters$Letters, size=6)+
      scale_color_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      scale_fill_scico_d(begin=0.9, end=0.2, palette=Rpalette())+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3]),
        axis.title = element_text(size=font_sizes[2], face= "bold"),
        title = element_text(size=font_sizes[1], face="bold", lineheight = .8),
        legend.position="bottom",
        legend.title.position = "top",
        legend.title = element_text(size=font_sizes[2], face= "bold")
      )
  })
### Data
  output$tit_fluoro_DT <- renderDataTable({
    data_tit_fluoro
  })
  output$tit_fruit_DT <- renderDataTable({
    data_tit_fruit
  })
  output$tit_fruit_sum_DT <- renderDataTable({
    data_tit_fruit_summary
  })
}

# run it!
shinyApp(ui = ui, server = server)