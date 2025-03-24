##### SETUP #####
## this code runs on the client at execution
# load packages
library(shiny)
library(ggplot2)
library(showtext)
library(scico)
library(bslib)
library(bsicons)
library(MASS)
library(MuMIn)
library(lmerTest)

# graphics
p_palettes <- scico_palette_names()
font_sizes <- c(20,16,14)
font_add_google("Open Sans", family = "open")
font_add_google("Montserrat", family = "mont")
showtext_auto()

# distributions
## currently only continuous distributions are supported
dists <- c("normal", "lognormal", "gamma", "exponential")

# load data
load("data_gsw.RData")
load("data_fruit.RData")
load("data_ps2.RData")

## scale variables for later use in modeling
## scaling is important so they have equal "weight" in the model
gsw_scaled_vars <- data.frame(apply(data_gsw[,c(10:14)], 2, scale))
ps2_scaled_vars <- data.frame(apply(data_ps2[,c(11:15)], 2, scale))
gsw_mod_data <- cbind(data_gsw[,c(1,5,6,15)], gsw_scaled_vars)
ps2_mod_data <- cbind(data_ps2[,c(1,5,6,10,16)], ps2_scaled_vars)
mod_var_names <- c("Date", "DaysFromGermination", "AmbientHumidity", "AmbientPressure", "AmbientTemperature", "AmbientLight", "LeafTemperature")

## preload vars
### kind of a funky way of doing this, but it makes it REALLY easy to check if a variable
### is continuous or discrete later with [if (var %in% vars_d)]
gsw_vars <- c("DaysFromGermination", "AmbientHumidity", "AmbientPressure", "AmbientTemperature", "AmbientLight", "LeafTemperature", "gsw")
gsw_vars_d <- c("Treatment", "Transplantation", "Germination", "Row", "Pot", "Plant", "Time", "Date")
all_gsw_vars <- c(gsw_vars_d, gsw_vars)
ps2_vars <- c("DaysFromGermination", "AmbientHumidity", "AmbientPressure", "AmbientTemperature", "AmbientLight", "LeafTemperature", "PhiPS2", "LogitPhiPS2")
ps2_vars_d <- c("Treatment", "Transplantation", "Germination", "Row", "Pot", "Plant", "Device", "Time", "Date")
all_ps2_vars <- c(ps2_vars_d, ps2_vars)
fruit_vars <- c("DateHarvest", "DateAnalysis", "DaysFromHarvestToAnalysis", "DaysFromGermination", "Mass", "Ripeness", "SugarAvg", "SugarGrams")
fruit_vars_d <- c("Treatment", "Transplantation", "Germination", "Row", "Pot", "Plant", "BER")
all_fruit_vars <- c(fruit_vars_d, fruit_vars)

# custom functions
## i *think* this is faster and smaller than including these as a dependency via a custom package
multiKS_cont <- function(var, distributions) {
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
    ggplot2::labs(title=paste("PDF plot for", var_name, "over selected distributions"))+
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
    scico::scale_color_scico_d(begin=0.9, end=0.1, palette = palette)+
    ggplot2::theme(
      text = ggplot2::element_text(size=font_sizes[3], family="mont"),
      title = ggplot2::element_text(size=font_sizes[1], family = "open", face = "bold"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size=font_sizes[2], family = "open", face= "bold"),
      axis.title = ggplot2::element_text(size=font_sizes[2], family = "open", face= "bold"),
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
    ggplot2::labs(title=paste("CDF plot for", var_name, "over selected distributions"))+
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
    scico::scale_color_scico_d(begin=0.9, end=0.1, palette = palette)+
    ggplot2::theme(
      text = ggplot2::element_text(size=font_sizes[3], family="mont"),
      title = ggplot2::element_text(size=font_sizes[1], family = "open", face = "bold"),
      legend.position="bottom",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size=font_sizes[2], family = "open", face= "bold"),
      axis.title = ggplot2::element_text(size=font_sizes[2], family = "open", face= "bold"),
    )
  return(p)
}
predict_plot <- function(mod, data, rvar, pvar, group = NULL, length = 50, interval = "confidence", correction = "normal") {
  if (!is.null(data[[deparse(substitute(group))]])){ ## grouped prediciton plot
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
  } else { ### non-grouped prediction plot
    ### deparse variables
    d_pvar <- deparse(substitute(pvar))
    d_rvar <- deparse(substitute(rvar))
    ### get explicit names  of deparsed variables
    ### weird, but necessary for renaming the newdata (dx) columns \>_>/
    pvar_name <- colnames(data[d_pvar])
    rvar_name <- colnames(data[d_rvar])
    ## get predictor range
    dx_pvar <- seq(min(data[[d_pvar]]), max(data[[d_pvar]]), length)
    dx <- data.frame(pvar = dx_pvar)
    colnames(dx) <- pvar_name
    ## make prediction
    if (interval == "confidence") { ### confidence interval
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
    } else { ### prediction interval
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
      ggplot2::geom_point(data = data, ggplot2::aes(x=.data[[d_pvar]], y=.data[[d_rvar]], color=.data[[d_pvar]]))
    ## add prediction
    p <- p + 
      ggplot2::geom_line(data=dx, ggplot2::aes(x=.data[[d_pvar]], y=lo),
                         linewidth=1, show.legend=FALSE)+
      ggplot2::geom_line(data=dx, ggplot2::aes(x=.data[[d_pvar]], y=mn),
                         linewidth=2, show.legend=FALSE)+
      ggplot2::geom_line(data=dx, ggplot2::aes(x=.data[[d_pvar]], y=up),
                         linewidth=1, show.legend=FALSE)+
      ggplot2::geom_ribbon(data=dx, ggplot2::aes(x=.data[[d_pvar]], ymin=lo, ymax=up), alpha = 0.5)
  } ### end non-grouped segment
  ### make the plot look good (group agnostic)
  p <- p +
    ggplot2::labs(
      title = paste("Real data vs predicted 95%", interval, "interval"),
      subtitle = paste("Model:", deparse(mod$call))
    )+
    ggplot2::theme_bw()+
    ggplot2::theme(
      text = ggplot2::element_text(size=font_sizes[3]),
      legend.position="right",
      axis.title = ggplot2::element_text(size=font_sizes[2], face= "bold"),
      title = ggplot2::element_text(size=font_sizes[1], face="bold", lineheight = .5),
      plot.subtitle = ggplot2::element_text(size=font_sizes[2], face = "italic")
    )
  return(p)
}

# popover
gear <- popover(bs_icon("gear"),
                selectInput("palette","Select color palette",
                            choices = p_palettes, selected = "oslo"),
                title = "Options")
# github link
link_github <- tags$a(bs_icon("GitHub"), href = "https://github.com/zachpeagler/tomato-inoculant-app")

##### UI #####
ui <- navbarPage(collapsible = TRUE,
  title = "Tomato Inoculants",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  nav_panel("Fluorescence",
    tabsetPanel(
      tabPanel("Distributions",
        card(card_header("Stomatal Conductance (gsw)", class = "bg-primary"),
          layout_sidebar(
            sidebar=sidebar(open=FALSE,
              checkboxGroupInput("gsw_dists", "Distributions", choices=dists, 
                                 selected=c("normal", "lognormal", "gamma")),
              sliderInput("gsw_len", "Length to Test Distributions Over", min=1,
                          max=500, value=100)
            ), # end sidebar
            div(
              layout_column_wrap(
                plotOutput("gsw_pdf"),
                plotOutput("gsw_cdf")
              )
            ),
            div(
              markdown("###### **One-sample Kolmogorov-Smirnov tests for stomatal conductance against selected distributions**"),
              verbatimTextOutput("gsw_KS")
            )
          ) # end sidebar layout
        ), # end gsw card
        card(card_header("Photosystem II Efficiency (PHIPS2)", class = "bg-secondary"),
         markdown("
                  PhiPS2 is a **unitless ratio** on a scale of 0-1, so we don't need to create PDF and CDF plots and perform KS tests.
                  Instead, we know that we will wind up logit transforming it and we can use the logit transformed version in
                  our regression models. PhiPS2 is calculated as (maximum fluorescence - steady state fluorescence)/maximum fluorescence,
                  and is useful for estimating changes in the quantum yield of non-cyclic electron transport."),
         div(style="border-left: 5px solid", 
          markdown(
          "> For a more comprehensive explanation of PhiPS2, check out 
          [Genty *et al*., 1989](https://www.sciencedirect.com/science/article/abs/pii/S0304416589800169) or
          for a simpler explanation, the [chlorophyll fluorescence wikipedia page](https://en.wikipedia.org/wiki/Chlorophyll_fluorescence).")
          )
        ), # end phips2 card
      ), # end dists tab panel
      tabPanel("Plots",
        card(card_header("Interactive Histograms", class = "bg-primary"),
             div(layout_sidebar(sidebar=sidebar(
               selectInput("gsw_hist_var", "Select X Variable",
                           choices = gsw_vars, selected = "AmbientHumidity"),
               selectInput("gsw_hist_color", "Select Color Variable",
                           choices = gsw_vars_d, selected = "Treatment"),
               sliderInput("gsw_hist_bins", "Number of Bins",
                            value = 30, min = 2, max = 100)
               ), # end sidebar
               markdown("###### **Stomatal conductance data histogram**"),
               plotOutput("gsw_hist")
             )), # gsw hist div
             div(layout_sidebar(sidebar=sidebar(
               selectInput("ps2_hist_var", "Select Variable",
                           choices = ps2_vars, selected = "AmbientHumidity"),
               selectInput("ps2_hist_color", "Select Color Variable",
                           choices = ps2_vars_d, selected = "Treatment"),
               sliderInput("ps2_hist_bins", "Number of Bins",
                            value = 30, min = 2, max = 100)
               ), # end sidebar
               markdown("###### **Photosystem II efficiency data histogram**"),
               plotOutput("ps2_hist")
              )) # ps2 hist div
          ), # end hist card
        card(card_header("Interactive Stomatal Conductance Scatter", class = "bg-primary"),
             layout_sidebar(sidebar = sidebar(
               selectInput("gsw_x","X Variable",
                           choices = all_gsw_vars, selected = "AmbientHumidity"),
               selectInput("gsw_y","Y Variable",
                           choices = all_gsw_vars, selected = "gsw"),
               selectInput("gsw_col","Color Variable",
                           choices = all_gsw_vars, selected = "Treatment"),
               selectInput("gsw_shape", "Shape Variable",
                           choices = gsw_vars_d, selected = "Treatment"),
               sliderInput("gsw_jit", "Jitter Amount",
                           min=0, max=10, value =3),
               sliderInput("gsw_size", "Point Size",
                           min = 1, max=10, value = 3),
               checkboxInput("gsw_fwrap", "Individual Plot Per Treatment", FALSE)
             ), # end sidebar
             card_body(plotOutput("gsw_scatter"))
          ) # end sidebar layout
        ), # end gsw scatter plot
        card(card_header("Interactive Efficiency of Photosystem II (PhiPS2) Scatter", class = "bg-primary"),
             layout_sidebar(sidebar = sidebar(
               selectInput("ps2_x","X Variable",
                           choices = all_ps2_vars, selected = "DaysFromGermination"),
               selectInput("ps2_y","Y Variable",
                           choices = all_ps2_vars, selected = "PhiPS2"),
               selectInput("ps2_col","Color Variable",
                           choices = all_ps2_vars, selected = "AmbientHumidity"),
               selectInput("ps2_shape", "Shape Variable",
                           choices = ps2_vars_d, selected = "Treatment"),
               sliderInput("ps2_jit", "Jitter Amount",
                           min=0, max=10, value =3),
               sliderInput("ps2_size", "Point Size",
                           min = 1, max=10, value = 3),
               checkboxInput("ps2_fwrap", "Individual Plot Per Treatment", FALSE)
             ), # end sidebar
             card_body(plotOutput("ps2_scatter"))
             ) # end sidebar layout
        ) # end phips2 scatter plot
      ), # end plots tab panel
      tabPanel("Statistics",
        card(card_header("Stomatal conductance", class = "bg-primary"),
             div(layout_columns(col_widths = c(8,4),
              div(
                selectInput("gsw_mod_var", "Predictor Variable",
                            choices = mod_var_names, selected = "AmbientHumidity"),
               card(card_header("Model Summary"),
                    verbatimTextOutput("gsw_model_summary")
                    )
                ),# end model summary div
               div(# value boxes for AIC and r^2
                 value_box(
                   title = "GSW Model AIC",
                   value = textOutput("gsw_aic"),
                   theme = "bg-primary",
                   width = 0.2
                 ),
                 value_box(
                   title = "GSW Model R^2",
                   value = textOutput("gsw_r2"),
                   theme = "bg-secondary",
                   width = 0.2
                 ),
                ) # end value box div
               ) # end column wrap
             ), # end div
             div(
               markdown("###### **Stomatal conductance prediction plot**"),
               plotOutput("gsw_pred")
             )
        ), # end gsw stats card
      ), # end stats tab panel
      tabPanel("Data",
        card(card_header("Li-600 Data", class = "bg-primary"),
          dataTableOutput("gsw_DT")
          ),
        card(card_header("PhiPS2 Data", class = "bg-primary"),
          markdown("This dataset is a combination of data from the LI-COR Li-600
                   and PhotosynQ MultispeQ V2.0s. For the sake of this app running
                   efficiently, the data has been pared down to strictly what is needed.
                   The full datasets can be found [on my github](https://www.github.com/zachpeagler/Thesis/data/TIP24)."),
          dataTableOutput("ps2_DT")
          )
      ), # end data tab panel
      tabPanel("Info",
        card(markdown(
          "Fluorescence measurements were taken biweekly with a LI-COR LI-600 and 
          two PhotosynQ MultispeQ V2.0s over the course of the trial.
          Data is presented in a tidy format with each row representing a single 
          observation and each column representing a variable. <br>
          ### Explanatory Variables
          **Treatment** is the inoculation timing of the tomato. Options are Control, Germination, Transplantation, and Germ+Trans. <br>
          **Time** is the time at which the measurement was taken. <br>
          **Date** is the date at which the measurement was taken. <br>
          **DaysFromGermination** is the number of days from germination (2025-05-01) to the date of measurement. <br>
          **MinutesFromStart** is the number of minutes from the start of that day's observations to the time of measurement. <br>
          **Row** is the row of the tomato. (A:D) <br>
          **Pot** is the pot number of the tomato. (1:12) <br>
          **Plant** is a combination of *Row* and *Pot*, and acts as an ID for every individual plant. (A1: D12) <br>
          **AmbientHumidity** is the relative humidity (add units) at the time of measurement. <br>
          **AmbientLight** is the ambient light level (add units) at the time of measurement. <br>
          **AmbientPressure** is the ambient pressure (add units) at the time of measurement. <br>
          **LeafTemperature** is the temperature (C) of the leaf. <br>
          ### Response Variables
          **gsw** is the stomatal conductance (mol m-2 s-1) of the leaf. Stomatal conductance refers to the
          rate at which molecules are moving through the leaf's stomates, and is indicitave of photosynthesis.<br>
          **PhiPS2** is the quantum yield. It is unitless. (0:1) <br>
          > It's important to note that **only** the Li-600 can measure gsw, while both
          the Li-600 and the MultispeQ can measure PhiPS2. Also, even though both devices can 
          measure PhiPS2, they do so **in different ways**. For our purposes, this is fine
          so long as the measurements from each device correlate.
          "))
      ) # end info tab Panel
    ) # end tabset Panel
  ), # end nav panel "Fluorescence"
  nav_panel("Fruit",
    tabsetPanel(
      tabPanel("Distributions"),
      tabPanel("Plots"),
      tabPanel("Statistics"),
      tabPanel("Data",
        card(card_header("Fruit Data", class = "bg-primary"),
            dataTableOutput("fruit_DT")
        )
      ), # end data tab panel
      tabPanel("Info",
        markdown("
        The tomatoes were grown in 4 rows of 12 pots each, with each row corresponding to a different inoculation treatment.
        The data table is formatted in a tidy format with each row corresponding to one fruit and each column representing a variable.<br>
        ### Explanatory Variables <br>
        **Treatment** (factor) is the inoculation timing of the tomato. Options are Control, Germination, Transplantation, and Germ+Trans. <br>
        **Transplantation** (logical) indicates if the fruit comes from a plant inoculated at transplantation. <br>
        **Germination** (logical) indicates if the fruit comes from a plant inoculated at germination <br>
        **Row** (factor) is the row number of the tomato. (1:4) <br>
        **Pot** (factor) is the pot number of the tomato. (1:12) <br>
        **Plant** (factor) is a combination of *row* and *plant*, and acts as an ID for every individual plant. (1 1: 4 12) <br>
        **DateHarvest** (date) is the date the fruit was harvested (August 2024:October 2024) <br>
        **DateAnalysis** (date) is the date the fruit was analyzed in the lab (August 2024:October 2024) <br>
        **DaysFromHarvestToAnalysis** (int) is the number of days from harvest to analysis. <br>
        **DaysFromGermination** (int) is the number of days from germination to fruit analysis. <br>
        ### Response Variables <br>
        **Mass** is the mass in grams of the tomato, measured on an Ohaus Scout. (~10:~400) <br>
        **BER** corresponds to whether or not the tomato has blossom end rot, a disease caused by calcium deficiency that renders the fruit unmarketable. (0,1) <br>
        **Penetrometer** corresponds to the force in kilograms it takes to penetrate the flesh of the tomato (~0.5:~4) <br>
        **Ripeness** is the **Penetrometer** value mapped from 0:1 and reversed, so that riper fruit are closer to 1 and unripe fruit are closer to 0. (0:1) <br>
        **SugarAvg** is the average of two measurements of the tomato juice's sugar concentration taken on a Fisher BRIX Refractometer (~2:~12) <br>
        **SugarGrams** is the grams of sugar in the tomato, calculated as (**SugarAvg**/100)x**Mass** <br>
        **Fruit** is an internal variable for lazily summarizing fruit counts. It always equals 1.
        ")
      ) # end info tab
    ) # end tab set panel
  ), # end fruit nav panel
  nav_panel("Info",
    layout_column_wrap(
    markdown(
    "##### **Tomato inoculant timing trial** <br>
    This trial accompanies the following objectives laid out in my thesis: <br>
    **Hypothesis 2** – Microbial bacterial granules (BGs) with the full microbial consortium applied
    at germination and transplantation will increase fluorescence parameters and fruit yield and quality
    more than either germination or transplantation inoculations. <br>
    **Objective 2.1** - Determine the effect of BG inoculation timing on tomato crop quality and yield. <br>
    **Objective 2.2** - Determine the effect of BG inoculation timing on tomato plant fluorescence parameters."),
    
    card(img(src="TIP24_1.jpg", align = "center"), max_height = 500)),
    markdown("##### **Microbial Consortium** <br>
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
      Also shown to increase root and shoot growth ([Glick et al, 1997](https://www.researchgate.net/publication/223543401_Early_development_of_canola_seedlings_in_the_presence_of_the_plant_growth-promoting_rhizobacterium_Pseudomonas_putida_GR12-2); [Hall et al, 1996](https://ui.adsabs.harvard.edu/abs/1996IsJPS..44...37H/abstract)). 
    "),
    markdown("
    This app only covers data from the **2024** tomato inoculant trial. Mostly 
    for the sake of my own sanity, as well as the fact that the trials aren't 
    exactly apples to apples. In **2023** we applied foliar and/or soil applications
    of *Methylobacterium oryzae CBMB20* (1x10^6 cfu/mL) to salt-stressed tomato plants, cultivar BHN 589.
    Controls were included for salt and inoculation for a two-factorial experimental
    design, with 8 replicates per group for a total of 32 plants. <br>
    The experimental design changed in 2024, opting for purely soil applications of
    a bacterial consortium: *Azospirillium brasilense*, *Azotobacter chroococcum*,
    *Bacillus subtilis*, *Methylobacterium oryzae CBMB20*, and *Pseudomonas putida* 
    (all at 1x10^6 cfu/mL) at two different time points: **germination** and/or **transplantation**.
    We also increased the sample size to 12 plants per group for a total of 48 plants. <br>
    "),
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
    ),
  nav_spacer(),
  nav_item(gear),
  nav_item(link_github)
)

##### SERVER #####
server <- function(input, output) {
# Reactive Expressions
# you might say "don't make a thousand individual reactive expressions!!! make a reactive values
# object and store them all in that!!" and to that i say "no"
# these are all "lazy" so they should (in theory) be more optimized than updating
# all the inputs in a single reactive values object. Less updates = faster. Right?
## global reactive expressions
  Rpalette <- reactive({input$palette})
## fluorescence reactive expressions
  ### gsw
  Rgsw_dists <- reactive({input$gsw_dists})
  Rgsw_hist_var <- reactive({input$gsw_hist_var})
  Rgsw_hist_color <- reactive({input$gsw_hist_color})
  Rgsw_hist_bins <- reactive({input$gsw_hist_bins})
  Rgsw_len <- reactive({input$gsw_len})
  Rgsw_x <- reactive({input$gsw_x})
  Rgsw_y <- reactive({input$gsw_y})
  Rgsw_col <- reactive({input$gsw_col})
  Rgsw_shape <- reactive({input$gsw_shape})
  Rgsw_jit <- reactive({input$gsw_jit * 0.1})
  Rgsw_fwrap <- reactive({input$gsw_fwrap})
  Rgsw_size <- reactive({input$gsw_size})
  Rgsw_mod_var <- reactive({input$gsw_mod_var})
  ### ps2
  Rps2_dists <- reactive({input$ps2_dists})
  Rps2_hist_var <- reactive({input$ps2_hist_var})
  Rps2_hist_color <- reactive({input$ps2_hist_color})
  Rps2_hist_bins <- reactive({input$ps2_hist_bins})
  Rps2_len <- reactive({input$ps2_len})
  Rps2_x <- reactive({input$ps2_x})
  Rps2_y <- reactive({input$ps2_y})
  Rps2_col <- reactive({input$ps2_col})
  Rps2_shape <- reactive({input$ps2_shape})
  Rps2_jit <- reactive({input$ps2_jit * 0.1})
  Rps2_fwrap <- reactive({input$ps2_fwrap})
  Rps2_size <- reactive({input$ps2_size})
## fruit reactive expressions
  
# Outputs
## Fluorescence
### Distributions
#### gsw
  # ks
  output$gsw_KS <- renderPrint({
    multiKS_cont(data_gsw$gsw, Rgsw_dists())
  })
  # pdf
  output$gsw_pdf <- renderPlot({
    multiPDF_plot(data_gsw$gsw, Rgsw_len(), Rgsw_dists(), palette = Rpalette())
    })
  # cdf
  output$gsw_cdf <- renderPlot({
    multiCDF_plot(data_gsw$gsw, Rgsw_len(), Rgsw_dists(), palette = Rpalette())
  })
### Plots
# hists
  output$gsw_hist <- renderPlot({
    gh <- ggplot(data_gsw, aes(x=.data[[Rgsw_hist_var()]], color = .data[[Rgsw_hist_color()]],
                               fill = .data[[Rgsw_hist_color()]]))+
      geom_histogram(bins = Rgsw_hist_bins())+
      theme_bw() +
      theme(
        text = element_text(size=font_sizes[3], family="mont"),
        axis.title = element_text(size=font_sizes[2], family = "open", face= "bold"),
        legend.title = ggplot2::element_text(size=font_sizes[2], family = "open", face= "bold"),
      )
    gh <- gh + scale_color_scico_d(begin=0.9, end=0.1, palette=Rpalette())
    gh <- gh + scale_fill_scico_d(begin=0.9, end=0.1, palette=Rpalette())
    return(gh)
  })
  output$ps2_hist <- renderPlot({
    ph <- ggplot(data_ps2, aes(x=.data[[Rps2_hist_var()]], color = .data[[Rps2_hist_color()]],
                               fill = .data[[Rps2_hist_color()]]))+
      geom_histogram(bins = Rps2_hist_bins()) + 
      theme_bw() +
      theme(
        text = element_text(size=font_sizes[3], family="mont"),
        axis.title = element_text(size=font_sizes[2], family = "open", face= "bold"),
        legend.title = ggplot2::element_text(size=font_sizes[2], family = "open", face= "bold"),
      )
    ph <- ph + scale_color_scico_d(begin=0.9, end=0.1, palette=Rpalette())
    ph <- ph + scale_fill_scico_d(begin=0.9, end=0.1, palette=Rpalette())
    return(ph)
  })
# scatters
  output$gsw_scatter <- renderPlot({
    gs <- ggplot(data=data_gsw, aes(x=.data[[Rgsw_x()]], y=.data[[Rgsw_y()]],
                                    color = .data[[Rgsw_col()]], shape = .data[[Rgsw_shape()]]))+
      geom_jitter(width=Rgsw_jit(), height=Rgsw_jit()*0.5, size = Rgsw_size())+
      ylab(gettext(Rgsw_y()))+
      xlab(gettext(Rgsw_x()))+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3], family="mont"),
        axis.title = element_text(size=font_sizes[2], family = "open", face= "bold"),
        title = element_text(size=font_sizes[1], family="open", face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], family = "open", face= "bold"),
        legend.title.position = "top"
      )
    if (Rgsw_x() %in% gsw_vars_d) {
      gs <- gs + scale_x_discrete(guide=guide_axis(check.overlap=TRUE))
    } else {
      gs <- gs + scale_x_continuous(guide=guide_axis(check.overlap=TRUE))
    }
    if (Rgsw_fwrap() == TRUE){
      gs <- gs + facet_wrap(~Treatment)
    }
    if (Rgsw_col() %in% gsw_vars_d) {
      gs <- gs + scale_color_scico_d(begin=0.9, end=0.1, palette=Rpalette())
    } else {
      gs <- gs + scale_color_scico(begin=0.9, end=0.1, palette=Rpalette())
    }
    return(gs)
  })
  output$ps2_scatter <- renderPlot({
    ps <- ggplot(data=data_ps2, aes(x=.data[[Rps2_x()]], y=.data[[Rps2_y()]],
                                    color = .data[[Rps2_col()]], shape = .data[[Rps2_shape()]]))+
      geom_jitter(width=Rps2_jit(), height=Rps2_jit()*0.5, size = Rps2_size())+
      ylab(gettext(Rps2_y()))+
      xlab(gettext(Rps2_x()))+
      theme_bw()+
      theme(
        text = element_text(size=font_sizes[3], family="mont"),
        axis.title = element_text(size=font_sizes[2], family = "open", face= "bold"),
        title = element_text(size=font_sizes[1], family="open", face="bold", lineheight = .8),
        legend.title = ggplot2::element_text(size=font_sizes[2], family = "open", face= "bold"),
        #        legend.position = "bottom",
        legend.title.position = "top"
      )
    if (Rps2_x() %in% ps2_vars_d) {
      ps <- ps + scale_x_discrete(guide=guide_axis(check.overlap=TRUE))
    } else {
      ps <- ps + scale_x_continuous(guide=guide_axis(check.overlap=TRUE))
    }
    if (Rps2_fwrap() == TRUE){
      ps <- ps + facet_wrap(~Treatment)
    }
    if (Rps2_col() %in% ps2_vars_d) {
      ps <- ps + scale_color_scico_d(begin=0.9, end=0.1, palette=Rpalette())
    } else {
      ps <- ps + scale_color_scico(begin=0.9, end=0.1, palette=Rpalette())
    }
    return(ps)
  })
### Statistics
#### gsw
  # model summary
  # AIC
  # R^2
  # prediction plot
### Data
  output$gsw_DT <- renderDataTable({
    data_gsw
  })
  output$ps2_DT <- renderDataTable({
    data_ps2
  })
## Fruit
### Distributions
### Plots
### Stats
### Data
  output$fruit_DT <- renderDataTable({
    data_fruit
  })
### Info
}

# run it!
shinyApp(ui = ui, server = server)