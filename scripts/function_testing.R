library(vegan)
library(ggplot2)
library(scico)
# pca plot
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
    geom_segment(data = vx, aes(x=0, y=0, xend=PC1*.18, yend=PC2*.18), color = "black")+
    annotate("text", x=vx[,1]*.2, y=vx[,2]*.2, label = rownames(vx))+
    xlim(-1, 1)+
    xlab(paste0("PC1 (", PC1val, "%)"))+
    ylab(paste0("PC2 (", PC2val, "%)"))+
    theme_bw()
}
# pca data
pca_data <- function(data, pcavars){
  p1 <- rda(pcavars)
  outdata <- cbind(data, p1$CA$u)
  return(outdata)
}

# pca prediction plot
pca_predict_plot <- function(mod, data, rvar, pvar, group = NULL, length = 50, interval = "confidence", correction = "normal") {
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
  p <- p +
    ggplot2::labs(
      title = paste("Real data vs predicted 95%", interval, "interval"),
      subtitle = paste("Model:", deparse(mod$call))
    )+
    ggplot2::theme_bw()+
    ggplot2::theme(
      text = ggplot2::element_text(size=16),
      legend.position="right",
      axis.title = ggplot2::element_text(size=16, face= "bold"),
      title = ggplot2::element_text(size=20, face="bold", lineheight = .5),
      plot.subtitle = ggplot2::element_text(size=14, face = "italic")
    )
  return(p)
}
