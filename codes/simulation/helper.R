#' Combine multiple chains
#' @param l1ideal_list a list of l1ideal results
#' @param coord_flip_ls a list of logical values indicating whether the coordinates need to be flipped
#' @param d1_sign_flip_ls a list of logical values indicating whether the signs of the first coordinate need to be flipped after flipping the coordinates if necessary
#' @param d2_sign_flip_ls a list of logical values indicating whether the signs of the second coordinate need to be flipped after flipping the coordinates if necessary
get_summary_chains <- function(
    l1ideal_list,
    coord_flip_ls,
    d1_sign_flip_ls,
    d2_sign_flip_ls) {
    legislators_res <- get_positions_from_chains(l1ideal_list, "legislators", coord_flip_ls, d1_sign_flip_ls, d2_sign_flip_ls) |> mutate(type = "legislator")
    yea_positions_res <- get_positions_from_chains(l1ideal_list, "yea_positions", coord_flip_ls, d1_sign_flip_ls, d2_sign_flip_ls) |> mutate(type = "yea_positions")
    nay_positions_res <- get_positions_from_chains(l1ideal_list, "nay_positions", coord_flip_ls, d1_sign_flip_ls, d2_sign_flip_ls) |> mutate(type = "nay_positions")
    res <- bind_rows(legislators_res, yea_positions_res, nay_positions_res) |> relocate(type)
    return(res)
}


get_positions_from_chains <- function(l1ideal_list, quantity_name,
                                      coord_flip_ls, 
                                      d1_sign_flip_ls, 
                                      d2_sign_flip_ls) {


    pos_d1_list <- map(l1ideal_list, function(x) x[[quantity_name]][[1]])
    pos_d2_list <- map(l1ideal_list, function(x) x[[quantity_name]][[2]])

    for(i in 1:length(l1ideal_list)) {
        # Coordinate flip if needed
        if (isTRUE(coord_flip_ls[i])) {
            temp <- pos_d1_list[[i]]
            pos_d1_list[[i]] <- pos_d2_list[[i]]
            pos_d2_list[[i]] <- temp
        }

        # Apply sign flip transformations if needed
        if (isTRUE(d1_sign_flip_ls[[i]])) {
            pos_d1_list[[i]] <- - pos_d1_list[[i]]
        }
        if (isTRUE(d2_sign_flip_ls[[i]])) {
            pos_d2_list[[i]] <- - pos_d2_list[[i]]
        }
    }

    # Convert lists to mcmc.list format if necessary
    # Here, pos_d1_list and pos_d2_list are list of mcmc objects
    pos_d1_mcmc <- do.call(mcmc.list, pos_d1_list)
    pos_d2_mcmc <- do.call(mcmc.list, pos_d2_list)

    # Convert lists to draws array format
    pos_d1_draws <- as_draws_array(pos_d1_mcmc)
    pos_d2_draws <- as_draws_array(pos_d2_mcmc)

    # Summarize draws
    res1 <- summarise_draws(pos_d1_draws)
    res2 <- summarise_draws(pos_d2_draws)

    res1 <- res1 |> mutate(dimension = 1)
    res2 <- res2 |> mutate(dimension = 2)
    res <- bind_rows(res1, res2) |> relocate(dimension)
    return(res)
}


#' Plot the simulation study (legislators)
#' 
#' @param synthetic_data
#' @param l1object
#' 
#' @return 
#' 
#' @import ggplot2
#' @references Sooahn Shin, Johan Lim, and Jong Hee Park 2019. "L1 norm Based Multidimensional Ideal Point Estimation: With Application to Roll Call Voting Data" Working Paper.
#' @export plot.simulation
plot_legislator_chains <- function(synthetic_data,
                                   summary_df,
                                   alpha = 1,
                                   size = 2) {
    ideal_df <- synthetic_data$legis_data
    legis_name <- summary_df |>
        filter(dimension == 1 & type == "legislator") |>
        pull(variable)
    ideal_df <- ideal_df[ideal_df$name %in% legis_name, ]
    ideal_df$ideal_point_1d <- summary_df |>
        filter(dimension == 1 & type == "legislator") |>
        pull(mean)
    ideal_df$ideal_point_2d <- summary_df |>
        filter(dimension == 2 & type == "legislator") |>
        pull(mean)

    plot_true <- ggplot() +
        geom_point(aes(x = true_ideal_point_1d, y = true_ideal_point_2d, col = group, shape = group), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "Dimension 1", y = "Dimension 2", title = "True Latent Space") +
        scale_color_brewer("", type = "qual", palette = "Set1")
    plot_res <- ggplot() +
        geom_point(aes(x = ideal_point_1d, y = ideal_point_2d, col = group, shape = group), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "Dimension 1", y = "Dimension 2", title = "L1 Norm Ideal Point Estimation") +
        scale_color_brewer("", type = "qual", palette = "Set1")
    plot_comp1 <- ggplot() +
        geom_point(aes(x = true_ideal_point_1d, y = ideal_point_1d, col = group, shape = group), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "true", y = "estimate", title = "True v. Estimate (Dimension 1)") +
        scale_color_brewer("", type = "qual", palette = "Set1")
    plot_comp2 <- ggplot() +
        geom_point(aes(x = true_ideal_point_2d, y = ideal_point_2d, col = group, shape = group), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "true", y = "estimate", title = "True v. Estimate (Dimension 2)") +
        scale_color_brewer("", type = "qual", palette = "Set1")
    p <- list(plot_true, plot_res, plot_comp1, plot_comp2)
    return(p)
}


#' Plot the simulation study (rollcalls)
#' 
#' @param synthetic_data
#' @param l1object
#' 
#' @return 
#' 
#' @import ggplot2
#' @references Sooahn Shin, Johan Lim, and Jong Hee Park 2019. "L1 norm Based Multidimensional Ideal Point Estimation: With Application to Roll Call Voting Data" Working Paper.
#' @export plot.simulation
plot_rollcall_chains <- function(synthetic_data,
                                   summary_df,
                                   alpha = 1,
                                   size = 2) {
    ideal_df <- synthetic_data$votes_data
    votes_name <- summary_df |>
        filter(dimension == 1 & type == "nay_positions") |>
        pull(variable)
    ideal_df <- ideal_df[ideal_df$name %in% votes_name, ]
    ideal_df$nay_position_1d <- summary_df |>
        filter(dimension == 1 & type == "nay_positions") |>
        pull(mean)
    ideal_df$nay_position_2d <- summary_df |>
        filter(dimension == 2 & type == "nay_positions") |>
        pull(mean)
    ideal_df$yea_position_1d <- summary_df |>
        filter(dimension == 1 & type == "yea_positions") |>
        pull(mean)
    ideal_df$yea_position_2d <- summary_df |>
        filter(dimension == 2 & type == "yea_positions") |>
        pull(mean)

    plot_true_nay <- ggplot() +
        geom_point(aes(x = true_nay_position_1d, y = true_nay_position_2d), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "Dimension 1", y = "Dimension 2", title = "True Latent Space") +
        scale_color_brewer("", type = "qual", palette = "Set1")
    plot_res_nay <- ggplot() +
        geom_point(aes(x = nay_position_1d, y = nay_position_2d), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "Dimension 1", y = "Dimension 2", title = "L1 Norm Ideal Point Estimation") +
        scale_color_brewer("", type = "qual", palette = "Set1")
    plot_comp1_nay <- ggplot() +
        geom_point(aes(x = true_nay_position_1d, y = nay_position_1d), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "true", y = "estimate", title = "True v. Estimate (Dimension 1)") +
        scale_color_brewer("", type = "qual", palette = "Set1")
    plot_comp2_nay <- ggplot() +
        geom_point(aes(x = true_nay_position_2d, y = nay_position_2d), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "true", y = "estimate", title = "True v. Estimate (Dimension 2)") +
        scale_color_brewer("", type = "qual", palette = "Set1")

    ## run the same codes with yea
    plot_true_yea <- ggplot() +
        geom_point(aes(x = true_yea_position_1d, y = true_yea_position_2d), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "Dimension 1", y = "Dimension 2", title = "True Latent Space") +
        scale_color_brewer("", type = "qual", palette = "Set1")
    plot_res_yea <- ggplot() +
        geom_point(aes(x = yea_position_1d, y = yea_position_2d), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "Dimension 1", y = "Dimension 2", title = "L1 Norm Ideal Point Estimation") +
        scale_color_brewer("", type = "qual", palette = "Set1")
    plot_comp1_yea <- ggplot() +
        geom_point(aes(x = true_yea_position_1d, y = yea_position_1d), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "true", y = "estimate", title = "True v. Estimate (Dimension 1)") +
        scale_color_brewer("", type = "qual", palette = "Set1")
    plot_comp2_yea <- ggplot() +
        geom_point(aes(x = true_yea_position_2d, y = yea_position_2d), data = ideal_df, alpha = alpha, size = size) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = "true", y = "estimate", title = "True v. Estimate (Dimension 2)") +
        scale_color_brewer("", type = "qual", palette = "Set1")

    p_nay <- list(plot_true_nay, plot_res_nay, plot_comp1_nay, plot_comp2_nay)
    p_yea <- list(plot_true_yea, plot_res_yea, plot_comp1_yea, plot_comp2_yea)
    return(p = list(p_nay, p_yea))
}

convert_seconds_to_hours <- function(time_string) {
  # Extract the numeric part of the string
  seconds <- as.numeric(sub(" seconds", "", time_string))
  
  # Convert seconds to hours
  hours <- seconds / 3600
  
  # Return the formatted string in hours
  paste(format(hours, digits=4), "hours")
}

#' Generate synthetic data (extended options of utility function)
generate.data.ext <- function(dimensions = 2, 
                              n1 = 40, 
                              n2 = 40, 
                              n3 = 10, 
                              n4 = 10,
                              m = 1000,
                              mu1 = c(1,1), 
                              mu2 = c(-1,-1), 
                              mu3 = c(1.2,-1.2), 
                              mu4 = c(-1.2,1.2),
                              sigma1 = 0.5, 
                              sigma2 = 0.5, 
                              sigma3 = 0.2, 
                              sigma4 = 0.2, 
                              theta = 1, 
                              utility = c("gaussian", "quadratic", "linear"),
                              uniform = TRUE,
                              seed = NULL) {
  if(!dimensions%in%c(1,2)) stop("'dimensions' must be either 1 or 2.\n")
  
  set.seed(seed)
  
  ### 1. Generate ideal points
  n <- n1+n2+n3+n4
  L <- data.frame(name = paste("Legislator",1:n), 
                  group = as.factor(c(rep("group1",n1),rep("group2",n2),rep("group3",n3),rep("group4",n4))),
                  stringsAsFactors = F)
  L[c("true_ideal_point_1d","true_ideal_point_2d")] <- rbind(rmvnorm(n1,mu1,diag(2)*sigma1),
                                                             rmvnorm(n2,mu2,diag(2)*sigma2),
                                                             rmvnorm(n3,mu3,diag(2)*sigma3),
                                                             rmvnorm(n4,mu4,diag(2)*sigma4))
  
  ### 2. Generate Yea & Nay outcome
  if(uniform) {
    O <- data.frame(name = paste("Vote",1:m), 
                    true_yea_position_1d = rnorm(m, mean = 0, sd = theta),
                    true_yea_position_2d = rnorm(m, mean = 0, sd = theta),
                    true_nay_position_1d = rnorm(m, mean = 0, sd = theta),
                    true_nay_position_2d = rnorm(m, mean = 0, sd = theta))
  } else {
    if(n2!=0) {
      idx_yea <- c(sample(1:n1, floor(m/2), replace = TRUE), sample((n1+1):(n1+n2), m - floor(m/2), replace = TRUE))
      idx_nay <- c(sample((n1+1):(n1+n2), floor(m/2), replace = TRUE), sample(1:n1, m - floor(m/2), replace = TRUE))
    } else {
      idx_yea <- sample(1:n1, m, replace = TRUE)
      idx_nay <- sample(1:n1, m, replace = TRUE)
    }
    O <- data.frame(name = paste("Vote",1:m), 
                    true_yea_position_1d = L[idx_yea,"true_ideal_point_1d"] + rnorm(m, mean = 0, sd = 0.1),
                    true_yea_position_2d = L[idx_yea,"true_ideal_point_2d"] + rnorm(m, mean = 0, sd = 0.1),
                    true_nay_position_1d = L[idx_nay,"true_ideal_point_1d"] + rnorm(m, mean = 0, sd = 0.1),
                    true_nay_position_2d = L[idx_nay,"true_ideal_point_2d"] + rnorm(m, mean = 0, sd = 0.1))
  }
  
  
  ### 3. Compute Pr(Yea) & Pr(Nay)
  Dijy <- L[rep(1:n, each = m),c("true_ideal_point_1d","true_ideal_point_2d")] - 
    do.call("rbind", rep(list(O[c("true_yea_position_1d","true_yea_position_2d")]), n))
  Dijn <- L[rep(1:n, each = m),c("true_ideal_point_1d","true_ideal_point_2d")] - 
    do.call("rbind", rep(list(O[c("true_nay_position_1d","true_nay_position_2d")]), n))

  if (utility == "gaussian") {
    
    Dijy <- (Dijy)^2
    Uijy <- exp(-rowSums(Dijy)/2)*10
    Dijn <- (Dijn)^2
    Uijn <- exp(-rowSums(Dijn)/2)*10
    
  } else if (utility == "quadratic") {
    
    Dijy <- (Dijy)^2
    Uijy <- -rowSums(Dijy)
    Dijn <- (Dijn)^2
    Uijn <- -rowSums(Dijn)
    
  } else if (utility == "linear") {
    
    Dijy <- abs(Dijy)
    Uijy <- -rowSums(Dijy)
    Dijn <- abs(Dijn)
    Uijn <- -rowSums(Dijn)
    
  }
  
  Ystar <- pnorm(Uijy-Uijn,0,0.5)
  Ystar <- matrix(Ystar,n,m,byrow=TRUE)
  
  ### 4. Set Y
  U <- matrix(runif(m*n,0,1),n,m)
  Y <- U<Ystar
  Y <- 1*Y
  rownames(Y) <- L$name
  colnames(Y) <- O$name
  
  if(dimensions==1) {
    L <- L[,c("name","group","true_ideal_point_1d")]
    O <- O[,c("true_yea_position_1d","true_nay_position_1d")]
  }
  
  dat <- list(votes = Y, 
              legis_data = L, 
              votes_data = O, 
              code = match.call())
  
  class(dat) <- c("l1synthetic")
  
  return(dat)
}

rmvnorm <- function(n,mu,Sigma){
  E <- matrix(rnorm(n*length(mu)),n,length(mu))
  t( t(E%*%chol(Sigma)) + c(mu) )
}

plot_multichain <- function(n_chain, dat, res_list, coord_flip_ls, d1_sign_flip_ls = NULL, d2_sign_flip_ls = NULL) {
    if(is.null(d1_sign_flip_ls)) d1_sign_flip_ls <- rep(FALSE, n_chain)
    if(is.null(d2_sign_flip_ls)) d2_sign_flip_ls <- rep(FALSE, n_chain)
    p_ls <- map(1:n_chain, function(m) {
        p <- plot.simulation(dat, res_list[[m]], coord_flip = coord_flip_ls[m])
        p1 <- p[[3]] + scale_color_brewer("", type = "qual", palette = "Set1")
        if(d1_sign_flip_ls[[m]]) p1 <- p1 + scale_y_continuous(trans = scales::reverse_trans(), labels = function(y) -y)
        p2 <- p[[4]] + scale_color_brewer("", type = "qual", palette = "Set1")
        if(d2_sign_flip_ls[[m]]) p2 <- p2 + scale_y_continuous(trans = scales::reverse_trans(), labels = function(y) -y)
        row_title <- ggplot() +
            ggtitle(paste("Chain", m)) +
            theme_void() +
            theme(
                plot.title = element_text(hjust = 0.5, face = "bold")
            )
        return(((row_title / (p1 | p2)) + plot_layout(heights = c(0.05, 1))))
    })
    return(p_ls)
}

#' Generate figure for Bayesian IRT
make_irt_figure <- function(simulation_id, filename, coord_flip = FALSE, sign_flip_1d = FALSE, sign_flip_2d = FALSE) {
    dat <- read_rds(paste0("../../data/simulation/utility/sim", simulation_id, "_", filename, "_dat.rds"))
    res <- read_rds(paste0("../../data/simulation/utility/sim", simulation_id, "_", filename, "_res_summary.rds"))
    load(paste0("../../data/simulation/utility/sim", simulation_id, "_", filename, "_irt.RData"))
    p <- plot_legislator_chains(dat, res)
    ideal_df <- dat$legis_data
    ideal_df <- ideal_df[ideal_df$name %in% res$variable, ]
    if (coord_flip) {
        if (sign_flip_1d) {
            ideal_df$ideal_point_1d <- -idl$xbar[, 2]
        } else {
            ideal_df$ideal_point_1d <- idl$xbar[, 2]
        }
        if (sign_flip_2d) {
            ideal_df$ideal_point_2d <- -idl$xbar[, 1]
        } else {
            ideal_df$ideal_point_2d <- idl$xbar[, 1]
        }
    } else {
        if (sign_flip_1d) {
            ideal_df$ideal_point_1d <- -idl$xbar[, 1]
        } else {
            ideal_df$ideal_point_1d <- idl$xbar[, 1]
        }
        if (sign_flip_2d) {
            ideal_df$ideal_point_2d <- -idl$xbar[, 2]
        } else {
            ideal_df$ideal_point_2d <- idl$xbar[, 2]
        }
    }

    p[[2]] <- ggplot() +
        geom_point(
            aes(
                x = ideal_point_1d,
                y = ideal_point_2d,
                col = group, shape = group
            ),
            size = 2, data = ideal_df
        ) +
        theme_classic() +
        theme(legend.position = "none") +
        scale_color_brewer("", type = "qual", palette = "Set1") +
        labs(x = "Dimension 1", y = "Dimension 2", title = "Bayesian IRT (without pre-/post-process)")
    p[[3]] <- ggplot() +
        geom_point(
            aes(
                x = true_ideal_point_1d, y =
                    ideal_point_1d, col = group, shape = group
            ),
            size = 2, data = ideal_df
        ) +
        theme_classic() +
        theme(legend.position = "none") +
        scale_color_brewer("", type = "qual", palette = "Set1") +
        labs(x = "true", y = "estimate", title = "True v. Estimate (Dimension 1)")
    p[[4]] <- ggplot() +
        geom_point(
            aes(
                x = true_ideal_point_2d, y =
                    ideal_point_2d, col = group, shape = group
            ),
            size = 2, data = ideal_df
        ) +
        theme_classic() +
        theme(legend.position = "none") +
        scale_color_brewer("", type = "qual", palette = "Set1") +
        labs(x = "true", y = "estimate", title = "True v. Estimate (Dimension 2)")
    return(p)
}

#' Generate figure for W-NOMINATE
make_wn_figure <- function(simulation_id, filename, coord_flip = FALSE, sign_flip_1d = FALSE, sign_flip_2d = FALSE) {
    dat <- read_rds(paste0("../../data/simulation/utility/sim", simulation_id, "_", filename, "_dat.rds"))
    res <- read_rds(paste0("../../data/simulation/utility/sim", simulation_id, "_", filename, "_res_summary.rds"))
    load(paste0("../../data/simulation/utility/sim", simulation_id, "_", filename, "_wn.RData"))
    p <- plot_legislator_chains(dat, res)
    ideal_df <- dat$legis_data
    ideal_df <- ideal_df[ideal_df$name %in% res$variable, ]
    if (coord_flip) {
        if (sign_flip_1d) {
            ideal_df$ideal_point_1d <- -wn$legislators$coord2D
        } else {
            ideal_df$ideal_point_1d <- wn$legislators$coord2D
        }
        if (sign_flip_2d) {
            ideal_df$ideal_point_2d <- -wn$legislators$coord1D
        } else {
            ideal_df$ideal_point_2d <- wn$legislators$coord1D
        }
    } else {
        if (sign_flip_1d) {
            ideal_df$ideal_point_1d <- -wn$legislators$coord1D
        } else {
            ideal_df$ideal_point_1d <- wn$legislators$coord1D
        }
        if (sign_flip_2d) {
            ideal_df$ideal_point_2d <- -wn$legislators$coord2D
        } else {
            ideal_df$ideal_point_2d <- wn$legislators$coord2D
        }
    }

    p[[2]] <- ggplot() +
        geom_point(
            aes(
                x = ideal_point_1d,
                y = ideal_point_2d,
                col = group, shape = group
            ),
            size = 2, data = ideal_df
        ) +
        theme_classic() +
        theme(legend.position = "none") +
        scale_color_brewer("", type = "qual", palette = "Set1") +
        labs(x = "Dimension 1", y = "Dimension 2", title = "W-NOMINATE")
    p[[3]] <- ggplot() +
        geom_point(
            aes(
                x = true_ideal_point_1d, y =
                    ideal_point_1d, col = group, shape = group
            ),
            size = 2, data = ideal_df
        ) +
        theme_classic() +
        theme(legend.position = "none") +
        scale_color_brewer("", type = "qual", palette = "Set1") +
        labs(x = "true", y = "estimate", title = "True v. Estimate (Dimension 1)")
    p[[4]] <- ggplot() +
        geom_point(
            aes(
                x = true_ideal_point_2d, y =
                    ideal_point_2d, col = group, shape = group
            ),
            size = 2, data = ideal_df
        ) +
        theme_classic() +
        theme(legend.position = "none") +
        scale_color_brewer("", type = "qual", palette = "Set1") +
        labs(x = "true", y = "estimate", title = "True v. Estimate (Dimension 2)")
    return(p)
}