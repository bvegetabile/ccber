################################################################################
#
# Functions related to calculating the Entropy Rate of Finite Markov Processes
#   - Author: Brian Vegetabile, University of California - Irvine
#
################################################################################

# Simulation of Finite Markov Chains -------------------------------------------

#' Simulate a Markov Chain Given a Transition Matrix
#'
#' @param trans_mat A row-stochastic matrix representing a Markov transition matrix
#' @param n_sims Number of observations for simulation length. Default of 100.
#' @return A vector of observations from a Markov chain
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 500)
SimulateMarkovChain <- function(trans_mat, n_sims=100){
  n_states <- nrow(trans_mat)
  states <- seq(1, n_states)

  simulations <- matrix(0, nrow = 1, ncol = n_sims)

  stat_mat <- CalcEigenStationary(trans_mat = trans_mat)
  init_state <- sample(x = states,
                       size = 1,
                       replace = TRUE,
                       prob = stat_mat)
  simulations[1,1] <- init_state

  for (i in 2:n_sims){
    prev_step <- simulations[1,(i-1)]
    next_step <- sample(states,
                        size = 1,
                        replace = TRUE,
                        prob = trans_mat[prev_step,])
    simulations[1, i] <- next_step
  }
  return(as.vector(simulations))
}

# Functions for the Estimation of Entropy Rate----------------------------------

#' Compute a Matrix of Transition Counts from an Observed Sequence (First-Order)
#'
#' @param event_seq Vector of observations.
#' @param n_states Total number of expected unique states.
#' @return Matrix \deqn{n_{states} \times n_{states}} representing the number of transitions from each state to all other states.
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 500)
#' tc <- CalcTransitionCounts(sim_mc)
CalcTransitionCounts <- function(event_seq, n_states=length(unique(event_seq))){
  obs_trans <- matrix(nrow = n_states, ncol = n_states, 0)
  for (t in 1:(length(event_seq) - 1)){
    obs_trans[event_seq[t],
              event_seq[t + 1]] <- obs_trans[event_seq[t], event_seq[t + 1]] + 1
  }
  return(obs_trans)
}


#' Compute a Matrix of Transition Counts from an Observed Sequence (Arbitrary Order)
#'
#' @inheritParams CalcTransitionCounts
#' @param state_space vector composed of all of the state space
#' @param mc_order order of the Markov chain.  Defaults to 1.
#'
#' @return Returns a matrix of transition counts of a given order
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 500)
#' tc2 <- CalcTC_Mth_Order(sim_mc, 1:2, mc_order=2)
CalcTC_Mth_Order <- function(event_seq, state_space, mc_order=1){
  n_obs <- length(event_seq)
  n_states <- length(state_space)
  states <- data.frame(matrix(NA, nrow=n_states, ncol=mc_order))
  for(i in 1:mc_order){
    states[,i] <- state_space
  }
  df_args <- c(expand.grid(states), sep=":")
  vector_states <- do.call(paste, df_args)
  obs_trans <- matrix(0, nrow = length(vector_states), ncol = length(vector_states))
  rownames(obs_trans) <- vector_states
  colnames(obs_trans) <- vector_states

  if(mc_order > 1){
    n_new <- n_obs - (mc_order-1)
    mth_order_seq <- rep(NA, n_new)
    for(i in 1:n_new){
      mth_order_seq[i] <- paste(event_seq[i:(i+(mc_order-1))], collapse = ':')
    }
  } else {
    mth_order_seq <- event_seq
  }
  for (t in 1:(length(mth_order_seq) - 1)){
    obs_trans[mth_order_seq[t],
              mth_order_seq[t + 1]] <- obs_trans[mth_order_seq[t],
                                                 mth_order_seq[t + 1]] + 1
  }
  return(obs_trans)
}

#' Calculate the Transition Matrix of a First-Order Markov Chain
#'
#' @param trans_counts Matrix of transition counts
#' @return Row-stochastic transition matrix of a first-order Markov chain.
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 500)
#' tc <- CalcTransitionCounts(sim_mc)
#' tm <- CalcTransitionMatrix(tc)
CalcTransitionMatrix <- function(trans_counts){
  mat_dim = ncol(trans_counts)
  row_totals <- rowSums(trans_counts)
  row_tot_mat <- matrix(rep(row_totals, ncol(trans_counts)),
                        nrow=mat_dim, ncol=mat_dim)
  trans_mat <- trans_counts / row_tot_mat
  trans_mat[trans_mat=="NaN"] <- 0
  return(trans_mat)
}


#' Calculate the Stationary Distribution of a First-Order Markov Chain from its
#' Transition Matrix
#'
#' @param trans_mat Transition matrix
#' @return vector representing the stationary distribution of the Markov chain
#' @examples
#' tm1 <- matrix(c(0,0,1,1,0,0,0,1,0), 3,3, T)
#' sm1 <- CalcEigenStationary(tm1)
#' tm2 <- matrix(c(.3, 0.7, 0.8, 0.2), 2,2, T)
#' sm2 <- CalcEigenStationary(tm2)
CalcEigenStationary <- function(trans_mat){
  tm_eig <- eigen(t(trans_mat))
  if(any(round(Mod(tm_eig$values),10)==1)){
    lamb1 <- which(abs(tm_eig$values-1) == min(abs(tm_eig$values-1)))
    stat_vec <- tm_eig$vectors[,lamb1] / sum(tm_eig$vectors[,lamb1])
    return(Re(stat_vec))
  } else{
    stat_vec <- rep(0, nrow(trans_mat))
    return(stat_vec)
  }
}


#' Calculation of the Empirical Stationary Distribution from an observed Markov Chain (first-order)
#'
#' @param event_seq Observed sequence of events as a Markov chain
#' @param state_space State space of the observed Markov chain
#'
#' @return Estimate of the stationary distribution
#'
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 500)
#' true_sm <- CalcEigenStationary(t_mat)
#' est_sm <- CalcEmpiricalStationary(sim_mc, 1:2)
CalcEmpiricalStationary <- function(event_seq, state_space){
  emp_stat <- matrix(0, nrow = 1, ncol = length(state_space))
  for(i in 1:length(state_space)){
    emp_stat[1,i] <- length(event_seq[event_seq == state_space[i]]) / length(event_seq)
  }
  return(emp_stat)
}

#' Calculation of the Empirical Stationary Distribution from an observed Markov Chain (Arbitrary-Order)
#'
#' @param event_seq Observed sequence of events as a Markov chain
#' @param state_space State space of the observed Markov chain
#' @param mc_order Order of the Markov Chain
#'
#' @return Estimate of the stationary distribution
#'
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 10000)
#' tc2 <- CalcTC_Mth_Order(sim_mc, 1:2, 2)
#' tm2 <- CalcTransitionMatrix(tc2)
#' eig_sm <- CalcEigenStationary(tm2)
#' emp_sm <- CalcEmpStat_Mth_Order(sim_mc, 1:2, 2)
#' print(eig_sm)
#' print(emp_sm)
CalcEmpStat_Mth_Order <- function(event_seq, state_space, mc_order=1){
  n_obs <- length(event_seq)
  n_states <- length(state_space)
  states <- data.frame(matrix(NA, nrow=n_states, ncol=mc_order))
  for(i in 1:mc_order){
    states[,i] <- state_space
  }
  df_args <- c(expand.grid(states), sep=":")
  vector_states <- do.call(paste, df_args)

  emp_stat <- matrix(0, nrow = 1, ncol = length(vector_states))
  colnames(emp_stat) <- vector_states

  if(mc_order > 1){
    n_new <- n_obs - (mc_order-1)
    mth_order_seq <- rep(NA, n_new)
    for(i in 1:n_new){
      mth_order_seq[i] <- paste(event_seq[i:(i+(mc_order-1))], collapse = ':')
    }
  } else {
    mth_order_seq <- event_seq
  }
  for(i in 1:length(vector_states)){
    emp_stat[1,i] <- length(mth_order_seq[mth_order_seq == vector_states[i]]) / length(mth_order_seq)
  }
  return(emp_stat)
}

#' Calculate an estimate of the Entropy Rate of a finite Markov Chain
#'
#' @param trans_mar Transition Matrix of a finite Markov chain
#' @param stat_dist Vector of the stationary distribution of the Markov chain
#'
#' @return Estimate of the Entropy Rate
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 10000)
#' tc2 <- CalcTC_Mth_Order(sim_mc, 1:2, 2)
#' tm2 <- CalcTransitionMatrix(tc2)
#' eig_sm <- CalcEigenStationary(tm2)
#' emp_sm <- CalcEmpStat_Mth_Order(sim_mc, 1:2, 2)
#' CalcMarkovEntropyRate(t_mat, CalcEigenStationary(t_mat))
#' CalcMarkovEntropyRate(tm2, eig_sm)
#' CalcMarkovEntropyRate(tm2, emp_sm)
CalcMarkovEntropyRate <- function(trans_mat, stat_dist){
  n_dim <- length(stat_dist)
  stat_mat <- matrix(rep(stat_dist, n_dim), n_dim, n_dim, byrow=TRUE)
  ent_rate <- -sum(t(stat_mat) * trans_mat * log2(trans_mat), na.rm = TRUE)
  return(ent_rate)
}

#' Calculate the Entropy Rate of a Finite Markov Chain
#'
#' @param event_seq Observed sequence of events
#' @param state_space State space of the observed process
#' @param mc_order Order of the Markov Chain
#' @param stat_method Method for computing the Stationary distribution
#'
#' @return Estimate of the Entropy Rate
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 50000)
#' CalcMarkovEntropyRate(t_mat, CalcEigenStationary(t_mat))
#' CalcEntropyRate(sim_mc, 1:2, mc_order = 1, stat_method="Empirical")
#' CalcEntropyRate(sim_mc, 1:2, mc_order = 1, stat_method="Eigen")
CalcEntropyRate <- function(event_seq,
                            state_space,
                            mc_order = 1,
                            stat_method='Empirical'){
  tc <- CalcTC_Mth_Order(event_seq, state_space, mc_order)
  tm <- CalcTransitionMatrix(tc)
  if(stat_method != 'Empirical'){
    sm <- CalcEigenStationary(tm)
  } else{
    sm <- CalcEmpStat_Mth_Order(event_seq, state_space, mc_order)
  }
  ent <- CalcMarkovEntropyRate(tm, sm)
  return(ent)
}


