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

#' Compute a Matrix of Transition Counts from an Observed Sequence
#'
#' @param event_seq Vector of observations.
#' @param n_states Total number of expected unique states.
#' @return Matrix representing the number of transitions from each state to all other states.
#'@examples
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


#####
# Calculation of the empirical estimate of the transition matrix of a
# finite Markov Chain.

CalcTransitionMatrix <- function(trans_counts){
  mat_dim = ncol(trans_counts)
  row_totals <- rowSums(trans_counts)
  row_tot_mat <- matrix(rep(row_totals, ncol(trans_counts)),
                        nrow=mat_dim, ncol=mat_dim)
  trans_mat <- trans_counts / row_tot_mat
  trans_mat[trans_mat=="NaN"] <- 0
  return(trans_mat)
}

#####
# Calculation of the Eigenvalue Decomposition of the Transition Matrix as an
# estimate of the Stationary Distribution
#
# @examples
# > tm <- matrix(c(0,0,1,1,0,0,0,1,0),3,3,TRUE)
# > CalcEigenStationary(tm)
# [1] 0.3333333 0.3333333 0.3333333

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

#####
# Calculation of the Empirical estimate of the Stationary Distribution

CalcEmpiricalStationary <- function(m_chain, state_space){
  emp_stat <- matrix(0, nrow = 1, ncol = length(state_space))
  for(i in 1:length(state_space)){
    emp_stat[1,i] <- length(m_chain[m_chain == state_space[i]]) / length(m_chain)
  }
  return(emp_stat)
}

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

#####
# Calculation of the estimate of the entropy rate of a finite Markov Chain.

CalcMarkovEntropyRate <- function(trans_mat, stat_mat){
  n_dim <- length(stat_mat)
  stat_mat <- matrix(rep(stat_mat, n_dim), n_dim, n_dim, byrow=TRUE)
  ent_rate <- -sum(t(stat_mat) * trans_mat * log2(trans_mat), na.rm = TRUE)
  return(ent_rate)
}

################################################################################
#
# Function to which organizes all other functions to calculate the entropy rate
#

CalcEntropyRate <- function(event_seq,
                            state_space,
                            method='Markov',
                            mc_order = 1,
                            stat_method='Empirical'){
  if(method == 'Markov'){
    tc <- CalcTC_Mth_Order(event_seq, state_space, mc_order)
    tm <- CalcTransitionMatrix(tc)
    if(stat_method != 'Empirical'){
      sm <- CalcEigenStationary(tm)
    } else{
      sm <- CalcEmpStat_Mth_Order(event_seq, state_space, mc_order)
    }
    ent <- CalcMarkovEntropyRate(tm, sm)
    return(ent)
  } else {
    message('Error: Not a valid entropy rate estimation method -> Choose "Markov" or "SWLZ".')
    return(NULL)
  }
}


