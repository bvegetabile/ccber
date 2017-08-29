#-------------------------------------------------------------------------------
#
# Behavioral Entropy Rate Estimation
#
#-------------------------------------------------------------------------------

# Analyzing Files --------------------------------------------------------------
# - ber_analyze_file : calculates statistics, including ER, from a specified file
# - ber_analyze_dir  : calculates statistics, including ER, from a specified dir

#' Estimate Behavioral Entropy Rate based upon Video Data Files for a Directory
#'
#' @param dir_loc directory of files
#' @param tactile_padding right padding adjustment to tactile events
#' @param auditory_padding right padding adjustment to auditory events
#' @param behavior_types dictionary of behavior types.  Required sections, mom_auditory_types, mom_tactile_types, mom_visual_types, baby_visual_types, missing_types
#' @param missing_threshold proportion of acceptable missing time
#'
#' @return Entropy rate estimates of an individual
ber_analyze_dir <- function(dir_loc,
                            tactile_padding = 1.0,
                            auditory_padding = 1.0,
                            behavior_types=list("mom_auditory_types" = c('Vocal'),
                                                "mom_tactile_types" = c('TouchBaby',
                                                                        'HoldingBaby'),
                                                "mom_visual_types" = c('ManipulatingObject'),
                                                "baby_visual_types" = c('LookAtMomActivity'),
                                                "missing_types" = c('CantTellHolding',
                                                                    'ActivityNotVisible',
                                                                    'CantTellLooking')),
                            missing_threshold = 0.1){

  old_dir <- getwd()
  setwd(dir_loc)

  # Reading in data
  all_files = list.files(dir_loc, pattern="*.xlsx")
  n_files = length(all_files)

  # Allocating DataFrame for returning data
  resultsDF <- data.frame('SubjectID'=character(),
                          'CanEstimateEntropy'=logical(),
                          'EntropyRate'=double(),
                          'TotalNumberOfTransitions'=double(),
                          'CombinedVideoDuration'=double(),
                          'PercentMissing'=double(),
                          'AuditoryCounts'=double(),
                          'AuditoryTotalTime'=double(),
                          'AuditoryAverageTime'=double(),
                          'VisualCounts'=double(),
                          'VisualTotalTime'=double(),
                          'VisualAverageTime'=double(),
                          'TactileCounts'=double(),
                          'TactileTotalTime'=double(),
                          'TactileAverageTime'=double(),
                          stringsAsFactors = F)


  # Analysis Section -----------------------------------------------------------

  run_start <- Sys.time()
  run_count <- 1
  for (i in 1:n_files){
    individual_file <- all_files[i]
    resultsDF[i,] <- ber_analyze_file(individual_file,
                                      tactile_padding=tactile_padding,
                                      auditory_padding=auditory_padding,
                                      behavior_types=behavior_types,
                                      missing_threshold=missing_threshold)
    run_count = run_count + 1
    message(paste('COMPLETED ID: ', resultsDF[i,1]))
  }
  run_dur <- Sys.time() - run_start
  message(paste('Script total run time: ', round(as.numeric(run_dur, units='mins'),3), 'minutes'))

  # Resetting the old directory pointer
  setwd(old_dir)

  return(resultsDF)
}

#' Estimate Behavioral Entropy Rate based upon Video Data
#'
#' @param f_loc file location
#' @param plot_all logical: Plot the data to observe the sequence of behaviors
#' @param plots_to_file logical: send all plots to a file
#' @param tactile_padding right padding adjustment to tactile events
#' @param auditory_padding right padding adjustment to auditory events
#' @param behavior_types dictionary of behavior types.  Required sections, mom_auditory_types, mom_tactile_types, mom_visual_types, baby_visual_types, missing_types
#' @param missing_threshold proportion of acceptable missing time
#'
#' @return Entropy rate estimates of an individual
ber_analyze_file <- function(f_loc,
                             plot_all=F,
                             plots_to_file=F,
                             tactile_padding = 1.0,
                             auditory_padding = 1.0,
                             behavior_types=list("mom_auditory_types" = c('Vocal'),
                                                 "mom_tactile_types" = c('TouchBaby',
                                                                         'HoldingBaby'),
                                                 "mom_visual_types" = c('ManipulatingObject'),
                                                 "baby_visual_types" = c('LookAtMomActivity'),
                                                 "missing_types" = c('CantTellHolding',
                                                                     'ActivityNotVisible',
                                                                     'CantTellLooking')),
                             missing_threshold = 0.1){

  # Unpacking input behavior types ---------------------------------------------
  mom_auditory_types <- behavior_types$mom_auditory_types
  mom_tactile_types <- behavior_types$mom_tactile_types
  mom_visual_types <- behavior_types$mom_visual_types
  baby_visual_types <- behavior_types$baby_visual_types
  missing_types <- behavior_types$missing_types

  # extracting data from file using the readxl package
  behavior_data <- data.frame(readxl::read_xlsx(f_loc))


  # Mother ID should be in first cell in the observation column
  id_number <- behavior_data$Observation[1]

  if(sum(is.na(behavior_data$Behavior)) > 0){
    message(message(paste('ERROR: ', id_number, ' - Missing Behavior Information')))
    data2return <- data.frame('SubjectID'=as.character(id_number),
                              'CanEstimateEntropy'=FALSE,
                              'EntropyRate'=NA,
                              'TotalNumberOfTransitions'=NA,
                              'CombinedVideoDuration'=NA,
                              'PercentMissing'=NA,
                              'AuditoryCounts'=NA,
                              'AuditoryTotalTime'=NA,
                              'AuditoryAverageTime'=NA,
                              'VisualCounts'=NA,
                              'VisualTotalTime'=NA,
                              'VisualAverageTime'=NA,
                              'TactileCounts'=NA,
                              'TactileTotalTime'=NA,
                              'TactileAverageTime'=NA,
                              stringsAsFactors = F)
    return(data2return)
  }

  # Identifying the last time between both the mother and baby files
  lasttime = max(behavior_data$Time_Relative_sf)
  lastduration = max(subset(behavior_data,
                            behavior_data$Time_Relative_sf==max(behavior_data$Time_Relative_sf))$Duration_sf)
  endtime = lasttime + lastduration
  ##############################################################################
  # Finding the total amount of missing time
  missing <- .subset_by_types(behavior_data,
                              missing_types)

  percent_missing <- sum(missing$Duration_sf)/endtime

  if(percent_missing>=missing_threshold){
    message(paste('Mother/Child: ', id_number, ' - Not enough usable time'))
    data2return <- data.frame('SubjectID'=as.character(id_number),
                              'CanEstimateEntropy'=FALSE,
                              'EntropyRate'=NA,
                              'TotalNumberOfTransitions'=NA,
                              'CombinedVideoDuration'=endtime,
                              'PercentMissing'=percent_missing,
                              'AuditoryCounts'=NA,
                              'AuditoryTotalTime'=NA,
                              'AuditoryAverageTime'=NA,
                              'VisualCounts'=NA,
                              'VisualTotalTime'=NA,
                              'VisualAverageTime'=NA,
                              'TactileCounts'=NA,
                              'TactileTotalTime'=NA,
                              'TactileAverageTime'=NA,
                              stringsAsFactors = F)
    return(data2return)
  }
  # ##############################################################################
  #
  # ##############################################################################
  # Finding all of the unique events for a given state type
  baby_vis <- .subset_by_types(behavior_data, baby_visual_types)
  mom_vis <- .subset_by_types(behavior_data, mom_visual_types)
  mom_tac <- .subset_by_types(behavior_data, mom_tactile_types, tactile_padding)
  mom_aud <- .subset_by_types(behavior_data, mom_auditory_types, auditory_padding)
  #
  # Reducing these events, either by intersection or union of events
  whole_interval <- intervals::Intervals(c(0,endtime))
  tac_states <- .find_unions(mom_tac, "TACTILE")
  aud_states <- .find_unions(mom_aud, "AUDITORY")
  vis_states <- .compare_intersection(mom_vis, baby_vis, "VISUAL")
  nottac_states <- .state_complement(whole_interval, tac_states, "NOT TACTILE")
  notaud_states <- .state_complement(whole_interval, aud_states, "NOT AUDITORY")
  notvis_states <- .state_complement(whole_interval, vis_states, "NOT VISUAL")
  #
  # Finding each state category i.e. 'L-T-!V' means Looking, touching, but not speaking
  vis_tac_aud <- .find_macrostate(vis_states, tac_states, aud_states, "V-T-A", 8)
  vis_tac_notaud <- .find_macrostate(vis_states, tac_states, notaud_states, "V-T-!A",7)
  vis_nottac_aud <- .find_macrostate(vis_states, nottac_states, aud_states, "V-!T-A",6)
  notvis_tac_aud <- .find_macrostate(notvis_states, tac_states, aud_states, "!V-T-A",5)
  notvis_nottac_aud <- .find_macrostate(notvis_states, nottac_states, aud_states, "!V-!T-A",4)
  notvis_tac_notaud <- .find_macrostate(notvis_states, tac_states, notaud_states, "!V-T-!A",3)
  vis_nottac_notaud <- .find_macrostate(vis_states, nottac_states, notaud_states, "V-!T-!A",2)
  notvis_nottac_notaud <- .find_macrostate(notvis_states, nottac_states, notaud_states, "!V-!T-!A",1)
  alltypes <- rbind(vis_tac_aud,
                    vis_tac_notaud,
                    vis_nottac_aud,
                    notvis_tac_aud,
                    notvis_nottac_aud,
                    notvis_tac_notaud,
                    vis_nottac_notaud,
                    notvis_nottac_notaud)
  state_sequence <- alltypes[with(alltypes, order(Start)), ]
  # ##############################################################################
  #
  # ##############################################################################
  # # Entropy Calculations
  transition_counts <- CalcTransitionCounts(state_sequence[,5], 8)
  transition_matrix <- CalcTransitionMatrix(transition_counts)
  stationary_matrix <- CalcEmpiricalStationary(state_sequence[,5], 1:8)
  entropy_rate <- CalcMarkovEntropyRate(transition_matrix, stationary_matrix)

  numbered_states <- c()
  for(i in 1:nrow(state_sequence)){
    numbered_states <- rbind(numbered_states,
                             c(state_sequence[i,]$Start, state_sequence[i,]$point))
    numbered_states <- rbind(numbered_states,
                             c(state_sequence[i,]$End, state_sequence[i,]$point))
  }

  if(plot_all == T){
    plot_file(endtime=endtime,
              aud_states=aud_states,
              tac_states=tac_states,
              vis_states=vis_states,
              numbered_states=numbered_states,
              id_number=id_number)

    plot_orig(endtime=endtime,
              aud_states=aud_states,
              tac_states=tac_states,
              vis_states=vis_states,
              numbered_states=numbered_states,
              id_number=id_number)

    plot_transformed(endtime=endtime,
                     alltypes=alltypes,
                     id_number=id_number)

    plot_sequence(state_sequence=state_sequence,
                  id_number=id_number)

    plot_counts(transition_counts, id_number)

    plot_transitions(transition_matrix, id_number)
  }



  data2return <- data.frame('SubjectID'=as.character(id_number),
                            'CanEstimateEntropy'=TRUE,
                            'EntropyRate'=entropy_rate,
                            'TotalNumberOfTransitions'=sum(transition_counts),
                            'CombinedVideoDuration'=endtime,
                            'PercentMissing'=percent_missing,
                            'AuditoryCounts'=nrow(aud_states),
                            'AuditoryTotalTime'=sum(aud_states[,3]),
                            'AuditoryAverageTime'=mean(aud_states[,3]),
                            'VisualCounts'=nrow(vis_states),
                            'VisualTotalTime'=sum(vis_states[,3]),
                            'VisualAverageTime'=mean(vis_states[,3]),
                            'TactileCounts'=nrow(tac_states),
                            'TactileTotalTime'=sum(tac_states[,3]),
                            'TactileAverageTime'=mean(tac_states[,3]),
                            stringsAsFactors = F)

  return(data2return)
}


# Plotting Functions: Visualization for individual sequences -------------------

plot_file <- function(endtime=endtime,
                      aud_states=aud_states,
                      tac_states=tac_states,
                      vis_states=vis_states,
                      numbered_states=numbered_states,
                      id_number=id_number){
  par(mar=c(3,10,3,8))
  plot(0, type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,6), ylab="", xlab="")
  for(i in 1:nrow(aud_states)){
    starttime <- aud_states[i,]$Start
    duration <- aud_states[i,]$Duration
    vert <- 5
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(1,0,0,.5))
  }
  for(i in 1:nrow(tac_states)){
    starttime <- tac_states[i,]$Start
    duration <- tac_states[i,]$Duration
    vert <- 4
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,1,.5))
  }
  for(i in 1:nrow(vis_states)){
    starttime <- vis_states[i,]$Start
    duration <- vis_states[i,]$Duration
    vert <- 3
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,1,0,.5))
  }
  lines(numbered_states[,1], 3*(numbered_states[,2]-1)/7)
  axis(1, at=seq(0,endtime,50), las=2)
  axis(2,at=c(1.5, 3.5, 4.5, 5.5), labels=c("State Transition","Visual", "Tactile", "Auditory"), las=2)
  axis(4, at = c(0,3*seq(1,7)/7), labels=c('No State', 'Visual Only', 'Tactile Only', 'Auditory Only',
                                           'Tactile-Auditory', 'Visual-Auditory', 'Visual-Tactile', 'All Three'),
       las=2)
  abline(h=c(0,3,4,5,6))
  abline(h=3*seq(1,7)/7, lty=3, col=rgb(0,0,0,0.2))
  abline(h=3*c(0.5, 3.5, 6.5)/7, lty=2, col=rgb(0,0,0.75,0.6))
  title(paste('File Identifier: ', id_number)) #, ', Entropy: ', round(entropy_rate,4)
}


plot_orig <- function(endtime=endtime,
                      aud_states=aud_states,
                      tac_states=tac_states,
                      vis_states=vis_states,
                      numbered_states=numbered_states,
                      id_number=id_number){
  par(mar=c(4,5,4,4)+0.1)
  plot(0,type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,3), ylab="", xlab="Time")
  for(i in 1:nrow(aud_states)){
    starttime <- aud_states[i,]$Start
    duration <- aud_states[i,]$Duration
    vert <- 2
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  for(i in 1:nrow(tac_states)){
    starttime <- tac_states[i,]$Start
    duration <- tac_states[i,]$Duration
    vert <- 1
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  for(i in 1:nrow(vis_states)){
    starttime <- vis_states[i,]$Start
    duration <- vis_states[i,]$Duration
    vert <- 0
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  axis(1, at=seq(0,endtime,50), las=1)
  axis(2,at=c(0.5, 1.5, 2.5), labels=c("Visual", "Tactile", "Auditory"), las=2)
  abline(h=c(0,1,2, 3))
  title(paste('File Identifier: ', id_number))
}

plot_transformed <- function(endtime=endtime,
                             alltypes=alltypes,
                             id_number=id_number){
  par(mar=c(4,8,4,4)+0.1)
  plot(0,type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,8), ylab="", xlab="Time")
  for(i in 1:nrow(alltypes)){
    starttime <- alltypes[i,]$Start
    duration <- alltypes[i,]$Duration
    vert <- alltypes[i,]$point - 1
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert),
            col = rgb(0,0,0,.5))
  }
  axis(1, at=seq(0,endtime,50), las=1)
  axis(2,at=seq(0.5, 7.5, 1), labels=c("No State",
                                       "Visual",
                                       "Tactile",
                                       "Auditory",
                                       "Auditory/Tactile",
                                       "Auditory/Visual",
                                       "Visual/Tactile",
                                       'All States'), las=2)
  abline(h=0:8)
  title(paste('File Identifier: ', id_number))
}

plot_sequence <- function(state_sequence=state_sequence,
                          id_number=id_number){
  par(mar=c(4,8,4,4)+0.1)
  plot(0,type='n', axes=FALSE, xlim=c(0,nrow(state_sequence)), ylim=c(0,8), ylab="", xlab="Time Index")
  lines(0:(nrow(state_sequence)-1), state_sequence$point-0.5, lty=3, col=rgb(0,0,0,0.5))
  points(0:(nrow(state_sequence)-1), state_sequence$point-0.5, pch=19, col=rgb(0,0,0,0.5))
  abline(h=0:8)
  axis(1, at=seq(0,nrow(state_sequence),50), las=1)
  axis(2,at=seq(0.5, 7.5, 1), labels=c("No State",
                                       "Visual",
                                       "Tactile",
                                       "Auditory",
                                       "Auditory/Tactile",
                                       "Auditory/Visual",
                                       "Visual/Tactile",
                                       'All States'), las=2)
  title(paste('File Identifier: ', id_number))
}

plot_counts <- function(transition_counts, id_number){
  type_labs <- c('NS', 'V', 'T', 'A', 'A/T', 'A/V', 'V/T', 'A/T/V')
  par(mar =c(4,4,4,4)+0.1)
  plot(0, xlim=c(0, 8), ylim=c(0,8),
       pch=19, col=rgb(0,0,0,0), axes=F,
       ylab='From Action', xlab='To Action\n',
       xaxs='i', yaxs='i',
       main=paste('Transition Counts, ID:', id_number))
  abline(h=0:8)
  axis(2, at=seq(0.5, 7.5, 1), labels=type_labs[8:1], las=1)
  axis(1, at=seq(0.5, 7.5, 1), labels=type_labs[1:8], las=1)
  axis(4, at=seq(0.5, 7.5, 1)[8:1], labels=rowSums(transition_counts), las=1)
  abline(v=0:8)
  exes <- seq(0.5, 7.5, 1)
  whys <- seq(7.5, 0.5, -1)
  for(j in 1:8){
    for(i in 1:8){
      text(exes[i], whys[j], labels = transition_counts[j, i])
    }
  }
}

plot_transitions <- function(transition_matrix, id_number){
  type_labs <- c('NS', 'V', 'T', 'A', 'A/T', 'A/V', 'V/T', 'A/T/V')
  par(mar =c(4,4,4,4)+0.1)
  plot(0, xlim=c(0, 8), ylim=c(0,8),
       pch=19, col=rgb(0,0,0,0), axes=F,
       ylab='From Action', xlab='To Action\n',
       xaxs='i', yaxs='i',
       main=paste('Transition Probabilities, ID:', id_number))
  abline(h=0:8)
  axis(2, at=seq(0.5, 7.5, 1), labels=type_labs[8:1], las=1)
  axis(1, at=seq(0.5, 7.5, 1), labels=type_labs[1:8], las=1)
  abline(v=0:8)
  exes <- seq(0.5, 7.5, 1)
  whys <- seq(7.5, 0.5, -1)
  for(j in 1:8){
    for(i in 1:8){
      polygon(c(exes[i]-0.5, exes[i]-0.5, exes[i]+0.5, exes[i]+0.5),
              c(whys[j]-0.5, whys[j]+0.5, whys[j]+0.5, whys[j]-0.5),
              col=rgb(0,0,0.75, round(transition_matrix[j, i],2)))
      text(exes[i], whys[j], labels = round(transition_matrix[j, i],2))
    }
  }
}


# Required Functions: Creating time sequences from specified intervals ---------

.subset_by_types <- function(dset, event_str, padding=0.0){
  dat <- c()
  for(i in 1:length(event_str)){
    dat <- rbind(dat, dset[(dset$Behavior == event_str[i]) &
                             ((dset$Event_Type =='State start') | (dset$Event_Type =='State point') | (dset$Event_Type =='Point')) ,
                           c("Time_Relative_sf",'Duration_sf')])
  }
  dat$Duration_sf[dat$Duration_sf == 0] <- padding
  return(dat)
}

.compare_intersection <-  function (mother, baby, statetype=""){
  babyints <- intervals::Intervals(cbind(baby[,1], baby[,1]+baby[,2]))
  momints <- intervals::Intervals(cbind(mother[,1], mother[,1]+mother[,2]))
  mombaby_intersection <- intervals::interval_intersection(babyints, momints)
  if(length(mombaby_intersection)==0){
    mombaby_intersection <- data.frame(c(0),c(0))
  }
  mombaby_intersection <- data.frame(mombaby_intersection)
  names(mombaby_intersection) <- c('Start', 'End')
  mombaby_intersection$Duration <- mombaby_intersection$End - mombaby_intersection$Start
  mombaby_intersection$State <- statetype
  return(mombaby_intersection)
}

.find_unions <- function(times, statetype=""){
  ints <- intervals::Intervals(cbind(times[,1], times[,1]+times[,2]))

  allunions <- intervals::interval_union(ints)

  if(length(allunions)==0){
    allunions <- data.frame(c(0),c(0))
  }
  allunions <- data.frame(allunions)
  names(allunions) <- c('Start', 'End')
  allunions$Duration <- allunions$End - allunions$Start
  allunions$State <- statetype
  return(allunions)
}

.state_complement<- function(whole_interval, states, statetype=""){
  state_comp <- suppressWarnings(intervals::interval_difference(whole_interval, intervals::Intervals(states[,c(1,2)])))
  if(length(state_comp)==0){
    state_comp <- data.frame(c(0),c(0))
  }
  state_comp <- data.frame(state_comp)
  names(state_comp) <- c('Start', 'End')
  state_comp$Duration <- state_comp$End - state_comp$Start
  state_comp$State <- statetype
  return(state_comp)
}

.find_macrostate <- function(look, touch, vocal, tag="", pt=0){
  look_ints <- intervals::Intervals(look[,c(1,2)])
  touch_ints <- intervals::Intervals(touch[,c(1,2)])
  vocal_ints <- intervals::Intervals(vocal[,c(1,2)])
  step1 <- intervals::interval_intersection(look_ints, touch_ints)
  step2 <- intervals::interval_intersection(vocal_ints, step1)
  if(length(step2)==0){
    triple_intersection <- data.frame(c(0),c(0))
  } else{
    triple_intersection <- data.frame(step2)
  }
  names(triple_intersection) <- c('Start', 'End')
  triple_intersection$Duration <- triple_intersection$End - triple_intersection$Start
  triple_intersection$State <- tag
  triple_intersection$point <- pt
  return(triple_intersection[triple_intersection$Duration!=0.0,])
}
