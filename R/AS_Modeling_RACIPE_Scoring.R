
topology_AS <- read.table("test.txt", header = T)
sracipePlotCircuit(rSet_AS_50000, plotToFile = F)

#rSet_AS_50000 <- sracipeSimulate(topology_AS, integrate = TRUE, numModels = 50000)
#sracipePlotData(rSet_AS_50000, plotToFile = T)

OE_rows <- which(sracipeParams(rSet_AS_50000)[,"G_CH"] > 50 & sracipeParams(rSet_AS_50000)[,"G_PD"] > 50 & sracipeParams(rSet_AS_50000)[,"G_LIF"] > 50, arr.ind = T)

rSet_AS_50000_expression <- t(assay(rSet_AS_50000))

rSet_AS_50000_expression_log <- log2(rSet_AS_50000_expression)

means_log_50000 <- colMeans(rSet_AS_50000_expression_log)
sds_log_50000 <- apply(rSet_AS_50000_expression_log, 2, sd)

rSet_AS_50000_expression_log_z <- sweep(rSet_AS_50000_expression_log, 
                                        2, means_log_50000, FUN = "-")

rSet_AS_50000_expression_log_z <- sweep(rSet_AS_50000_expression_log_z, 
                                        2, sds_log_50000, FUN = "/")


distance_euc_40000 <- dist(rSet_AS_50000_expression_log_z[1:40000, ])
clusters_euc_D2_40000 <- hclust(distance_euc_40000, method = "ward.D2")
assignedClusters_euc_D2_40000 <- cutree(clusters_euc_D2_40000, 4)

states_list <- list(which(assignedClusters_euc_D2_40000 == 1), which(assignedClusters_euc_D2_40000 == 2), which(assignedClusters_euc_D2_40000 == 3), which(assignedClusters_euc_D2_40000 == 4))

rSet_AS_40000_expression_log_z <- rSet_AS_50000_expression_log_z[1:40000,]
rSet_AS_40000_expression_log_z_binarized <- rSet_AS_50000_expression_log_z[1:40000,]

for (i in 1:ncol(rSet_AS_40000_expression_log_z)) {
  
  binarized <- binarize.kMeans(rSet_AS_40000_expression_log_z[,i], dip.test = TRUE)
  rSet_AS_40000_expression_log_z_binarized[, i] <- binarized@binarizedMeasurements
  
}
dim(unique(rSet_AS_40000_expression_log_z_binarized))


round(colMeans(rSet_AS_40000_expression_log_z_binarized[which(assignedClusters_euc_D2_40000 == 1),]))
round(colMeans(rSet_AS_40000_expression_log_z_binarized[which(assignedClusters_euc_D2_40000 == 2),]))
round(colMeans(rSet_AS_40000_expression_log_z_binarized[which(assignedClusters_euc_D2_40000 == 3),]))
round(colMeans(rSet_AS_40000_expression_log_z_binarized[which(assignedClusters_euc_D2_40000 == 4),]))

for (i in 1:ncol(rSet_AS_50000_expression_log_z)) {
  
  plot <- ggplot2::ggplot(data = data.frame(expression = rSet_AS_40000_expression_log_z[, i]), aes(x = expression)) + 
    geom_density() + geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[which(assignedClusters_euc_D2_40000 == 1), i]), aes(x = expression), color = "red") + 
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[which(assignedClusters_euc_D2_40000 == 2), i]), aes(x = expression), color = "green") +
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[which(assignedClusters_euc_D2_40000 == 3), i]), aes(x = expression), color = "blue") +
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[which(assignedClusters_euc_D2_40000 == 4), i]), aes(x = expression), color = "yellow") +
    # geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[UE_rows_40, ][, i]), aes(x = expression), color = "purple") +
    labs(title = paste(colnames(rSet_AS_40000_expression_log_z)[i]))
  
  print(plot)
}

rSet_AS_40000_params <- sracipeParams(rSet_AS_50000)[1:40000,]

signals_111 <- which(rSet_AS_40000_params[,"G_CH"] > 65 & rSet_AS_40000_params[,"G_PD"] > 65 & rSet_AS_40000_params[,"G_LIF"] > 65, arr.ind = T)

signals_permutations <- as.matrix(expand.grid(lapply(numeric(length(c(1,2,3))), function(x) c(0,1))), ncol=length(c(1,2,3)))
colnames(signals_permutations) <- c("CH", "PD", "LIF")
signal_combinations_list <- vector(mode = "list", length = nrow(signals_permutations))

for (i in 1:length(signal_combinations_list)) {
  
  if (signals_permutations[i,1] == 1) { 
    
    G_CH <- rSet_AS_40000_params[,"G_CH"] > 65
    
  } else {
    
    G_CH <- rSet_AS_40000_params[,"G_CH"] < 35
    
  }
  
  if (signals_permutations[i,2] == 1) { 
    
    G_PD <- rSet_AS_40000_params[,"G_PD"] > 65
    
  } else {
    
    G_PD <- rSet_AS_40000_params[,"G_PD"] < 35
    
  }
  
  if (signals_permutations[i,3] == 1) { 
    
    G_LIF <- rSet_AS_40000_params[,"G_LIF"] > 65
    
  } else {
    
    G_LIF <- rSet_AS_40000_params[,"G_LIF"] < 35
    
  }
  
  signal_combinations_list[[i]] <-which(G_CH & G_PD & G_LIF, arr.ind = T)
  names(signal_combinations_list)[i] <- paste(signals_permutations[i,], collapse = "")
  print(length(signal_combinations_list[[i]]))
}

for (i in 1:ncol(rSet_AS_40000_expression_log_z)) {
  
  plot <- ggplot2::ggplot(data = data.frame(expression = rSet_AS_40000_expression_log_z[, i]), aes(x = expression)) + 
    geom_density() + geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[signal_combinations_list[[1]], i]), aes(x = expression), color = "red") + 
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[signal_combinations_list[[2]], i]), aes(x = expression), color = "green") +
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[signal_combinations_list[[3]], i]), aes(x = expression), color = "purple") +
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[signal_combinations_list[[4]], i]), aes(x = expression), color = "orange") + 
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[signal_combinations_list[[5]], i]), aes(x = expression), color = "grey") + 
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[signal_combinations_list[[6]], i]), aes(x = expression), color = "pink") +
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[signal_combinations_list[[7]], i]), aes(x = expression), color = "blue") +
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[signal_combinations_list[[8]], i]), aes(x = expression), color = "yellow") +
    labs(title = paste(colnames(rSet_AS_40000_expression_log_z)[i]))
  
  print(plot)
}

overlap_list <- vector(mode = "list", length = 4)

for (j in 1:4) {
  
  overlap_matrix <- matrix(ncol = 4, nrow = 8)
  colnames(overlap_matrix) <- c("Signal_pattern","Models", "PercentofState", "PercentofallModels")
  
  for (k in 1:8) {
    
    overlap_matrix[k,1] <- as.numeric(paste(signals_permutations[k,], collapse = ""))
    overlap_matrix[k,2] <- length(intersect(which(assignedClusters_euc_D2_40000 == j), signal_combinations_list[[k]]))
    overlap_matrix[k,3] <- overlap_matrix[k,2]/length(which(assignedClusters_euc_D2_40000 == j))*100
    overlap_matrix[k,4] <- overlap_matrix[k,2]/length(assignedClusters_euc_D2_40000)*100
  }
  
  overlap_matrix <- overlap_matrix[order(overlap_matrix[,"Models"]),]
  overlap_list[[j]] <- overlap_matrix
}
# intersect(which(assignedClusters_euc_D2_40000 == 1), signal_combinations_list[[1]])

lapply(overlap_list, colSums)

Reduce("+",lapply(signal_combinations_list, length))
3936 + 2886 + 3669 + 3188

for (i in 1:ncol(rSet_AS_40000_expression_log_z)) {
  
  plot <- ggplot2::ggplot(data = data.frame(expression = rSet_AS_40000_expression_log_z[, i]), aes(x = expression)) + 
    geom_density() + geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[signal_combinations_list[[1]], i]), aes(x = expression), color = "red") +
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[signal_combinations_list[[5]], i]), aes(x = expression), color = "blue") +
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[which(assignedClusters_euc_D2_40000 == 2), i]), aes(x = expression), color = "green", linetype = "dashed") +
    geom_density(data = data.frame(expression = rSet_AS_40000_expression_log_z[which(assignedClusters_euc_D2_40000 == 1), i]), aes(x = expression), color = "orange", linetype = "dashed") +
    labs(title = paste(colnames(rSet_AS_40000_expression_log_z)[i]))
  
  print(plot)
}

transition_matrix <- matrix(nrow = length(signal_combinations_list), ncol = length(states_list))
rownames(transition_matrix) <- 1:length(signal_combinations_list)

for (i in 1:nrow(transition_matrix)) {
  rownames(transition_matrix)[i] <- paste(signals_permutations[i,], collapse = "")
  
  for (j in 1:ncol(transition_matrix)) {
    
    transition_matrix[i,j] <- length(signal_combinations_list[[i]])/length(states_list[[j]])
    
  }
  
}

condition_range_models <- sort(Reduce(c, signal_combinations_list)) #unlist(signal_combinations_list)

length(states_list[[1]])
length(states_list[[2]])
length(states_list[[3]])
length(states_list[[4]])

length(condition_range_models)
for (i in 1:nrow(transition_matrix)) {
  rownames(transition_matrix)[i] <- paste(signals_permutations[i,], collapse = "")
  
  for (j in 1:ncol(transition_matrix)) {
    
    transition_matrix[i,j] <- length(base::intersect(signal_combinations_list[[i]], states_list[[j]]))/length(base::intersect(condition_range_models, states_list[[j]]))*100
    transition_matrix
  }
  
}

length(signal_combinations_list[[1]])/length(base::intersect(condition_range_models, states_list[[1]]))



length(signal_combinations_list[[1]])/length(base::intersect(condition_range_models, states_list[[1]]))

Robustness <- function(reference, normalized_expression_matrix, specific_vector = NULL) {
  
  binarized_expression <- normalized_expression_matrix
  
  robustness_list <- vector(mode = "list", length = 2)  
  models_list <- vector(mode = "list", length = 1)
  score_matrix <- matrix(ncol = 2, nrow = 1)
  
  if (!is.null(specific_vector)) {
    
    score_matrix <- matrix(ncol = 4, nrow = 1)
    
  }
  
  for (i in 1:ncol(normalized_expression_matrix)) {
    
    binarized <- binarize.kMeans(normalized_expression_matrix[,i], dip.test = TRUE)
    binarized_expression[, i] <- binarized@binarizedMeasurements
    
  }
  
  identical_models <- which(apply(binarized_expression, 1, function(x) all.equal(x, reference, check.names = FALSE) == "TRUE"  ))
  models_list[[1]] <- identical_models
  score_matrix[1,1] <- length(identical_models)
  score_matrix[1,2] <- length(identical_models)/nrow(binarized_expression)*100
  
  if (!is.null(specific_vector)) { 
    
    identical_models_specific <- which(apply(binarized_expression[specific_vector,], 1, function(x) all.equal(x, reference, check.names = FALSE) == "TRUE"  ))
    models_list <- list.append(models_list,identical_models_specific )
    score_matrix[1,3] <- length(identical_models_specific)
    score_matrix[1,4] <- length(identical_models_specific)/length(specific_vector)*100
    
  }
  robustness_list[[1]] <- score_matrix
  robustness_list[[2]] <- models_list
  names(robustness_list) <- c("scores", "models")
  robustness_list
}

CHPDLIF_111_vector <- c(1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1)
names(CHPDLIF_111_vector) <- colnames(rSet_AS_40000_expression_log_z)
CHPDLIF_111_models_specific <- Robustness(CHPDLIF_111_vector, rSet_AS_40000_expression_log_z, specific_vector = signal_combinations_list$`111`) 


CHPDLIF_110_vector <- c(0,1,0,1,1,1,0,1,0,1,1,1,1,0,1,0)
names(CHPDLIF_110_vector) <- colnames(rSet_AS_40000_expression_log_z)
CHPDLIF_110_models_specific <- Robustness(CHPDLIF_110_vector, rSet_AS_40000_expression_log_z, specific_vector = signal_combinations_list$`110`) 


CHPDLIF_101_vector <- c(1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1)
names(CHPDLIF_101_vector) <- colnames(rSet_AS_40000_expression_log_z)
CHPDLIF_101_models_specific <- Robustness(CHPDLIF_101_vector, rSet_AS_40000_expression_log_z, specific_vector = signal_combinations_list$`101`) 

CHPDLIF_011_vector <- c(1,0,1,1,1,1,0,0,1,0,0,1,0,1,1,1)
names(CHPDLIF_011_vector) <- colnames(rSet_AS_40000_expression_log_z)
CHPDLIF_011_models_specific <- Robustness(CHPDLIF_011_vector, rSet_AS_40000_expression_log_z, specific_vector = signal_combinations_list$`011`) 

CHPDLIF_000_vector <- c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)
names(CHPDLIF_000_vector) <- colnames(rSet_AS_40000_expression_log_z)
CHPDLIF_000_models_specific <- Robustness(CHPDLIF_000_vector, rSet_AS_40000_expression_log_z, specific_vector = signal_combinations_list$`000`) 


round(colMeans(rSet_AS_40000_expression_log_z_binarized[which(assignedClusters_euc_D2_40000 == 1),]))
round(colMeans(rSet_AS_40000_expression_log_z_binarized[which(assignedClusters_euc_D2_40000 == 2),]))
round(colMeans(rSet_AS_40000_expression_log_z_binarized[which(assignedClusters_euc_D2_40000 == 3),]))
round(colMeans(rSet_AS_40000_expression_log_z_binarized[which(assignedClusters_euc_D2_40000 == 4),]))
