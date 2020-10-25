#0.01
testest <- sracipeSimulate(topology_AS, integrate = TRUE, numModels = 250)

rSet_AS_signals_oe_250 <- sracipeSimulate(topology_AS, integrate = FALSE, numModels = 250)

rSet_AS_parameters_values_250 <- sracipeParams(rSet_AS_signals_oe_250)


rSet_AS_parameters_values_250[, c("G_CH", "G_PD", "G_LIF")] <- cbind(runif(250, 90, 100), runif(250, 90, 100), runif(250, 90, 100))
head(rSet_AS_parameters_values_250[, c("G_CH", "G_PD", "G_LIF")])

sracipeParams(rSet_AS_signals_oe_250) <- rSet_AS_parameters_values_250

rSet_AS_signals_oe_250_01 <- sracipeSimulate(rSet_AS_signals_oe_250, integrate = TRUE, genParams = F, integrateStepSize = .01, numModels = 250)

rSet_AS_signals_oe_expression_250_01 <- assay(rSet_AS_signals_oe_250_01)

rSet_AS_signals_oe_expression_250_01_t <- t(rSet_AS_signals_oe_expression_250_01)

rSet_AS_signals_oe_expression_250_01_t_filtered <- rSet_AS_signals_oe_expression_250_01_t[complete.cases(rSet_AS_signals_oe_expression_250_01_t), ] #remove NAs

rSet_AS_signals_oe_expression_250_01_t_filtered <- rSet_AS_signals_oe_expression_250_01_t_filtered[(rowSums(rSet_AS_signals_oe_expression_250_01_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_250_01_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_250_01_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_250_01_t_filtered)[i]))
}

test_01 <- rSet_AS_signals_oe_expression_250_01_t > 1000
head(rSet_AS_signals_oe_expression_250_01_t > 1000)
test_01 <- if_na(test_01, "NAN", label = NULL)

sum(as.character(test_01) != "FALSE")


#0.005
rSet_AS_signals_oe_250_005 <- sracipeSimulate(rSet_AS_signals_oe_250, integrate = TRUE, genParams = F, integrateStepSize = .005, numModels = 250)

rSet_AS_signals_oe_expression_250_005 <- assay(rSet_AS_signals_oe_250_005)

rSet_AS_signals_oe_expression_250_005_t <- t(rSet_AS_signals_oe_expression_250_005)

rSet_AS_signals_oe_expression_250_005_t_filtered <- rSet_AS_signals_oe_expression_250_005_t[complete.cases(rSet_AS_signals_oe_expression_250_005_t), ] #remove NAs

rSet_AS_signals_oe_expression_250_005_t_filtered <- rSet_AS_signals_oe_expression_250_005_t_filtered[(rowSums(rSet_AS_signals_oe_expression_250_005_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_250_005_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_250_005_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_250_005_t_filtered)[i]))
}

test_005 <- rSet_AS_signals_oe_expression_250_005_t > 1000
head(rSet_AS_signals_oe_expression_250_005_t > 1000)
test_005 <- if_na(test_005, "NAN", label = NULL)

sum(as.character(test_005) != "FALSE")

#0.001
rSet_AS_signals_oe_250_001 <- sracipeSimulate(rSet_AS_signals_oe_250, integrate = TRUE, genParams = F, integrateStepSize = .001, numModels = 250)

rSet_AS_signals_oe_expression_250_001 <- assay(rSet_AS_signals_oe_250_001)

rSet_AS_signals_oe_expression_250_001_t <- t(rSet_AS_signals_oe_expression_250_001)

rSet_AS_signals_oe_expression_250_001_t_filtered <- rSet_AS_signals_oe_expression_250_001_t[complete.cases(rSet_AS_signals_oe_expression_250_001_t), ] #remove NAs

rSet_AS_signals_oe_expression_250_001_t_filtered <- rSet_AS_signals_oe_expression_250_001_t_filtered[(rowSums(rSet_AS_signals_oe_expression_250_001_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_250_001_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_250_001_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_250_001_t_filtered)[i]))
}

test_001 <- rSet_AS_signals_oe_expression_250_001_t > 1000
head(rSet_AS_signals_oe_expression_250_001_t > 1000)
test_001 <- if_na(test_001, "NAN", label = NULL)

sum(as.character(test_001) != "FALSE")


#DP
rSet_AS_signals_oe_250_DP <- sracipeSimulate(rSet_AS_signals_oe_250, integrate = TRUE, genParams = F, stepper = "DP", numModels = 250)

rSet_AS_signals_oe_expression_250_DP <- assay(rSet_AS_signals_oe_250_DP)

rSet_AS_signals_oe_expression_250_DP_t <- t(rSet_AS_signals_oe_expression_250_DP)

test_DP <- rSet_AS_signals_oe_expression_250_DP_t > 1000
head(rSet_AS_signals_oe_expression_250_DP_t > 1000)

rSet_AS_signals_oe_expression_250_DP_t_filtered <- rSet_AS_signals_oe_expression_250_DP_t[complete.cases(rSet_AS_signals_oe_expression_250_DP_t), ] #remove NAs

rSet_AS_signals_oe_expression_250_DP_t_filtered <- rSet_AS_signals_oe_expression_250_DP_t_filtered[(rowSums(rSet_AS_signals_oe_expression_250_DP_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_250_DP_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_250_DP_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_250_DP_t_filtered)[i]))
}

test_DP <- if_na(test_DP, "NAN", label = NULL)
head(rSet_AS_signals_oe_expression_250_DP_t)
sum(as.character(test_DP) != "FALSE")
# 
# 
# rSet_AS_signals_oe_005 <- sracipeSimulate(topology_AS, integrate = FALSE, numModels = 250)
# 
# rSet_AS_parameters_values_005 <- sracipeParams(rSet_AS_signals_oe_005)
# 
# 
# rSet_AS_parameters_values_005[, c("G_CH", "G_PD", "G_LIF")] <- cbind(runif(250, 90, 100), runif(250, 90, 100), runif(250, 90, 100))
# head(rSet_AS_parameters_values_005[, c("G_CH", "G_PD", "G_LIF")])
# 
# sracipeParams(rSet_AS_signals_oe_005) <- rSet_AS_parameters_values_005
# 
# rSet_AS_signals_oe_005 <- sracipeSimulate(rSet_AS_signals_oe_005, integrate = TRUE, genParams = F, integrateStepSize = .005, numModels = 250)
# 
# rSet_AS_signals_oe_expression_005 <- assay(rSet_AS_signals_oe_005)
# 
# rSet_AS_signals_oe_expression_005_t <- t(rSet_AS_signals_oe_expression_005)
# 
# test_005 <- rSet_AS_signals_oe_expression_005_t > 1000
# head(rSet_AS_signals_oe_expression_005_t > 1000)
# test_005 <- if_na(test_005, "NAN", label = NULL)
# 
# sum(as.character(test_005) != "FALSE")
# 
# #0.001
# rSet_AS_signals_oe_001 <- sracipeSimulate(topology_AS, integrate = FALSE, numModels = 250)
# 
# rSet_AS_parameters_values_001 <- sracipeParams(rSet_AS_signals_oe_001)
# 
# 
# rSet_AS_parameters_values_001[, c("G_CH", "G_PD", "G_LIF")] <- cbind(runif(250, 90, 100), runif(250, 90, 100), runif(250, 90, 100))
# head(rSet_AS_parameters_values_001[, c("G_CH", "G_PD", "G_LIF")])
# 
# sracipeParams(rSet_AS_signals_oe_001) <- rSet_AS_parameters_values_001
# 
# rSet_AS_signals_oe_001 <- sracipeSimulate(rSet_AS_signals_oe_001, integrate = TRUE, genParams = F, integrateStepSize = .001, numModels = 250)
# 
# rSet_AS_signals_oe_expression_001 <- assay(rSet_AS_signals_oe_001)
# 
# rSet_AS_signals_oe_expression_001_t <- t(rSet_AS_signals_oe_expression_001)
# 
# test_001 <- rSet_AS_signals_oe_expression_001_t > 1000
# head(rSet_AS_signals_oe_expression_001_t > 1000)
# test_001 <- if_na(test_001, "NAN", label = NULL)
# 
# sum(as.character(test_001) != "FALSE")

#0.01
testest <- sracipeSimulate(topology_AS, integrate = TRUE, numModels = 2000)

rSet_AS_signals_oe_2000 <- sracipeSimulate(topology_AS, integrate = FALSE, numModels = 2000)

rSet_AS_parameters_values_2000 <- sracipeParams(rSet_AS_signals_oe_2000)


rSet_AS_parameters_values_2000[, c("G_CH", "G_PD", "G_LIF")] <- cbind(runif(2000, 90, 100), runif(2000, 90, 100), runif(2000, 90, 100))
head(rSet_AS_parameters_values_2000[, c("G_CH", "G_PD", "G_LIF")])

sracipeParams(rSet_AS_signals_oe_2000) <- rSet_AS_parameters_values_2000

rSet_AS_signals_oe_2000_01 <- sracipeSimulate(rSet_AS_signals_oe_2000, integrate = TRUE, genParams = F, integrateStepSize = .01, numModels = 2000)

rSet_AS_signals_oe_expression_2000_01 <- assay(rSet_AS_signals_oe_2000_01)

rSet_AS_signals_oe_expression_2000_01_t <- t(rSet_AS_signals_oe_expression_2000_01)

rSet_AS_signals_oe_expression_2000_01_t_filtered <- rSet_AS_signals_oe_expression_2000_01_t[complete.cases(rSet_AS_signals_oe_expression_2000_01_t), ] #remove NAs

rSet_AS_signals_oe_expression_2000_01_t_filtered <- rSet_AS_signals_oe_expression_2000_01_t_filtered[(rowSums(rSet_AS_signals_oe_expression_2000_01_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_01_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_2000_01_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_01_t_filtered)[i]))
}

test_01 <- rSet_AS_signals_oe_expression_2000_01_t > 1000
head(rSet_AS_signals_oe_expression_2000_01_t > 1000)
test_01 <- if_na(test_01, "NAN", label = NULL)

sum(as.character(test_01) != "FALSE")


#0.005
rSet_AS_signals_oe_2000_005 <- sracipeSimulate(rSet_AS_signals_oe_2000, integrate = TRUE, genParams = F, integrateStepSize = .005, numModels = 2000)

rSet_AS_signals_oe_expression_2000_005 <- assay(rSet_AS_signals_oe_2000_005)

rSet_AS_signals_oe_expression_2000_005_t <- t(rSet_AS_signals_oe_expression_2000_005)

rSet_AS_signals_oe_expression_2000_005_t_filtered <- rSet_AS_signals_oe_expression_2000_005_t[complete.cases(rSet_AS_signals_oe_expression_2000_005_t), ] #remove NAs

rSet_AS_signals_oe_expression_2000_005_t_filtered <- rSet_AS_signals_oe_expression_2000_005_t_filtered[(rowSums(rSet_AS_signals_oe_expression_2000_005_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_005_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_2000_005_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_005_t_filtered)[i]))
}

test_005 <- rSet_AS_signals_oe_expression_2000_005_t > 1000
head(rSet_AS_signals_oe_expression_2000_005_t > 1000)
test_005 <- if_na(test_005, "NAN", label = NULL)

sum(as.character(test_005) != "FALSE")

#0.001
rSet_AS_signals_oe_2000_001 <- sracipeSimulate(rSet_AS_signals_oe_2000, integrate = TRUE, genParams = F, integrateStepSize = .001, numModels = 2000)

rSet_AS_signals_oe_expression_2000_001 <- assay(rSet_AS_signals_oe_2000_001)

rSet_AS_signals_oe_expression_2000_001_t <- t(rSet_AS_signals_oe_expression_2000_001)

rSet_AS_signals_oe_expression_2000_001_t_filtered <- rSet_AS_signals_oe_expression_2000_001_t[complete.cases(rSet_AS_signals_oe_expression_2000_001_t), ] #remove NAs

rSet_AS_signals_oe_expression_2000_001_t_filtered <- rSet_AS_signals_oe_expression_2000_001_t_filtered[(rowSums(rSet_AS_signals_oe_expression_2000_001_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_001_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_2000_001_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_001_t_filtered)[i]))
}

test_001 <- rSet_AS_signals_oe_expression_2000_001_t > 1000
head(rSet_AS_signals_oe_expression_2000_001_t > 1000)
test_001 <- if_na(test_001, "NAN", label = NULL)

sum(as.character(test_001) != "FALSE")


#DP
rSet_AS_signals_oe_2000_DP <- sracipeSimulate(rSet_AS_signals_oe_2000, integrate = TRUE, genParams = F, stepper = "DP", numModels = 2000)

rSet_AS_signals_oe_expression_2000_DP <- assay(rSet_AS_signals_oe_2000_DP)

rSet_AS_signals_oe_expression_2000_DP_t <- t(rSet_AS_signals_oe_expression_2000_DP)

test_DP <- rSet_AS_signals_oe_expression_2000_DP_t > 1000
head(rSet_AS_signals_oe_expression_2000_DP_t > 1000)

rSet_AS_signals_oe_expression_2000_DP_t_filtered <- rSet_AS_signals_oe_expression_2000_DP_t[complete.cases(rSet_AS_signals_oe_expression_2000_DP_t), ] #remove NAs

rSet_AS_signals_oe_expression_2000_DP_t_filtered <- rSet_AS_signals_oe_expression_2000_DP_t_filtered[(rowSums(rSet_AS_signals_oe_expression_2000_DP_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_DP_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_2000_DP_t_filtered[, i], breaks = 100, main = paste(colnames(rSet_AS_signals_oe_expression_2000_DP_t_filtered)[i]))
}

test_DP <- if_na(test_DP, "NAN", label = NULL)
head(rSet_AS_signals_oe_expression_2000_DP_t)
sum(as.character(test_DP) != "FALSE")

variabless <- 1:4
as.matrix(expand.grid(lapply(numeric(length(variabless)), function(x) c(0,1,2))), ncol=length(variabless))
