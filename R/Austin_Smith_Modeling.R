####Austin Smith_011_KD_Esrrb####

SavePlots = TRUE
SaveCSVs = TRUE

#GENERATING ARGUMENTS

#generating vectors
VariableLogicTxt <- read.delim(file = "/Users/gordid/Desktop/ASmithNetworkLogic.txt", header = FALSE)

VariableLogicTxt <- VariableLogicTxt[[1]][c(-(length(VariableLogicTxt[[1]])-1), -length(VariableLogicTxt[[1]]))] #identified that last two rows are parameters, not vectors. And removed them.

Variables_vector_AS <- vector_of_variable_names(VariableLogicTxt)

#generating logic
Logiclist_AS <- txt_to_logic_formula(VariableLogicTxt)
as.character(Logiclist_AS[[1]][2])

#defining parameters
Parameters_vector_AS <- c(0, 1, 1)
names(Parameters_vector_AS) <- c("LIF", "CH", "PD")

#creating BoolInfo object
Variables_AS <- Variables_vector_AS
Parameters_AS <- c("LIF", "CH", "PD")
Logic_AS <- Logiclist_AS 

BoolInfo_AS <- BoolInfo(variables = Variables_AS, parameters = Parameters_AS, logic = Logic_AS)

test_AS <- BoolInfoResults(BoolInfo_AS, NetworkDirectory = "/Users/gordid/Desktop/Modeling/Austin_Smith/Boolean/Networks/", ResultsDirectory = "/Users/gordid/Desktop/Modeling/Austin_Smith/Boolean/Results/")

######
BoolInfo_AS_011_KD_Esrrb <- BoolInfo(variables = Variables_vector_AS, parameters = Parameters_vector_AS, logic = Logiclist_AS, KD = "Esrrb")

file_name_suffix <- paste0(sub("_$", "" , paste0(names(BoolInfo_AS_011_KD_Esrrb@parameters),  BoolInfo_AS_011_KD_Esrrb@parameters, "_", collapse = "")), if (length(BoolInfo_AS_011_KD_Esrrb@KD > 0)) { paste0("_KD_", BoolInfo_AS_011_KD_Esrrb@KD )})

#BUILDING NETWORK

#Create Directory
Directory_Networks <-  paste0("/Users/gordid/Desktop/Modeling/Austin_Smith/Boolean/Networks/",file_name_suffix, "/")

if (dir.exists(Directory_Networks) == FALSE) {
  dir.create(Directory_Networks, recursive = TRUE)
} else {
  "Already made this directory!"
}

#Create state space list with update breaks, and edges/nodes dataframes for visNetwork, and state space vector for igraph
Boolean_state_space_list_AS_011_KD_Esrrb_update_break <- Boolean_state_space(BoolInfo_AS_011_KD_Esrrb, updates = 10, update_break = TRUE)

state_space_dataframe_AS_011_KD_Esrrb_nodes <- state_space_dataframe(Boolean_state_space_list_AS_011_KD_Esrrb_update_break, BoolInfo_AS_011_KD_Esrrb, toDecimal = TRUE, nodes = T, edges = F)
state_space_dataframe_AS_011_KD_Esrrb_nodes

state_space_dataframe_AS_011_KD_Esrrb_edges <- state_space_dataframe(Boolean_state_space_list_AS_011_KD_Esrrb_update_break, BoolInfo_AS_011_KD_Esrrb, toDecimal = TRUE, nodes = F, edges = T)
state_space_dataframe_AS_011_KD_Esrrb_edges
dim(state_space_dataframe_AS_011_KD_Esrrb_edges) 

state_space_vector_igraph <- state_space_dataframe(Boolean_state_space_list_AS_011_KD_Esrrb_update_break, BoolInfo_AS_011_KD_Esrrb, toDecimal = TRUE, nodes = F, edges = F) 
# state_space_dataframe_AS_011_KD_Esrrb_edges[which(state_space_dataframe_AS_011_KD_Esrrb_edges == "256", arr.ind = T),]
# state_space_dataframe_AS_011_KD_Esrrb_edges[c(which(state_space_dataframe_AS_011_KD_Esrrb_edges[, 1] == "256"), which(state_space_dataframe_AS_011_KD_Esrrb_edges[, 2] == "256")), ]
# c(which(state_space_dataframe_AS_011_KD_Esrrb_edges[, 1] == "256"), which(state_space_dataframe_AS_011_KD_Esrrb_edges[, 2] == "256"))

AS_011_KD_Esrrb_network_kk <- visNetwork(state_space_dataframe_AS_011_KD_Esrrb_nodes, state_space_dataframe_AS_011_KD_Esrrb_edges) %>% visEdges(arrows = list(to = list(enabled = TRUE, 
scaleFactor = 0.25)), smooth = FALSE) %>% visNodes(size = 3) %>% visPhysics(stabilization = F) %>% visIgraphLayout(layout = "layout_with_kk")

visSave(graph = AS_011_KD_Esrrb_network_kk, file = paste0(Directory_Networks, file_name_suffix, "_network_kk.html"))


# AS_011_network_mds
# AS_011_network_kk
# AS_011_network_tree
# AS_011_network_sugiyama
# AS_011_network_graphopt
# AS_011_network_dh

#BUILDING HEATMAP

#Create Directory

file_name_suffix <- paste0(sub("_$", "" , paste0(names(BoolInfo_AS_011_KD_Esrrb@parameters),  BoolInfo_AS_011_KD_Esrrb@parameters, "_", collapse = "")), if (length(BoolInfo_AS_011_KD_Esrrb@KD > 0)) { paste0("_KD_", BoolInfo_AS_011_KD_Esrrb@KD )})

Directory_Results <-  paste0("/Users/gordid/Desktop/Modeling/Austin_Smith/Boolean/Results/", file_name_suffix, "/")
if (dir.exists(Directory_Results) == FALSE) {
  dir.create(Directory_Results, recursive = TRUE)
} else {
  "Already made this directory!"
}

#Create state space list, create decimal state space matrix, generate heatmap
Boolean_state_space_list_AS_011_KD_Esrrb <- Boolean_state_space(BoolInfo_AS_011_KD_Esrrb, updates = 10, update_break = F)

decimal_state_space_matrix_AS_011_KD_Esrrb <- decimal_state_space_matrix(Boolean_state_space_list_AS_011_KD_Esrrb, BoolInfo_AS_011_KD_Esrrb, as_range = F)

if (SaveCSVs) {write.csv(decimal_state_space_matrix_AS_011_KD_Esrrb, file = paste0(Directory_Results, file_name_suffix, "_decimal_state_space_matrix.csv")) }
                                                                                   
                                                                                  


heatmap_AS_011_KD_Esrrb <- pheatmap(decimal_state_space_matrix_AS_011_KD_Esrrb, cluster_rows = TRUE, cluster_cols = FALSE)

if (SavePlots == TRUE) {pdf(file = paste0(Directory_Results, file_name_suffix, "_heatmap.pdf"),
                            width=16.25, height=12.5)}

heatmap_AS_011_KD_Esrrb

if (SavePlots == TRUE) {dev.off()}

decimal_state_space_matrix_AS_011_KD_Esrrb_initial_final <- decimal_state_space_matrix_AS_011_KD_Esrrb[,c(1, ncol(decimal_state_space_matrix_AS_011_KD_Esrrb) )]

unique(decimal_state_space_matrix_AS_011_KD_Esrrb[, ncol(decimal_state_space_matrix_AS_011_KD_Esrrb)])
binary(unique(decimal_state_space_matrix_AS_011_KD_Esrrb[, ncol(decimal_state_space_matrix_AS_011_KD_Esrrb)]))

fixed_points <-sort(unique(decimal_state_space_matrix_AS_111[, ncol(decimal_state_space_matrix_AS_111)]), decreasing = T)

binary(fixed_points, mb = 12)
decimal_state_space_matrix_AS_011[binary(fixed_points, mb = 12), ncol(decimal_state_space_matrix_AS_011)]

strsplit(binary(fixed_points, mb = 12), "")[[1]]

fixed_points_list <- list()
for (k in 1:length(fixed_points)) {
  fixed_points_list[[k]] <- sum(decimal_state_space_matrix_AS_111[, ncol(decimal_state_space_matrix_AS_111)] == fixed_points[k])
}
sum(unlist(list(1, 3)))
sum(fixed_points_list)
sum(decimal_state_space_matrix_AS_111[, ncol(decimal_state_space_matrix_AS_111)] == fixed_points[1])
sum(decimal_state_space_matrix_AS_111[, ncol(decimal_state_space_matrix_AS_111)] == as.list(fixed_points))

binary(matrix(c(1,3,5,7), nrow = 2, byrow = 2))
matrix(nrow = 2, ncol = 2)

####Building summary matrix####

predictions_summary <- function(initial_BoolInfo, final_BoolInfo, initial_state_matrix, final_state_matrix) {
  
  file_name_suffix_initial <- paste0(sub(", $", "" , paste0(initial_BoolInfo@parameters, ":",  initial_BoolInfo@parameter_values, ", ", collapse = "")), if (length(initial_BoolInfo@KD > 0)) { paste0("_KD_", initial_BoolInfo@KD )})
  
  file_name_suffix_final <- paste0(sub(", $", "" , paste0(final_BoolInfo@parameters, ":",  final_BoolInfo@parameter_values, ", ", collapse = "")), if (length(final_BoolInfo@KD > 0)) { paste0("_KD_", final_BoolInfo@KD )})
  
  #initialize matrix
  summary_matrix <- matrix(nrow = length(unique(initial_state_matrix[, ncol(initial_state_matrix)]))*2, ncol = (length(initial_BoolInfo@variables) + 1) )
  colnames(summary_matrix) <- c(initial_BoolInfo@variables, c("Percent of all fixed points"))
  rownames(summary_matrix) <- 1:nrow(summary_matrix)

  #set fixed points and corresponding predictions using them as initial conditions
  fixed_points_vector <- sort(unique(initial_state_matrix[, ncol(initial_state_matrix)]), decreasing = T)
  predictions_vector <- final_state_matrix[binary(fixed_points_vector, mb = (length(initial_BoolInfo@variables)-1)), ncol(final_state_matrix)]
  
  fixed_points_list <- list()
  for (k in 1:length(fixed_points_vector)) {
    fixed_points_list[[k]] <- sum(initial_state_matrix[, ncol(initial_state_matrix)] == fixed_points_vector[k])
  }
  
  predictions_list <- list()
  for (l in 1:length(fixed_points_vector)) {
    predictions_list[[l]] <- sum(final_state_matrix[, ncol(final_state_matrix)] == predictions_vector[l])
  }


  for (i in 1:length(fixed_points_vector)) {
    summary_matrix[(2*i - 1), 1:length(initial_BoolInfo@variables)] <-  strsplit(binary(fixed_points_vector, mb = (length(initial_BoolInfo@variables)-1)), "")[[i]]
    
    summary_matrix[(2*i - 1), ncol(summary_matrix)] <-  (fixed_points_list[[i]]/sum(unlist(fixed_points_list)))*100
    

    
    rownames(summary_matrix)[2*i - 1] <- paste0(file_name_suffix_initial, " Fixed Point ", i)
    
  }
  
  for (j in 1:length(predictions_vector)) {
    summary_matrix[(2*j), 1:length(initial_BoolInfo@variables)] <-  strsplit(binary(predictions_vector, mb = (length(initial_BoolInfo@variables)-1)), "")[[j]]
    
    summary_matrix[(2*j), ncol(summary_matrix)] <-  (predictions_list[[j]]/sum(unlist(predictions_list)))*100
    
    rownames(summary_matrix)[2*j] <- paste0(file_name_suffix_final, " Final State (initial conditions: ", "Fixed Point ", j, ")")
  }
  
  summary_matrix
  
}

model_summary_AS_111_011 <- model_summary(BoolInfo_AS_111, BoolInfo_AS_011, decimal_state_space_matrix_AS_111, decimal_state_space_matrix_AS_011)

predictions_matrix_AS <- read.csv(file = "/Users/gordid/Desktop/Modeling/Austin_Smith/Boolean/Results/predictions_matrix.csv")[,-15]
colnames(predictions_matrix_AS)[1] <- "Parameters"
predictions_matrix_AS

predictions_matrix_AS[grep(pattern = "LIF:1, CH:1, PD:1 Fixed", predictions_matrix_AS[, 1]) ,]
rbind(predictions_matrix_AS[grep(pattern = "LIF:1, CH:1, PD:1 Fixed", predictions_matrix_AS[, 1]) ,][1,], experimental_results_summary_AS[1, ], predictions_matrix_AS[grep(pattern = "LIF:1, CH:1, PD:1 Fixed", predictions_matrix_AS[, 1]) ,][2,])

predictions_matrix_AS[grep(pattern = "LIF:0, CH:1, PD:1 Fixed", predictions_matrix_AS[, 1]) ,]
rbind(predictions_matrix_AS[grep(pattern = "LIF:0, CH:1, PD:1 Fixed", predictions_matrix_AS[, 1]) ,][1,], experimental_results_summary_AS[2, ], predictions_matrix_AS[grep(pattern = "LIF:0, CH:1, PD:1 Fixed", predictions_matrix_AS[, 1]) ,][2,])

predictions_matrix_AS[grep(pattern = "LIF:1, CH:1, PD:0 Fixed", predictions_matrix_AS[, 1]) ,]
rbind(predictions_matrix_AS[grep(pattern = "LIF:1, CH:1, PD:0 Fixed", predictions_matrix_AS[, 1]) ,][1,], experimental_results_summary_AS[3, ], predictions_matrix_AS[grep(pattern = "LIF:1, CH:1, PD:0 Fixed", predictions_matrix_AS[, 1]) ,][2,])

predictions_matrix_AS[grep(pattern = "LIF:1, CH:0, PD:1 Fixed", predictions_matrix_AS[, 1]) ,][1:6,]
rbind(predictions_matrix_AS[grep(pattern = "LIF:1, CH:0, PD:1 Fixed", predictions_matrix_AS[, 1]) ,][1:6,], experimental_results_summary_AS[4, ])

predictions_matrix_AS[grep(pattern = "LIF:0, CH:0, PD:0 Fixed", predictions_matrix_AS[, 1]) ,]
rbind(predictions_matrix_AS[grep(pattern = "LIF:0, CH:0, PD:0 Fixed", predictions_matrix_AS[, 1]) ,][1,], experimental_results_summary_AS[5, ], predictions_matrix_AS[grep(pattern = "LIF:0, CH:0, PD:0 Fixed", predictions_matrix_AS[, 1]) ,][2,])


experimental_results_summary_AS <- read.csv(file = "/Users/gordid/Desktop/AS_Results_Summary.csv")
experimental_results_summary_AS <- experimental_results_summary_AS[, order(match(colnames(experimental_results_summary_AS), colnames(predictions_matrix_AS)))]
experimental_results_summary_AS



order(match(colnames(experimental_results_summary_AS)[-1], colnames(predictions_matrix_AS)[c(-1, -15)] ))

colnames(experimental_results_summary_AS)[-1]
colnames(predictions_matrix_AS)[c(-1, -15)]

colnames(experimental_results_summary_AS)[-1][order(match(colnames(experimental_results_summary_AS)[-1], colnames(predictions_matrix_AS)[c(-1, -15)] ))]
colnames(predictions_matrix_AS)[1] <- "Parameters"



#####test######
BoolInfo_test <- BoolInfo(parameters = c("P1, P2"), variables = c("x1, x2, x3, x4, x5, x6"), logic = list(~P1, ~P1 | P2, ~x1 | x2, ~(!(x5) | !(x6)) & x3, ~x1, ~(!(x2) & !(P2))))
BoolBuildR(BoolInfo_test)

#######RACIPE######
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("sRACIPE")

devtools::install_github("vivekkohar/sRACIPE")

install.packages("/Users/gordid/Desktop/sRACIPE_master_2", repos = NULL, type = "source")

library(sRACIPE)


RACIPE_df <- BoolBuildR_edges
RACIPE_df <- RACIPE_df[, -ncol(RACIPE_df)]
RACIPE_df[, 3] <- sapply(RACIPE_df[, 3], gsub, pattern = "arrow", replacement = 1)
RACIPE_df[, 3] <- sapply(RACIPE_df[, 3], gsub, pattern = "circle", replacement = 2)
colnames(RACIPE_df) <- c("Source", "Target", "Type")
data("demoCircuit")
rSet <- sRACIPE::sracipeSimulate(demoCircuit)
write.table(RACIPE_df, quote = FALSE,row.names = FALSE, file = "test.txt")

