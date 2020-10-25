TestClass2 <- setClass(
  # Set the name for the class
  "TestClass2",
  
  #Define the slots
  
  slots = c(variables = "character",
            parameters = "character",
            parameter_values = "numericORlogical"

            
  ),
  
  validity=function(object) {
    
    if (length(object@parameter_values) != length(object@parameters)) {
      
      TestClass2 <<- setClass(
        # Set the name for the class
        "TestClass2",
        
        slots = c(variables = "character",
                  parameters = "character",
                  parameter_values = "numericORlogical"
        ),
       
        prototype = list(parameter_values = rep(1, length(object@parameters)))
         
      )
      
    }
      
      
    
  }
)

TestClass2(variables = c("a", "b"), parameters = c("c"))


CreateBoolInfo <- function(variables, parameters, logic, parameter_values = NA, KD = NULL) {
  
  
  if (length(parameter_values) != length(parameters) ) {
    warning("The amount of parameter values is not equal to the amount of parameter names. All parameter values have been set to 1.")
    parameter_values <- rep(1, length(parameters))
  } else {
    parameter_values <- as.numeric(parameter_values)
  }
  
  TestClass2 <<- setClass(
    # Set the name for the class
    "TestClass2",
    
    #Define the slots
    
    slots = c(variables = "character",
              parameters = "character",
              logic = "list",
              parameter_values = "numericORlogical",
              KD = "character"
    )
)
  if (length(KD) > 0) {
    print(length(KD))
    new("TestClass2", variables = variables, parameters = parameters, logic = logic, parameter_values = parameter_values,  KD = KD)
    
  } else {
    
    new("TestClass2", variables = variables, parameters = parameters, logic = logic, parameter_values = parameter_values)
    
  } 

  
}

variables_Lac <- c("M", "E", "L")
parameters_Lac <- c("G_e", "L_e")
parameter_values_Lac <- c(0, 1)
logic_Lac <- list(~(!G_e) & (L | L_e), ~M , ~(!G_e) & E & L_e) #logic_list <- list(~((G_e + 1)*(L + L_e + L*L_e))%%2, ~M , ~((G_e + 1)*E*L_e)%%2)


BoolInfo_Lac <- CreateBoolInfo(variables = variables_Lac, parameters = parameters_Lac, logic = logic_Lac)


Boolean_state_space(BoolInfo_Lac)


TestClass1 <- function(names, values = NA) {
  
  if (length(values) != length(names) ) {
    warning("The amount of parameter values is not equal to the amount of parameter names. All parameter values have been set to 1.")
    values <- rep(1, length(names))
  } else {
  values <- as.numeric(values)
  }

  new("TestClass1", names = names, values = values)
   
}

TestClass_1 <- TestClass1(names = c("a", "b") )



BoolInfo_test <- BoolInfo(variables = c("x1", "x2", "x3"), logic = list(~x2, ~x1 | x3, ~(x1 & x2) | x3))


State_Space_update_break <- Boolean_state_space(BoolInfo = BoolInfo_Lac , updates = 5, update_break = TRUE)

State_Space_node <- state_space_dataframe(State_Space_update_break, BoolInfo_test, toDecimal = FALSE, nodes = T, edges = F)

State_Space_edge <- state_space_dataframe(State_Space_update_break, BoolInfo_test, toDecimal = FALSE, nodes = F, edges = T)

State_Space_igrap <- state_space_dataframe(State_Space_update_break, BoolInfo, toDecimal = TRUE, nodes = F, edges = F) 

State_Space_networ <- visNetwork(State_Space_node, State_Space_edge) %>% visEdges(arrows = list(to = list(enabled = TRUE, 
                                                                                                             scaleFactor = 0.25)), smooth = FALSE) %>% visNodes(size = 3) %>% visPhysics(stabilization = F) %>% visIgraphLayout(layout = "layout_with_kk")


state_space_diagram <- function (BoolInfo) {
  
  State_Space_update_break <- Boolean_state_space(BoolInfo = BoolInfo, update_break = TRUE)

  State_Space_nodes <- state_space_dataframe(State_Space_update_break, BoolInfo = BoolInfo, toDecimal = FALSE, nodes = T, edges = F)

  print(State_Space_nodes)
    
  if (length(BoolInfo@variables) < 5) {
    
    State_Space_nodes <- cbind(State_Space_nodes, label = State_Space_nodes[, "id"] )
    
  }
  
  State_Space_edges <- state_space_dataframe(State_Space_update_break, BoolInfo = BoolInfo, toDecimal = FALSE, nodes = F, edges = T)
  
  
  if (length(BoolInfo@variables) > 5) {
    
    visNetwork <- visNetwork(State_Space_nodes, State_Space_edges)  %>% visEdges(arrows = list(to = list(enabled = TRUE,  scaleFactor = 0.25)), smooth = FALSE)  %>% visNodes(size = 3) %>% visPhysics(stabilization = F) %>% visIgraphLayout(layout = "layout_with_kk")
    
  } else {
    
    visNetwork <- visNetwork(State_Space_nodes, State_Space_edges)  %>% visEdges(arrows = list(to = list(enabled = TRUE,  scaleFactor = 0.25)), smooth = TRUE) %>% visNodes(value = 30, shape = "circle", scaling = list(label = list(enabled = T)))
  }
  
  visNetwork
}

state_space_diagram(BoolInfo_test)


test4 = matrix(1:4, nrow = 2, byrow = TRUE)
colnames(test4) <- c("a", "b")
test4[, 3] <- test4[, 1]
cbind(test4, label = test4[, 1])
# boolean_state_space_list_test <- head(Boolean_state_space_list_AS_111_update_break)
# number_of_variables_test <- 13
# 
# head(boolean_state_space_list_test)
# 
# 
# 
# node_names_list_test <- list()
# for (f in 1:length(boolean_state_space_list_test)) {
#   node_names_vector_test <- c()
#   for (e in 1:nrow(boolean_state_space_list_test[[f]])) {
#     node_names_vector_test[e] <- paste(as.character(boolean_state_space_list_test[[f]][e, 1:number_of_variables_test ]), collapse = "")
#     print(nrow(boolean_state_space_list_test[[f]]))
#     print(paste(as.character(boolean_state_space_list_test[[f]][e, 1:number_of_variables_test ]), collapse = ""))
#   }
#   print(paste0(node_names_vector_test,": test"))
#   node_names_list_test[[f]] <- node_names_vector_test
# }
# print(head(node_names_list_test))
# for (d in 1:length(node_names_list_test)) {
#   node_names_list_test[[d]] <- rep(node_names_list_test[[d]], each = 2)
#   node_names_list_test[[d]] <- node_names_list_test[[d]][c(-1, -length(node_names_list_test[[d]]))]
#   # print(node_names_list_test[[d]])
#   # plot(igraph::graph(node_names_list_test[[d]]))
# }
# state_space_vector_test <- unlist(node_names_list_test)
# state_space_vector_test <- split(state_space_vector_test, (ceiling(seq_along(state_space_vector_test)/2)))

##############

match_rows_to_value_list <- list()
for (i in 1:nrow(decimal_state_space_matrix_AS_111)) {
  if (length(which(decimal_state_space_matrix_AS_111[i, ] == 0)) > 0)
    match_rows_to_value_list[[i]] <- decimal_state_space_matrix_AS_111[i, ]
}
match_rows_to_value_list <- Filter(length, match_rows_to_value_list )

##############
head(match_rows_to_value_list, n = 25)
matching_vector_test <- c(770, 256)
match_vector(list_of_vectors =  match_rows_to_value_list, matching_vector_test)

##############
matching_vector_test <- c(280, 112)

test_list <- list(c(1033, 280, 112), c(1033, 112, 280))

match_vector <- function(list_of_vectors, matching_vector) {
  
  list_of_matching_vectors <- list()
  
  for (i in 1:length(list_of_vectors)) {
    for (j in 1:length(list_of_vectors[[i]])) {
      for (k in 1:length(matching_vector)) {
        if ((k < length(matching_vector)) & (j < length(list_of_vectors[[i]])) & (list_of_vectors[[i]][j] == matching_vector[k]) & (list_of_vectors[[i]][j+1] == matching_vector[k + 1])) {
          print("test")
          list_of_matching_vectors[[i]] <- list_of_vectors[[i]]
          break
        }
      }
    }
  }
  
  
  list_of_matching_vectors <- Filter(length, list_of_matching_vectors)
  list_of_matching_vectors
}

match_vector(list_of_vectors = test_list, matching_vector = matching_vector_test )




####practicing clustering graphs

set.seed(123)
plot(barabasi.game(1000) %>% as.undirected())




nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
                    group = sample(LETTERS[1:3], 15, replace = TRUE))

edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
                    to = trunc(runif(15)*(15-1))+1)

visNetwork(nodes, edges) %>% visEdges(arrows = "to") %>%
  visOptions(collapse = TRUE)



# ####Austin Smith_111####
# 
# 
# SavePlots = TRUE
# SaveCSVs = TRUE
# 
# #GENERATING ARGUMENTS
# VariableLogicTxt <- read.delim(file = "/Users/gordid/Desktop/ASmithNetworkLogic.txt", header = FALSE)
# VariableLogicTxt <- VariableLogicTxt[[1]][c(-(length(VariableLogicTxt[[1]])-1), -length(VariableLogicTxt[[1]]))]
# 
# Logiclist_AS <- txt_to_logic_formula(VariableLogicTxt)
# 
# Variables_vector_AS <- vector_of_variable_names(VariableLogicTxt)
# Parameters_vector_AS <- c(1, 1, 1)
# names(Parameters_vector_AS) <- c("LIF", "CH", "PD")
# 
# BoolInfo_AS_111 <- BoolInfo(variables = Variables_vector_AS, parameters = Parameters_vector_AS, logic = Logiclist_AS)
# 
# #BUILDING NETWORK
# 
# #Create Directory
# Directory_Networks <-  paste0("/Users/gordid/Desktop/Modeling/Austin_Smith/Boolean/Networks/", "AS_111")
# 
# if (dir.exists(Directory_Networks) == FALSE) {
#   dir.create(Directory_Networks, recursive = TRUE)
# } else {
#   "Already made this directory!"
# }
# 
# #Create state space list with update breaks, and edges/nodes dataframes for visNetwork, and state space vector for igraph
# Boolean_state_space_list_AS_111_update_break <- Boolean_state_space(variables = Variables_vector_AS, parameters = Parameters_vector_AS, logic = Logiclist_AS, updates = 13, update_break = TRUE)
# 
# state_space_dataframe_AS_111_nodes <- state_space_dataframe(Boolean_state_space_list_AS_111_update_break, 13, toDecimal = TRUE, nodes = T, edges = F)
# state_space_dataframe_AS_111_nodes
# 
# state_space_dataframe_AS_111_edges <- state_space_dataframe(Boolean_state_space_list_AS_111_update_break, 13, toDecimal = TRUE, nodes = F, edges = T)
# state_space_dataframe_AS_111_edges
# dim(state_space_dataframe_AS_111_edges) 
# 
# state_space_vector_igraph <- state_space_dataframe(Boolean_state_space_list_AS_111_update_break, 13, toDecimal = TRUE, nodes = F, edges = F) 
# # state_space_dataframe_AS_111_edges[which(state_space_dataframe_AS_111_edges == "256", arr.ind = T),]
# # state_space_dataframe_AS_111_edges[c(which(state_space_dataframe_AS_111_edges[, 1] == "256"), which(state_space_dataframe_AS_111_edges[, 2] == "256")), ]
# # c(which(state_space_dataframe_AS_111_edges[, 1] == "256"), which(state_space_dataframe_AS_111_edges[, 2] == "256"))
# 
# AS_111_network_kk <- visNetwork(state_space_dataframe_AS_111_nodes, state_space_dataframe_AS_111_edges) %>% visEdges(arrows = list(to = list(enabled = TRUE, 
#                                                                                                                                              scaleFactor = 0.25)), smooth = FALSE) %>% visNodes(size = 3) %>% visPhysics(stabilization = F) %>% visIgraphLayout(layout = "layout_with_kk")
# 
# visSave(graph = AS_111_network_kk, file = paste0(Directory_Networks, "/AS_111_network_kk.html"))
# 
# # AS_111_network_mds
# # AS_111_network_kk
# # AS_111_network_tree
# # AS_111_network_sugiyama
# # AS_111_network_graphopt
# # AS_111_network_dh
# 
# #BUILDING HEATMAP
# 
# #Create Directory
# Directory_Results <-  paste0("/Users/gordid/Desktop/Modeling/Austin_Smith/Boolean/Results/", "AS_111")
# 
# if (dir.exists(Directory_Results) == FALSE) {
#   dir.create(Directory_Results, recursive = TRUE)
# } else {
#   "Already made this directory!"
# }
# 
# #Create state space list, create decimal state space matrix, generate heatmap
# Boolean_state_space_list_AS_111 <- Boolean_state_space(variables = Variables_vector_AS, parameters = Parameters_vector_AS, logic = Logiclist_AS, updates = 13, update_break = F)
# 
# decimal_state_space_matrix_AS_111 <- decimal_state_space_matrix(Boolean_state_space_list_AS_111, 13, as_range = F)
# if (SaveCSVs) {write.csv(decimal_state_space_matrix_AS_111, file = paste0(Directory_Results, "/AS_111_decimal_state_space_matrix.csv"))}
# 
# if (SavePlots == TRUE) {pdf(file = paste0(Directory_Results, "/AS_111_heatmap.pdf"), width=16.25, height=12.5)}
# 
# decimal_state_space_matrix_AS_111_initial_final <- decimal_state_space_matrix_AS_111[,c(1, ncol(decimal_state_space_matrix_AS_111) )]
# 
# heatmap_AS_111 <- pheatmap(decimal_state_space_matrix_AS_111, cluster_rows = TRUE, cluster_cols = FALSE)
# heatmap_AS_111
# if (SavePlots == TRUE) {dev.off()}
# 
# unique(decimal_state_space_matrix_AS_111[, ncol(decimal_state_space_matrix_AS_111)])
# binary(unique(decimal_state_space_matrix_AS_111[, ncol(decimal_state_space_matrix_AS_111)]))
# Variables_vector_AS
# 
# sum(decimal_state_space_matrix_AS_111[, "t + 12"] == unique(decimal_state_space_matrix_AS_111[, "t + 12"])[1])
# sum(decimal_state_space_matrix_AS_111[, "t + 12"] == unique(decimal_state_space_matrix_AS_111[, "t + 12"])[2])
# 
# 
# 
# 
# binary(c(0, 2), mb = 12)
# 
# 
# 
# 
# 
# library(grid)
# library(gridExtra)
# 
# # example data & header row
# tab <- tableGrob(mtcars[1:3, 1:4], rows=NULL)
# tab$
#   header <- tableGrob(mtcars[1, 1:2], rows=NULL, cols=c("head1", "head2")) 
# 
# jn <- gtable_combine(header[1,], tab, along=2)
# jn$widths <- rep(max(jn$widths), length(jn$widths)) # make column widths equal
# #grid.newpage()
# #grid.draw(jn) # see what it looks like before altering gtable
# 
# # change the relevant rows of gtable
# jn$layout[1:4 , c("l", "r")] <- list(c(1, 3), c(2, 4))
# 
# grid.newpage()
# grid.draw(tab)
# 
# 
# gtable(unit(1:3, c("cm")), unit(5, "cm"))

