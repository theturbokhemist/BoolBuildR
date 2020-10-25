# install.packages("lazyeval")
# install.packages("pryr")
# install.packages("DiagrammeR")
# install.packages("compositions")
install_github('RBioFabric',  username='wjrl')
library(RBioFabric)

####Boolean_state_space function. Parameters argument can be either a named logical or numerical vector. Also, takes both algebraic and boolean logic as the logic argument####
Boolean_state_space <- function(variables, parameters, logic, updates, update_break = FALSE) {
  
  #initialize state space list
  Boolean_state_space_list <- list()
  node_names_list <- list()
  
  
  parameter_names <- names(parameters)
  
  parameters <- as.numeric(parameters)
  
  #generate matrix of all possible combinations
  node_matrix <- as.matrix(expand.grid(lapply(numeric(length(variables)), function(x) c(0,1))), ncol=length(variables)) #http://www.di.fc.ul.pt/~jpn/r/combinatorics/combinatorics.html
  
  
  colnames(node_matrix) <- NULL #This is important, not sure why. Had to do with assigned variables having "names" if this matrix has column names.
  # print(node_matrix)
  
  
  # do.call("<-",list(variables[1], node_matrix[1,1]))
  
  
  for (f in 1:nrow(node_matrix)) {
    ####generate local variables
    
    #assign variable values to variable names
    for (i in 1:length(variables)) {
      assign(variables[i], node_matrix[f,i])
      
    }
    #assign parameter values to parameter names
    for (j in 1:length(parameters)) {
      assign(parameter_names[j], parameters[j])
      # print(eval(as.name(parameter_names[j])))
    }
    
    # colnames(node_matrix) <- variables
    
    #initialize matrix
    Boolean_state_space_list[[f]] <- matrix(nrow = updates, ncol = length(c(variables, parameter_names)))
    colnames(Boolean_state_space_list[[f]]) <- c(variables, parameter_names)
    rownames(Boolean_state_space_list[[f]]) <- c(1:nrow(Boolean_state_space_list[[f]]))
    row.names(Boolean_state_space_list[[f]])[1] <- c("t")
    
    counter <- 1
    for (e in 1:(nrow(Boolean_state_space_list[[f]])-1)) {
      row.names(Boolean_state_space_list[[f]])[e + 1] <- paste("t", "+", counter, sep = " ")
      counter <- counter + 1
    }

    
    Boolean_state_space_list[[f]][1,] <- c(node_matrix[f,], parameters)
    
    
    for (a in 1:nrow(Boolean_state_space_list[[f]])) {
      for (b in (length(parameters)-1):0) {
        Boolean_state_space_list[[f]][a, ncol(Boolean_state_space_list[[f]]) - b] <- parameters[length(parameters) - b]
      }
      
    }
    
    
    # Get correct environment
    env <- pryr::where(variables[1])
     # print(env())
    # print(Boolean_state_space_list[[f]]) #test to see if matrix was initialized succesfully
    #test to see if local variables were successfully defined within the function:
    # print(variables[1])
    # print(eval(parse(text = variables[1])))
    
    
    #Synchronous update of matrix  
    for (k in 2:nrow(Boolean_state_space_list[[f]])) {
      
      for (h in 1:length(variables)) {
        #Set environment to evaluate formula in
        rlang::f_env(logic[[h]]) <- env
        
        # TEST TO SEE IF ITS WORKING: print(paste0("test_", "f:", f, "k:", k, "h:", h, "f_eval:", lazyeval::f_eval(logic[[h]])))
        
        Boolean_state_space_list[[f]][k, variables[h]] <- lazyeval::f_eval(logic[[h]]) #logicals get converted to numericals before being assigned to the matrix. This is a property of matrices.
      }
      
      for (g in 1:length(variables)) {
        assign(variables[g], Boolean_state_space_list[[f]][k, g])
        
      }
      if (update_break == TRUE) {
      if (identical(Boolean_state_space_list[[f]][k, ], Boolean_state_space_list[[f]][k-1, ]) & identical(Boolean_state_space_list[[f]][k, ], Boolean_state_space_list[[f]][k-2, ]))
        {
        Boolean_state_space_list[[f]] <- Boolean_state_space_list[[f]][1:(k-1), , drop = FALSE ]
        break 
        }
      }
    }
    
  }
  
names(Boolean_state_space_list) <- 1:(length(Boolean_state_space_list))
  Boolean_state_space_list
  
}

####Decimal state space matrix####

decimal_state_space_matrix <- function(boolean_state_space_list, number_of_variables, as_range = TRUE) {
  #initialize matrix
  decimal_matrix <- matrix(nrow = length(boolean_state_space_list), ncol = nrow(boolean_state_space_list[[1]]))
  row.names(decimal_matrix) <- 1:length(boolean_state_space_list)
  colnames(decimal_matrix) <- rownames(boolean_state_space_list[[1]])

  for (i in 1:length(boolean_state_space_list)) {
    for (j in 1:ncol(decimal_matrix)) {
      decimal_matrix[i, j] <- unbinary(paste(boolean_state_space_list[[i]][j,1:number_of_variables ], collapse = ""))
     
    }

    rownames(decimal_matrix)[i] <- paste(boolean_state_space_list[[i]][1,1:number_of_variables ], collapse = "")
  }
  if (as_range == TRUE) {
    decimal_matrix <- decimal_matrix/(2**number_of_variables-1)
  }
 decimal_matrix
}

####State Space Dataframe####

state_space_dataframe <- function(boolean_state_space_list, number_of_variables, toDecimal = TRUE, nodes = TRUE, edges = FALSE) {
  node_names_list <- list()
  for (f in 1:length(boolean_state_space_list)) {
    node_names_vector <- c()
    for (e in 1:nrow(boolean_state_space_list[[f]])) {
      node_names_vector[e] <- paste(as.character(boolean_state_space_list[[f]][e, 1:number_of_variables ]), collapse = "")
    }
    node_names_list[[f]] <- node_names_vector
  }
 # print(node_names_list)
  for (d in 1:length(node_names_list)) {
    node_names_list[[d]] <- rep(node_names_list[[d]], each = 2)
    node_names_list[[d]] <- node_names_list[[d]][c(-1, -length(node_names_list[[d]]))]
    # print(node_names_list[[d]])
    # plot(igraph::graph(node_names_list[[d]]))
  }
  state_space_vector <- unlist(node_names_list)
  state_space_vector <- split(state_space_vector, (ceiling(seq_along(state_space_vector)/2)))
  state_space_vector <-  state_space_vector[-which(duplicated(state_space_vector))]
  
  state_space_vector <- unname(unlist(state_space_vector))
  if (toDecimal) {
    state_space_vector <- as.character(unbinary(state_space_vector))
  }

  if (!nodes & !edges) {
    state_space_dataframe <- state_space_vector
  }
  
  if (nodes & !edges) {
    state_space_dataframe <- data.frame(id = unique(state_space_vector), title = paste0("<p><b>", unique(state_space_vector),"</b><br></p>"))
  }
  
  if ((!nodes & edges) | (nodes & edges)) {
    state_space_dataframe <- matrix(state_space_vector, ncol = 2, byrow = TRUE)
    state_space_dataframe <- as.data.frame(state_space_dataframe, stringsAsFactors = FALSE)
    colnames(state_space_dataframe) <- c("from", "to")
  }
  state_space_dataframe
}

####Functions for starting from text logic####


txt_to_logic_formula <- function(logic_text) {
  
  
  logic_text <- lapply(logic_text, gsub, pattern = "’", replacement = "")
  logic_text <- lapply(logic_text, gsub, pattern = "OR|or|Or", replacement = "|")
  logic_text <- lapply(logic_text, gsub, pattern = "AND|and", replacement = "&")
  logic_text <- lapply(logic_text, gsub, pattern = "NOT", replacement = "!")
  
  
  logic_text_RHS <- lapply(logic_text, gsub, pattern = ".*= ", replacement = "")
  logic_text_RHS <- lapply(logic_text_RHS, function(x) paste0("~", x))
  logic_text_RHS <- lapply(logic_text_RHS, noquote)
  logic_text_RHS <- lapply(logic_text_RHS, as.formula)
  logic_text_RHS
  
}


vector_of_variable_names <- function(logic_text) {
  
  logic_text <- unname(unlist(lapply(logic_text, gsub, pattern = "’.*", replacement = "")))
  logic_text
  
}

####LAC Operon####
variable_names_vector <- c("M", "E", "L")

named_parameter_vector <- c(FALSE, FALSE)
names(named_parameter_vector) <- c("G_e", "L_e")
named_parameter_vector

logic_list <- list(~(!G_e) & (L | L_e), ~M , ~(!G_e) & E & L_e)
logic_list <- list(~((G_e + 1)*(L + L_e + L*L_e))%%2, ~M , ~((G_e + 1)*E*L_e)%%2)


Boolean_state_space_list_Lac_update_break <- Boolean_state_space(variable_names_vector, named_parameter_vector, logic_list, updates = 5, update_break = TRUE)

Boolean_state_space_list_Lac <- Boolean_state_space(variable_names_vector, named_parameter_vector, logic_list, updates = 5)

decimal_state_space_matrix_Lac <- decimal_state_space_matrix(Boolean_state_space_list_Lac, 3)

decimal_state_space_matrix_Lac_decimal <- decimal_state_space_matrix(Boolean_state_space_list_Lac, 3, as_range = F)

pheatmap(decimal_state_space_matrix_Lac_decimal, cluster_rows = TRUE, cluster_cols = FALSE)


state_space_dataframe_Lac_nodes <- state_space_dataframe(Boolean_state_space_list_Lac_update_break, 3, toDecimal = TRUE, nodes = T, edges = F)
state_space_dataframe_Lac_nodes

state_space_dataframe_Lac_edges <- state_space_dataframe(Boolean_state_space_list_Lac_update_break, 3, toDecimal = TRUE, nodes = F, edges = T)
state_space_dataframe_Lac_edges


Lac_network <- visNetwork(state_space_dataframe_Lac_nodes, state_space_dataframe_Lac_edges) %>% visEdges(arrows = "to", smooth = FALSE) %>% visNodes(size = 3) %>%
  visPhysics(stabilization = FALSE) %>% visIgraphLayout()
Lac_network
visSave(graph = Lac_network, file = "/Users/gordid/Desktop/Lac_network.html" )


as.character(state_space_vector_AS)
#plot  
plot(igraph::graph(state_space_vector_AS))
graph <- igraph::graph(state_space_vector_AS)
visIgraph(graph)
layout <- layout_as_tree(graph, root = 1)
plot(graph, layout = layout)

decimal_state_space_matrix_Lac_decimal[, c("t", "t + 4")]


####Austin Smith Network####
VariableLogicTxt <- read.delim(file = "/Users/gordid/Desktop/ASmithNetworkLogic.txt", header = FALSE)
VariableLogicTxt <- VariableLogicTxt[[1]][c(-(length(VariableLogicTxt[[1]])-1), -length(VariableLogicTxt[[1]]))]

Logiclist_AS <- txt_to_logic_formula(VariableLogicTxt)

Variables_vector_AS <- vector_of_variable_names(VariableLogicTxt)
Parameters_vector_AS <- c(0, 1, 1)
names(Parameters_vector_AS) <- c("LIF", "CH", "PD")


Boolean_state_space_list_AS <- Boolean_state_space(variables = Variables_vector_AS, parameters = Parameters_vector_AS, logic = Logiclist_AS, updates = 7)

Boolean_state_space_list_AS_011_update_break <- Boolean_state_space(variables = Variables_vector_AS, parameters = Parameters_vector_AS, logic = Logiclist_AS, updates = 7, update_break = TRUE)
state_space_vector_AS_011_update_break <- state_space_vector(Boolean_state_space_list_AS_011_update_break, 13)


decimal_state_space_matrix_AS <- decimal_state_space_matrix(Boolean_state_space_list_AS, 13)

pheatmap(decimal_state_space_matrix_AS, cluster_rows = TRUE, cluster_cols = FALSE)

####official network####
Boolean_state_space_list_AS_111_update_break <- Boolean_state_space(variables = Variables_vector_AS, parameters = Parameters_vector_AS, logic = Logiclist_AS, updates = 13, update_break = TRUE)


state_space_dataframe_AS_111_nodes <- state_space_dataframe(Boolean_state_space_list_AS_111_update_break, 13, toDecimal = TRUE, nodes = T, edges = F)
state_space_dataframe_AS_111_nodes

state_space_dataframe_AS_111_edges <- state_space_dataframe(Boolean_state_space_list_AS_111_update_break, 13, toDecimal = TRUE, nodes = F, edges = T)
state_space_dataframe_AS_111_edges
dim(state_space_dataframe_AS_111_edges)
# state_space_dataframe_AS_111_edges[which(state_space_dataframe_AS_111_edges == "256", arr.ind = T),]
# state_space_dataframe_AS_111_edges[c(which(state_space_dataframe_AS_111_edges[, 1] == "256"), which(state_space_dataframe_AS_111_edges[, 2] == "256")), ]
# c(which(state_space_dataframe_AS_111_edges[, 1] == "256"), which(state_space_dataframe_AS_111_edges[, 2] == "256"))

AS_111_network <- visNetwork(state_space_dataframe_AS_111_nodes, state_space_dataframe_AS_111_edges) %>% visEdges(arrows = list(to = list(enabled = TRUE, 
                      scaleFactor = 0.25)), smooth = FALSE) %>% visNodes(size = 3) %>% visPhysics(stabilization = F) #%>% visIgraphLayout(layout = "layout_with_kk")
AS_111_network_mds
AS_111_network_kk
AS_111_network_tree
AS_111_network_sugiyama
AS_111_network_graphopt
AS_111_network_dh

visSave(graph = AS_111_network_dh, file = "/Users/gordid/Desktop/AS_111_network_dh.html" )
####official heatmap####
Boolean_state_space_list_AS_111 <- Boolean_state_space(variables = Variables_vector_AS, parameters = Parameters_vector_AS, logic = Logiclist_AS, updates = 13, update_break = F)
decimal_state_space_matrix_AS_111 <- decimal_state_space_matrix(Boolean_state_space_list_AS_111, 13, as_range = F)

pheatmap(decimal_state_space_matrix_AS_111, cluster_rows = TRUE, cluster_cols = FALSE)
a
unique(decimal_state_space_matrix_AS_111[, ncol(decimal_state_space_matrix_AS_111)])
binary(unique(decimal_state_space_matrix_AS_111[, ncol(decimal_state_space_matrix_AS_111)]))

sum(decimal_state_space_matrix_AS_111[, "t + 12"] == unique(decimal_state_space_matrix_AS_111[, "t + 12"])[1])
sum(decimal_state_space_matrix_AS_111[, "t + 12"] == unique(decimal_state_space_matrix_AS_111[, "t + 12"])[2])


# #####THIS VERSION OF FUNCTION WORKS, BUT NOT IN A WAY THAT THE INNERWORKINGS MAKES SENSE TO ME.####
# Boolean_state_space <- function(variables, parameters, logic, updates, boolean = TRUE) {
#   
#   #initialize state space list
#   Boolean_state_space_list <- list()
#   node_names_list <- list()
#   
#   
#   parameter_names <- names(parameters)
#   
#   #generate matrix of all possible combinations
#   node_matrix <- as.matrix(expand.grid(lapply(numeric(length(variables)), function(x) c(FALSE,TRUE))), ncol=length(variables)) #http://www.di.fc.ul.pt/~jpn/r/combinatorics/combinatorics.html
#   # print(node_matrix)
#   
#   if (boolean == TRUE) {
#     parameters <- as.logical(parameters)
#     names(parameters) <- parameter_names
#   } else {
#     parameters <- as.numeric(parameters)
#     names(parameters) <- parameter_names
#   }
#   
#   if(boolean == FALSE) {
#     node_matrix <- as.matrix(expand.grid(lapply(numeric(length(variables)), function(x) c(0,1))), ncol=length(variables))
#   }
#   
#   colnames(node_matrix) <- variables
#   # print(node_matrix)
#   
#   
#   for (f in 1:nrow(node_matrix)) {
#     ####generate local variables
#     
#     #assign variable values to variable names
#     for (i in 1:length(variables)) {
#       assign(variables[i], node_matrix[f,i])
#       # print(node_matrix[f,i])
#     }
#     #assign parameter values to parameter names
#     for (j in 1:length(parameters)) {
#       assign(parameter_names[j], parameters[j])
#       # print(eval(as.name(parameter_names[j])))
#     }
#     
#     #initialize matrix
#     Boolean_state_space_list[[f]] <- matrix(nrow = updates, ncol = length(c(variables, parameter_names)))
#     colnames(Boolean_state_space_list[[f]]) <- c(variables, parameter_names)
#     rownames(Boolean_state_space_list[[f]]) <- c(1:nrow(Boolean_state_space_list[[f]]))
#     row.names(Boolean_state_space_list[[f]])[1] <- c("t")
#     
#     Boolean_state_space_list[[f]][1,] <- c(node_matrix[f,], parameters)
#     
#     
#     for (a in 1:nrow(Boolean_state_space_list[[f]])) {
#       for (b in (length(parameters)-1):0) {
#         Boolean_state_space_list[[f]][a, ncol(Boolean_state_space_list[[f]]) - b] <- parameters[length(parameters) - b]
#       }
#       
#     }
#     
#     
#     # Get correct environment
#     env <- pryr::where(colnames(node_matrix)[1])
#     # print(env())
#     # print(Boolean_state_space_list[[f]]) #test to see if matrix was initialized succesfully
#     # print(c(M, E, L, G_e, L_e)) #test to see if local variables were successfully defined within the function
#     
#     
#     #Synchronous update of matrix  
#     for (k in 2:nrow(Boolean_state_space_list[[f]])) {
#       
#       for (h in 1:length(variables)) {
#         #Set environment to evaluate formula in
#         rlang::f_env(logic[[h]]) <- env
#         
#         Boolean_state_space_list[[f]][k, variables[h]] <- lazyeval::f_eval(logic[[h]])
#         
#       }
#       
#       for (g in 1:length(variables)) {
#         assign(variables[g], Boolean_state_space_list[[f]][k, g])
#         
#         
#       }
#       
#       
#       
#     }
#     # as.numeric(matrix(c(TRUE, FALSE, TRUE, TRUE), nrow = 2))
#     ###plotting diagram for state space
#     
#     
#     
#     node_names_vector <- c()
#     for (e in 1:nrow(Boolean_state_space_list[[f]])) {
#       print("test")
#       print(Boolean_state_space_list[[f]][e, 1:length(variables) ])
#       node_names_vector[e] <- paste(as.character(if (boolean == FALSE) {
#         Boolean_state_space_list[[f]][e, 1:length(variables) ]
#       } else {
#         as.numeric(Boolean_state_space_list[[f]][e, 1:length(variables) ])
#       }
#       ), collapse = " ")
#       node_names_list[[f]] <- node_names_vector
#     }
#     
#   }
#   print(node_names_vector)
#   print(Boolean_state_space_list)
#   
#   #plot
#   for (d in 1:length(node_names_list)) {
#     node_names_list[[d]] <- rep(node_names_list[[d]], each = 2)
#     node_names_list[[d]] <- node_names_list[[d]][c(-1, -length(node_names_list[[d]]))]
#     # print(node_names_list[[d]])
#     # plot(igraph::graph(node_names_list[[d]]))
#   }
#   state_space_vector <- unlist(node_names_list)
#   print(state_space_vector)
#   
#   plot(igraph::graph(state_space_vector))
#   # state_space_diagram <- igraph::graph(state_space_vector)
#   # layout <- layout_nicely(state_space_diagram, root = 8)
#   # plot(state_space_diagram, layout = layout)
#   
#   
#   
#   
# }
# 

#alternative to assigning env in the main function
# param_list_1 <- split(unname(named_parameter_vector), names(named_parameter_vector))
# 
# param_list <- c(split(unname(named_variable_vector), names(named_variable_vector)), param_list_1)
# class(param_list)
# cols <- names(named_variable_vector)
# 
# 
# 
# list(list(c(1,2), c(3,4)))




