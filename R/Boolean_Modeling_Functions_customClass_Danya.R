# install.packages("lazyeval")
# install.packages("pryr")
# install.packages("DiagrammeR")
# install.packages("compositions")

setClassUnion("numericORlogical", c("numeric", "logical"))


test <- TestClass(names = c("a", "b"))

BoolInfo <- setClass(
  # Set the name for the class
  "BoolInfo",
  
  #Define the slots
  
  slots = c(variables = "character",
            parameters = "character",
            logic = "list",
            parameter_values = "numericORlogical",
            KD = "character"
  )
  
  # , validity = function(object) {
  # 
  #   if (length(object@parameter_values) != length(object@parameters) ) {
  #     warning("The amount of parameter values is not equal to the amount of parameter names. All parameter values have been set to 1.")
  #     print("test1")
  #     object@parameter_values <- rep(1, length(object@parameters))
  #     print(object@parameter_values)
  #   } else {
  #     object@parameter_values <- as.numeric(object@parameter_values)
  #   }
  # }
  # , validity=function(object)
  # {
  #   if(is.null(names(object@parameters))) {
  #     return("Parameters vector is not named. Use: names(parameter_vector) <- c(character vector of parameter names)")
  #   } else {
  #     return(TRUE)
  #     
  #   }
  # }
)

BoolInfo_Lac <- BoolInfo(variables = variables_Lac, parameters = parameters_Lac, logic = logic_Lac)


variables_Lac <- c("M", "E", "L")
parameters_Lac <- c("G_e", "L_e")
parameter_values_Lac <- c(0, 1)
logic_Lac <- list(~(!G_e) & (L | L_e), ~M , ~(!G_e) & E & L_e) #logic_list <- list(~((G_e + 1)*(L + L_e + L*L_e))%%2, ~M , ~((G_e + 1)*E*L_e)%%2)

BoolInfo_Lac <- BoolInfo(variables = variables_Lac, parameters = parameters_Lac, logic = logic_Lac)

Boolean_state_space(BoolInfo_Lac, updates = 5)

BoolInfoResults_Lac <- BoolInfoResults(BoolInfo_Lac, updates = 5)
BoolInfoResults_Lac[[2]]

BoolBuildR <- function(BoolInfo, igraph = FALSE) {
  
  
  logic_list <- BoolInfo@logic
  
  #create list of the RHS of each formula
  for (i in 1:length(logic_list)) {
    logic_list[[i]] <- logic_list[[i]][2]
  }
  
  #remove all the special chars, except !, and string split
  logic_list <- lapply(logic_list, as.character)
  logic_list <- lapply(logic_list, gsub, pattern = "\\~", replacement = "" )
  logic_list <- lapply(logic_list, gsub, pattern = "OR|or|Or|\\|", replacement = "" )
  logic_list <- lapply(logic_list, gsub, pattern = "AND|and|\\&", replacement = "" )
  logic_list <- lapply(logic_list, gsub, pattern = "  ", replacement = " " )
  logic_list <- lapply(logic_list, gsub, pattern = "\\(|\\)", replacement = "" )
  # print(logic_list)
  
  logic_list_2 <- lapply(logic_list, gsub, pattern = "\\!", replacement = "" )
  
  logic_list <- lapply(logic_list, strsplit, split = " " )
  # print(logic_list)
  
  #create to_vector
  to <- list()
  
  for (j in 1:length(BoolInfo@variables)) {
 
    to[[j]] <- rep(BoolInfo@variables[j], length(logic_list[[j]][[1]]))
  }

  to <- unlist(to)
  # print(to)
  #create arrows type vector

  arrows.to.type <- c()
  color <- c()
  
  for (k in 1:length(to)) {
    
    if (stringr::str_detect(unlist(logic_list), pattern = "\\!")[k]) {
      arrows.to.type[k] <-  "circle"
      color[k] <- "red"
    } else {
      arrows.to.type[k] <- "arrow"
      color[k] <- "blue"
    }
  }
  
  #create from vector with list with no !
  logic_list_2 <- lapply(logic_list_2, strsplit, split = " " )
  
  from <- unlist(logic_list_2)
  # print(from)

edges <- as.data.frame(cbind(from, to, arrows.to.type, color))
BoolBuildR_edges <<- edges

nodes <- data.frame(id = c(BoolInfo@variables, BoolInfo@parameters), label = c(BoolInfo@variables, BoolInfo@parameters), color = c(rep("lightblue",length(BoolInfo@variables)), rep("lightgreen",length(BoolInfo@parameters))),
                    shape = rep("circle", length(c(BoolInfo@variables, BoolInfo@parameters))), value = rep(50, length(c(BoolInfo@variables, BoolInfo@parameters))) )
# print(nodes)

if (igraph) { visNetwork(nodes, edges) %>% visPhysics(stabilization = T) %>% visIgraphLayout(layout = "layout_with_kk")} else {visNetwork(nodes, edges)}

  
}

BoolInfo_test <- BoolInfo(parameters = c("P1", "P2"), variables = c("x1", "x2", "x3", "x4", "x5", "x6"), logic = list(~P1, ~P1 | P2, ~x1 | x2, ~(!(x5) | !(x6)) & x3, ~(x1 | x4), ~(!(x2) & !(P2))))
BoolInfo_test <- BoolInfo(variables = c("x1", "x2", "x3"), logic = list(~x2, ~x1 | x3, ~(x1 & x2) | x3))
BoolBuildR(BoolInfo_test)

BoolBuildR(BoolInfo_test) %>% visIgraphLayout(layout = "layout_with_dh")
BoolBuildR(BoolInfo_AS) %>% visIgraphLayout(layout = "layout_with_dh")



logic_text <- lapply(logic_text, gsub, pattern = "’", replacement = "")
logic_text <- lapply(logic_text, gsub, pattern = "OR|or|Or", replacement = "|")
logic_text <- lapply(logic_text, gsub, pattern = "AND|and", replacement = "&")
logic_text <- lapply(logic_text, gsub, pattern = "NOT", replacement = "!")


logic_text_RHS <- lapply(logic_text, gsub, pattern = ".*= ", replacement = "")
logic_text_RHS <- lapply(logic_text_RHS, function(x) paste0("~", x))
logic_text_RHS <- lapply(logic_text_RHS, noquote)
logic_text_RHS <- lapply(logic_text_RHS, as.formula)
logic_text_RHS





BoolInfoResults <- function(BoolInfo, updates = 15, NetworkDirectory, ResultsDirectory) {
  
 #initialize matrix of all possible permutations for boolean parameter values
  parameter_matrix <- as.matrix(expand.grid(lapply(numeric(length(BoolInfo@parameters)), function(x) c(0,1))), ncol=length(BoolInfo@parameters))
  
  #initialize list which will contain the decimal state space matrix for each parameter permutation
  Decimal_State_Space_Matrix_list <- list()
  
  #initialize list which will contain the BoolInfo for each parameter permutation
  BoolInfo_list <- list()

  #generate aforementioned list
  for (z in 1:nrow(parameter_matrix)) {
        
    BoolInfo@parameter_values <- as.numeric(parameter_matrix[z, ])
    
    BoolInfo_list[[z]] <- BoolInfo
    names(BoolInfo_list)[z] <- paste0(sub(", $", "" , paste0(BoolInfo@parameters, ":", BoolInfo@parameter_values, ", ", collapse = "")), if (length(BoolInfo@KD > 0)) { paste0("_KD_", BoolInfo@KD )})
    
    Decimal_State_Space_Matrix_list[[z]] <- decimal_state_space_matrix(Boolean_state_space(BoolInfo = BoolInfo , updates = updates), BoolInfo = BoolInfo )
    names(Decimal_State_Space_Matrix_list)[z] <- paste0(sub(", $", "" , paste0(BoolInfo@parameters, ":", BoolInfo@parameter_values, ", ", collapse = "")), if (length(BoolInfo@KD > 0)) { paste0("_KD_", BoolInfo@KD )})
    

    if (!missing(ResultsDirectory)) {
      
      file_name_suffix <- paste0(sub("_$", "" , paste0(BoolInfo@parameters,  BoolInfo@parameter_values, "_", collapse = "")), if (length(BoolInfo@KD > 0)) { paste0("_KD_", BoolInfo@KD )})
      
      Directory_Results <-  paste0(ResultsDirectory, file_name_suffix, "/")
  
      if (dir.exists(Directory_Results) == FALSE) {
        dir.create(Directory_Results, recursive = TRUE)
      } else {
        "Already made this directory!"
      }
      
      write.csv(Decimal_State_Space_Matrix_list[[z]], file = paste0(Directory_Results, file_name_suffix, "_decimal_state_space_matrix.csv"))
   

      pdf(file = paste0( Directory_Results, file_name_suffix, "_heatmap.pdf"),
                                  width=16.25, height=12.5)
      
      pheatmap(Decimal_State_Space_Matrix_list[[z]], cluster_rows = TRUE, cluster_cols = FALSE)
   
     dev.off()
      
    }
    
    ###################
    
    if (!missing(NetworkDirectory)) {
      
      file_name_suffix <- paste0(sub("_$", "" , paste0(BoolInfo@parameters,  BoolInfo@parameter_values, "_", collapse = "")), if (length(BoolInfo@KD > 0)) { paste0("_KD_", BoolInfo@KD )})
      
      Directory_Networks <-  paste0(NetworkDirectory, file_name_suffix, "/")
      
      if (dir.exists(Directory_Networks) == FALSE) {
        dir.create(Directory_Networks, recursive = TRUE)
      } else {
        "Already made this directory!"
      }
      
      State_Space_update_break <- Boolean_state_space(BoolInfo = BoolInfo , updates = updates, update_break = TRUE)
      
      State_Space_nodes <- state_space_dataframe(State_Space_update_break, BoolInfo, toDecimal = TRUE, nodes = T, edges = F)
      
      State_Space_edges <- state_space_dataframe(State_Space_update_break, BoolInfo, toDecimal = TRUE, nodes = F, edges = T)
      
      State_Space_igraph <- state_space_dataframe(State_Space_update_break, BoolInfo, toDecimal = TRUE, nodes = F, edges = F) 
      
      State_Space_network <- visNetwork(State_Space_nodes, State_Space_edges) %>% visEdges(arrows = list(to = list(enabled = TRUE, 
                                        scaleFactor = 0.25)), smooth = FALSE) %>% visNodes(size = 3) %>% visPhysics(stabilization = F) %>% visIgraphLayout(layout = "layout_with_kk")
      
      visSave(graph = State_Space_network, file = paste0(Directory_Networks, file_name_suffix, "_network_kk.html"))

    }
    
  }

  
  #create matrix of all possible combiantions of each element of the Decimal_State_Space_Matrix_list list
  permutations_matrix <- gtools::permutations(n = length(1:length(Decimal_State_Space_Matrix_list)), r = 2, v = (1:length(Decimal_State_Space_Matrix_list)) )

  #initialize list of comparisons
  predictions_summary_list <- list()
  
  for (y in 1:nrow(permutations_matrix)) {
    predictions_summary_list[[y]] <- predictions_summary(initial_BoolInfo = BoolInfo_list[[permutations_matrix[y, 1]]], final_BoolInfo = BoolInfo_list[[permutations_matrix[y, 2]]],
      initial_state_matrix = Decimal_State_Space_Matrix_list[[permutations_matrix[y, 1]]], final_state_matrix = Decimal_State_Space_Matrix_list[[permutations_matrix[y, 2]]] )

  }
predictions_summary_list
predictions_matrix <- do.call(rbind, predictions_summary_list)

if (!missing(ResultsDirectory)) {
  
  write.csv(predictions_matrix, file = paste0(ResultsDirectory, "predictions_matrix.csv"))

  }

predictions_matrix
}


BoolInfoResults_Lac <- BoolInfoResults(BoolInfo_Lac, updates = 5, NetworkDirectory = "/Users/gordid/Desktop/Modeling/Lac_Operon/Boolean/Networks/", ResultsDirectory = "/Users/gordid/Desktop/Modeling/Lac_Operon/Boolean/Results/")
do.call(rbind, BoolInfoResults_Lac)
BoolInfoResults_Lac[[1]][[2]]@variables

gtools::permutations(n = length(1:8), r = 2, v = (1:8) )

a <- list(list(matrix(data = c(1,2,3,4), nrow = 2), list("a", "b")), list(1, 2))
names(a)[1] <- "test"
a[[1]][[1]]
a[[1]][2]

####Boolean_state_space function. Parameters argument can be either a named logical or numerical vector. Also, takes both algebraic and boolean logic as the logic argument####
Boolean_state_space <- function(BoolInfo, updates = 15, update_break = FALSE) {
  
  #set variables within the function
  variables <- BoolInfo@variables
  
  logic <- BoolInfo@logic
  
  parameters <- BoolInfo@parameters
  
  if (length(BoolInfo@parameter_values) != length(BoolInfo@parameters) ) {
    warning("The amount of parameter values is not equal to the amount of parameter names. All parameter values have been set to 1.")
    parameter_values <- rep(1, length(BoolInfo@parameters))
  } else {
    parameter_values <- as.numeric(BoolInfo@parameter_values)
  }
  
  if (length(BoolInfo@KD) > 0) {
    variables <- variables[-which(variables == BoolInfo@KD)] 
    logic <- logic[-which(BoolInfo@variables == BoolInfo@KD)]
    parameters <- append(parameters, BoolInfo@KD)
    parameter_values <- append(parameter_values, rep(0, length(BoolInfo@KD)))
    
  }
  
  #initialize state space list
  Boolean_state_space_list <- list()
  node_names_list <- list()
  
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
    if (length(parameters) > 0) {
      for (j in 1:length(parameter_values)) {
        assign(parameters[j], parameter_values[j])
        # print(eval(as.name(parameters[j])))
      }
    }
    # colnames(node_matrix) <- variables
    
    #initialize matrix
    Boolean_state_space_list[[f]] <- matrix(nrow = updates, ncol = length(c(variables, parameters)))
    colnames(Boolean_state_space_list[[f]]) <- c(variables, parameters)
    rownames(Boolean_state_space_list[[f]]) <- c(1:nrow(Boolean_state_space_list[[f]]))
    row.names(Boolean_state_space_list[[f]])[1] <- c("t")
    
    counter <- 1
    for (e in 1:(nrow(Boolean_state_space_list[[f]])-1)) {
      row.names(Boolean_state_space_list[[f]])[e + 1] <- paste("t", "+", counter, sep = " ")
      counter <- counter + 1
    }
    
    
    Boolean_state_space_list[[f]][1,] <- c(node_matrix[f,], parameter_values)
    
    if (length(parameters) > 0) {
      for (a in 1:nrow(Boolean_state_space_list[[f]])) {
        for (b in 1:length(parameters)) {
          Boolean_state_space_list[[f]][a, (length(variables) + b)] <- parameter_values[b]
        }
        
      }
    }
    
    # Get correct environment
    env <- pryr::where(variables[1])
    #print(env)
    #test to see if matrix was initialized succesfully
    # print(Boolean_state_space_list[[f]]) 
    #test to see if local variables were successfully defined within the function:
    # print(variables[1])
    # print(eval(parse(text = variables[1])))
    
    
    #Synchronous update of matrix  
    for (k in 2:nrow(Boolean_state_space_list[[f]])) {
      
      for (h in 1:length(variables)) {
        #Set environment to evaluate formula in
        rlang::f_env(logic[[h]]) <- env
        
        # print("TEST TO SEE IF ITS WORKING")
        # print(paste0("test_", "f:", f, "k:", k, "h:", h, "f_eval:", lazyeval::f_eval(logic[[h]])))
        
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
  
  if (length(BoolInfo@KD) > 0) {
    for (m in 1:length(Boolean_state_space_list)) {
      Boolean_state_space_list[[m]] <- Boolean_state_space_list[[m]][ , c(BoolInfo@variables, BoolInfo@parameters)]
      
    }
    
  }
  
  names(Boolean_state_space_list) <- 0:(length(Boolean_state_space_list)-1)
  Boolean_state_space_list
  
}


Boolean_state_space_list_AS_011_KD_Esrrb

####Decimal state space matrix####

decimal_state_space_matrix <- function(boolean_state_space_list, BoolInfo, as_range = FALSE) {
  
  number_of_variables <- length(BoolInfo@variables)
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

state_space_dataframe <- function(boolean_state_space_list, BoolInfo, toDecimal = TRUE, nodes = TRUE, edges = FALSE) {
  
  number_of_variables <-  length(BoolInfo@variables)
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



#####state_space_diagram####
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

####LAC Operon####
variable_names_Lac <- c("M", "E", "L")
parameter_names_Lac <- c("G_e", "L_e")
parameter_values_Lac <- c(1, 1)

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

state_space_vector_Lac_igraph <- state_space_dataframe(Boolean_state_space_list_Lac_update_break, 3, toDecimal = TRUE, nodes = F, edges = F)

as.character(state_space_vector_Lac_igraph)
#plot  
plot(igraph::graph(state_space_vector_Lac_igraph))
graph <- igraph::graph(state_space_vector_Lac_igraph)

visIgraph(graph)
layout <- layout_as_tree(graph, root = 1)
plot(graph, layout = layout)

decimal_state_space_matrix_Lac_decimal[, c("t", "t + 4")]
