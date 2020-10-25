# install.packages("lazyeval")
# install.packages("pryr")
# install.packages("compositions")

#create a virtual class defined as a superclass of several other classes
setClassUnion("numericORlogical", c("numeric", "logical"))

#function that creates new BoolInfo class, and then creates a BoolInfo object
CreateBoolInfo <- function(variables, logic, signals = NULL, signal_values = NULL, KD = NULL) {
  
  #create class
  BoolInfo <<- setClass(
    # Set the name for the class
    "BoolInfo",
    
    #Define the slots
    
    slots = c(variables = "character",
              signals = "character",
              logic = "list",
              signal_values = "numericORlogical",
              KD = "character"
    )
  )
  
  if (!is.null(signals)) {

    if (length(signal_values) != length(signals) ) {
      warning("The amount of signal values is not equal to the amount of signal names. All signal values have been set to 1.")
      signal_values <- rep(1, length(signals))
    } else {
      signal_values <- as.numeric(signal_values)
    }
  } else {
    
    signals <- character(0)
    signal_values <- numeric(0)
    
  }
  
  if (is.null(KD)) { 
    
    KD <- character()
  }
  
  new("BoolInfo", variables = variables, signals = signals, logic = logic, signal_values = signal_values,  KD = KD)
  

}


####Function for converting text to logic formula and/or vector of variables. Works only on the standard way logic is written in text files####
BuildLogic <- function(logic_text, getVariables = TRUE) {
  
  logic_var_list <- vector(mode = "list", length = 2)
  names(logic_var_list) <- c("Logic", "Variables")
  
  if (getVariables) {
    
    logic_text_var <- unname(unlist(lapply(logic_text, gsub, pattern = "’.*", replacement = "")))
    logic_var_list$Variables <- logic_text_var
    
}

    logic_text <- lapply(logic_text, gsub, pattern = "’", replacement = "")
    logic_text <- lapply(logic_text, gsub, pattern = "OR|or|Or", replacement = "|")
    logic_text <- lapply(logic_text, gsub, pattern = "AND|and", replacement = "&")
    logic_text <- lapply(logic_text, gsub, pattern = "NOT", replacement = "!")
    
    logic_text_RHS <- lapply(logic_text, gsub, pattern = ".*= ", replacement = "")
    logic_text_RHS <- lapply(logic_text_RHS, function(x) paste0("~", x))
    logic_text_RHS <- lapply(logic_text_RHS, noquote)
    logic_text_RHS <- lapply(logic_text_RHS[[1]], as.formula)

    logic_var_list[[1]] <- logic_text_RHS 
    
    logic_var_list 
    
}


####Plot Boolean Network from BoolInfo object####
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
  
  nodes <- data.frame(id = c(BoolInfo@variables, BoolInfo@signals), label = c(BoolInfo@variables, BoolInfo@signals), color = c(rep("lightblue",length(BoolInfo@variables)), rep("lightgreen",length(BoolInfo@signals))),
                      shape = rep("circle", length(c(BoolInfo@variables, BoolInfo@signals))), value = rep(50, length(c(BoolInfo@variables, BoolInfo@signals))) )
  # print(nodes)
  
  if (igraph) { visNetwork(nodes, edges) %>% visPhysics(stabilization = T) %>% visIgraphLayout(layout = "layout_with_kk")} else {visNetwork(nodes, edges)}
  
  
}


####TrajectoryList Function####
TrajectoryList <- function(BoolInfo, updates = 15, update_break = FALSE) {
  
  #set variables within the function
  variables <- BoolInfo@variables
  
  logic <- BoolInfo@logic
  
  signals <- BoolInfo@signals
  
  signal_values <- BoolInfo@signal_values
  
  if (length(BoolInfo@KD) > 0) {
    variables <- variables[-which(variables == BoolInfo@KD)] 
    logic <- logic[-which(BoolInfo@variables == BoolInfo@KD)]
    
    signal_values <- append(BoolInfo@signal_values, rep(0, length(BoolInfo@KD)))
               
    signals <- append(signals, BoolInfo@KD)
    
  }
  
  #initialize state space list
  trajectory_list <- list()
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
    #assign signal values to signal names
    if (length(signals) > 0) {
      for (j in 1:length(signal_values)) {
        assign(signals[j], signal_values[j])
        # print(eval(as.name(signals[j])))
      }
    }
    # colnames(node_matrix) <- variables
    
    #initialize matrix
    trajectory_list[[f]] <- matrix(nrow = updates, ncol = length(c(variables, signals)))
    colnames(trajectory_list[[f]]) <- c(variables, signals)
    rownames(trajectory_list[[f]]) <- c(1:nrow(trajectory_list[[f]]))
    row.names(trajectory_list[[f]])[1] <- c("t")
    
    counter <- 1
    for (e in 1:(nrow(trajectory_list[[f]])-1)) {
      row.names(trajectory_list[[f]])[e + 1] <- paste("t", "+", counter, sep = " ")
      counter <- counter + 1
    }
    
    
    trajectory_list[[f]][1,] <- c(node_matrix[f,], signal_values)
    
    if (length(signals) > 0) {
      for (a in 1:nrow(trajectory_list[[f]])) {
        for (b in 1:length(signals)) {
          trajectory_list[[f]][a, (length(variables) + b)] <- signal_values[b]
        }
        
      }
    }
    
    # Get correct environment
    env <- pryr::where(variables[1])
    #print(env)
    #test to see if matrix was initialized succesfully
    # print(trajectory_list[[f]]) 
    #test to see if local variables were successfully defined within the function:
    # print(variables[1])
    # print(eval(parse(text = variables[1])))
    
    
    #Synchronous update of matrix  
    for (k in 2:nrow(trajectory_list[[f]])) {
      
      for (h in 1:length(variables)) {
        #Set environment to evaluate formula in
        rlang::f_env(logic[[h]]) <- env
        
        # print("TEST TO SEE IF ITS WORKING")
        # print(paste0("test_", "f:", f, "k:", k, "h:", h, "f_eval:", lazyeval::f_eval(logic[[h]])))
        
        trajectory_list[[f]][k, variables[h]] <- lazyeval::f_eval(logic[[h]]) #logicals get converted to numericals before being assigned to the matrix. This is a property of matrices.
      }
      
      for (g in 1:length(variables)) {
        assign(variables[g], trajectory_list[[f]][k, g])
        
      }
      if (update_break == TRUE) {
        if (identical(trajectory_list[[f]][k, ], trajectory_list[[f]][k-1, ]) & identical(trajectory_list[[f]][k, ], trajectory_list[[f]][k-2, ]))
        {
          #trajectory_list[[f]] <- trajectory_list[[f]][1:(k-1), , drop = FALSE ]
          #trajectory_list[[f]][k:nrow(trajectory_list[[f]]), ] <- trajectory_list[[f]][k, ]
          
          trajectory_list[[f]][k:updates, ] <- rep(trajectory_list[[f]][k, ], each = (updates - k + 1))
          break 
        }
      }
    }
    
  }
  
  if (length(BoolInfo@KD) > 0) {
    for (m in 1:length(trajectory_list)) {
      trajectory_list[[m]] <- trajectory_list[[m]][ , c(BoolInfo@variables, BoolInfo@signals)]
      
    }
    
  }
  
  names(trajectory_list) <- 0:(length(trajectory_list)-1)
  trajectory_list
  
}

####StateSpace Function####
#Used for constructing the heatmaps

StateSpace <- function(trajectory_list, BoolInfo, as_range = FALSE) {
  
  number_of_variables <- length(BoolInfo@variables)
  #initialize matrix
  decimal_matrix <- matrix(nrow = length(trajectory_list), ncol = nrow(trajectory_list[[1]]))
  row.names(decimal_matrix) <- 1:length(trajectory_list)
  colnames(decimal_matrix) <- rownames(trajectory_list[[1]])
  
  for (i in 1:length(trajectory_list)) {
    for (j in 1:ncol(decimal_matrix)) {
      decimal_matrix[i, j] <- compositions::unbinary(paste(trajectory_list[[i]][j,1:number_of_variables ], collapse = ""))
      
    }
    
    rownames(decimal_matrix)[i] <- paste(trajectory_list[[i]][1,1:number_of_variables ], collapse = "")
  }
  if (as_range == TRUE) {
    decimal_matrix <- decimal_matrix/(2**number_of_variables-1)
  }
  decimal_matrix
}

#AttractorStates Function
AttractorStates <- function(StateSpace, BoolInfo) {
  
  attractors_list <- list()
  
  unique_attractors <- sort(unique(StateSpace[, ncol(State_Space)]), decreasing = T)
  
  for (i in 1:length(unique_attractors)) {
    
    attractors_list[[i]] <- strsplit(binary(unique_attractors[i], mb = (length(BoolInfo@variables)-1)), "")
    
  }
  names(attractors_list) <- unique_attractors
 
  attractors_df <- data.frame(matrix(unlist(attractors_list), nrow=length(attractors_list), byrow=T))
  colnames(attractors_df) <- BoolInfo@variables
  rownames(attractors_df) <- unique_attractors
  attractors_df
}


####StateSpacePlotComponents Function####
#Used for plotting the state space diagrams

StateSpacePlotComponents <- function(trajectory_list, BoolInfo, toDecimal = TRUE, nodes = TRUE, edges = TRUE, igraph = TRUE) {
  
  state_space_plot_components_list <- vector("list", length = 3)
  names(state_space_plot_components_list) <- c("nodes", "edges", "igraph")
  
  number_of_variables <-  length(BoolInfo@variables)
  node_names_list <- list()
  
  for (f in 1:length(trajectory_list)) {
    node_names_vector <- c()
    for (e in 1:nrow(trajectory_list[[f]])) {
      node_names_vector[e] <- paste(as.character(trajectory_list[[f]][e, 1:number_of_variables ]), collapse = "")
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
    state_space_vector <- as.character(compositions::unbinary(state_space_vector))
  }
  
  
  if (nodes) {
    state_space_plot_components_list$nodes <- data.frame(id = unique(state_space_vector), title = paste0("<p><b>", unique(state_space_vector),"</b><br></p>"))
  }
  
  if (edges) {
    edges_df <- matrix(state_space_vector, ncol = 2, byrow = TRUE)
    edges_df <- as.data.frame(edges_df, stringsAsFactors = FALSE)
    colnames(edges_df) <- c("from", "to")
    
    state_space_plot_components_list$edges <- edges_df
  }
  
  if (igraph) {
    state_space_plot_components_list$igraph <- state_space_vector
  }
  
  state_space_plot_components_list
}


####StateSpaceDiagram Function####
StateSpaceDiagram <- function (state_space_plot_components_list, BoolInfo) {
  
  if (length(BoolInfo@variables) > 5) {
    
    visNetwork <- visNetwork(state_space_plot_components_list$nodes, state_space_plot_components_list$edges)  %>%
    visEdges(arrows = list(to = list(enabled = TRUE,  scaleFactor = 0.25)), smooth = FALSE)  %>% visNodes(size = 3) %>% visPhysics(stabilization = F) %>% visIgraphLayout(layout = "layout_with_kk")
    
  } else {
    
    state_space_plot_components_list$nodes <- cbind(state_space_plot_components_list$nodes, label = state_space_plot_components_list$nodes$id)
    # print(state_space_plot_components_list$nodes )
    
    visNetwork <- visNetwork(state_space_plot_components_list$nodes, state_space_plot_components_list$edges)  %>%
    visEdges(arrows = list(to = list(enabled = TRUE,  scaleFactor = 0.25)), smooth = TRUE) %>% visNodes(shape = "circle", scaling = list(label = list(enabled = T)))
  }
  
  visNetwork
}


####Results Matrix####
predictions_summary <- function(initial_BoolInfo, final_BoolInfo, initial_state_matrix, final_state_matrix) {
  
  file_name_suffix_initial <- paste0(sub(", $", "" , paste0(initial_BoolInfo@signals, ":",  initial_BoolInfo@signal_values, ", ", collapse = "")), if (length(initial_BoolInfo@KD > 0)) { paste0("_KD_", initial_BoolInfo@KD )})
  
  file_name_suffix_final <- paste0(sub(", $", "" , paste0(final_BoolInfo@signals, ":",  final_BoolInfo@signal_values, ", ", collapse = "")), if (length(final_BoolInfo@KD > 0)) { paste0("_KD_", final_BoolInfo@KD )})
  
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

BoolSimulate <- function(BoolInfo, updates = 15) {
  
  signal_matrix <- as.matrix(expand.grid(lapply(numeric(length(BoolInfo@signals)), function(x) c(0,1))), ncol=length(BoolInfo@signals))
  signal_matrix
  
  attractor_states_list <- list()
  
  row_names_list <- list()
  
  for (i in 1:nrow(signal_matrix)) {
    
    BoolInfo@signal_values <- signal_matrix[i, ]
    
    Trajectory_List <- TrajectoryList(BoolInfo = BoolInfo, updates = updates, update_break = T)
    
    State_Space <- StateSpace(BoolInfo = BoolInfo, trajectory_list = Trajectory_List)
    
    Attractor_States <- AttractorStates(StateSpace = State_Space, BoolInfo = BoolInfo)
    
    attractor_states_list[[i]] <- Attractor_States
    row_names_list[[i]] <- rownames(Attractor_States)
  }
  
  attractor_states_matrix <- dplyr::bind_rows(attractor_states_list)
  attractor_states_matrix <- unique(attractor_states_matrix[,])
  rownames(attractor_states_matrix) <- unique(unlist(row_names_list))
  attractor_states_matrix
  
}


####Using all other functions####
BoolInfoResults <- function(BoolInfo, updates = 15, NetworkDirectory, ResultsDirectory) {
  
  #initialize matrix of all possible permutations for boolean signal values
  signal_matrix <- as.matrix(expand.grid(lapply(numeric(length(BoolInfo@signals)), function(x) c(0,1))), ncol=length(BoolInfo@signals))
  
  #initialize list which will contain the decimal state space matrix for each signal permutation
  Decimal_State_Space_Matrix_list <- list()
  
  #initialize list which will contain the BoolInfo for each signal permutation
  BoolInfo_list <- list()
  
  #generate aforementioned list
  for (z in 1:nrow(signal_matrix)) {
    
    BoolInfo@signal_values <- as.numeric(signal_matrix[z, ])
    
    BoolInfo_list[[z]] <- BoolInfo
    names(BoolInfo_list)[z] <- paste0(sub(", $", "" , paste0(BoolInfo@signals, ":", BoolInfo@signal_values, ", ", collapse = "")), if (length(BoolInfo@KD > 0)) { paste0("_KD_", BoolInfo@KD )})
    
    Decimal_State_Space_Matrix_list[[z]] <- decimal_state_space_matrix(Boolean_state_space(BoolInfo = BoolInfo , updates = updates), BoolInfo = BoolInfo )
    names(Decimal_State_Space_Matrix_list)[z] <- paste0(sub(", $", "" , paste0(BoolInfo@signals, ":", BoolInfo@signal_values, ", ", collapse = "")), if (length(BoolInfo@KD > 0)) { paste0("_KD_", BoolInfo@KD )})
    
    
    if (!missing(ResultsDirectory)) {
      
      file_name_suffix <- paste0(sub("_$", "" , paste0(BoolInfo@signals,  BoolInfo@signal_values, "_", collapse = "")), if (length(BoolInfo@KD > 0)) { paste0("_KD_", BoolInfo@KD )})
      
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
      
      file_name_suffix <- paste0(sub("_$", "" , paste0(BoolInfo@signals,  BoolInfo@signal_values, "_", collapse = "")), if (length(BoolInfo@KD > 0)) { paste0("_KD_", BoolInfo@KD )})
      
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




BoolSimulate(BoolInfo_Lac)
TrajectoryList(BoolInfo = BoolInfo_Lac, updates = 15, update_break = T)
StateSpace(TrajectoryList(BoolInfo = BoolInfo_Lac, updates = 15, update_break = T), BoolInfo = BoolInfo_Lac)
AttractorStates(StateSpace(TrajectoryList(BoolInfo = BoolInfo_Lac, updates = 15, update_break = T), BoolInfo = BoolInfo_Lac), BoolInfo = BoolInfo_Lac)

BoolInfo_Lac <- CreateBoolInfo(variables = variable_names_Lac, logic = logic_list, signals = parameter_names_Lac)
variable_names_Lac <- c("M", "E", "L")
parameter_names_Lac <- c("G_e", "L_e")
parameter_values_Lac <- c(1, 1)

logic_list <- list(~(!G_e) & (L | L_e), ~M , ~(!G_e) & E & L_e)
logic_list <- list(~((G_e + 1)*(L + L_e + L*L_e))%%2, ~M , ~((G_e + 1)*E*L_e)%%2)

BoolInfo_Lac_simple <- CreateBoolInfo(variables = variable_names_Lac_simple, logic = logic_list_simple, signals = parameter_names_Lac_simple)
variable_names_Lac_simple <- c("B", "P")
parameter_names_Lac_simple <- c("G_e", "L_e")
parameter_values_Lac_simple <- c(1, 1)

logic_list_simple <- list(~((!G_e) & L_e), ~((!G_e) & L_e))
BoolBuildR(BoolInfo_Lac_simple)
BoolSimulate(BoolInfo_Lac_simple)




