library(tidyverse)
library(datasets)




#Checks that the input arguments are valid
smote_nc <- function(df, var, k = 5, over_ratio = 1) {
  
  #Tests include:
  #only providing one majority/minority splitting variable
  #that variable needs to be a factor or a name of a factor
  #only need one nearest neighbor value greater than 1
  #the input variables need to be numeric and contain no NA values
  
  if (length(var) != 1) {
    rlang::abort("Please select a single factor variable for `var`.")
  }
  
  var <- rlang::arg_match(var, colnames(df))
  
  if (!(is.factor(df[[var]]) | is.character(df[[var]]))) {
    rlang::abort(paste0(var, " should be a factor or character variable."))
  }
  
  if (length(k) != 1) {
    rlang::abort("`k` must be length 1.")
  }
  
  if (k < 1) {
    rlang::abort("`k` must be non-negative.")
  }
  
  # if (is.null(cat_vars)) {
  #   rlang::warn("No categorical variables provided, attempting to determine automatically")
  #   cat_vars <- sapply(df,function(x) is.factor(x)|is.character(x)|is.logical(x))
  # }
  
  check_na(select(df, -all_of(var)), "smote")
  
  smote_impl(df, var, cat_vars, k, over_ratio)
}


#Splits data and appends new minority instances
smote_impl <- function(df, var, cat_vars, k, over_ratio) {
  
  #split data into list names by classes
  data <- split(df, df[[var]])
  #Number of majority instances
  majority_count <- max(table(df[[var]]))
  #How many minority samples do we want in total?
  ratio_target <- majority_count * over_ratio
  #How many classes do we need to upsample (account for 2+ classes!)
  #Get the indices of those classes
  which_upsample <- which(table(df[[var]]) < ratio_target)
  #For each minorty class, determine how many more samples are needed
  samples_needed <- ratio_target - table(df[[var]])[which_upsample]
  #Just saving the names of those classes
  min_names <- names(samples_needed)

  
  #Create a list to save all the new minority classes
  out_dfs <- list()
  
  #Loop through all the minorty classes, this will only loop once if there is only one minorit class
  for (i in seq_along(samples_needed)) {
    #Extract the minority dataframe
    minority <- data[[min_names[i]]]
 
    
    #Ensure that we have more minority isntances than desired neighbors
    if (nrow(minority) <= k) {
      rlang::abort(paste0(
        "Not enough observations of '", min_names[i],
        "' to perform SMOTE."
      ))
    }
    
    #Run the smote algorithm (minority data, # of neighbors, # of sampeles needed)
    out_df <- smote_data(minority, k = k, n_samples = samples_needed[i])
    out_dfs[[i]] <- out_df
  }
  
  #Bind all of the synthesized minority classes together
  final <- rbind(df, do.call(rbind, out_dfs))
  #Make sure the levels are correct for every categorial variable (needed?)
  final[[var]] <- factor(final[[var]], levels = levels(df[[var]]))
  rownames(final) <- NULL
  final
}

#Uses nearest-neighbors and interpolation to generate new instances
smote_data <- function(data, k, n_samples, smote_ids = seq_len(nrow(data))) {
  
  #Runs a nearest neighbor search
  #outputs a matrix, each row is a minorty instance and each column is a nearest neighbor
  #k is +1 because the sample is always a nearest neighbor to itself
  ids <- t(gower::gower_topn(x=data, y=data, n=k+1)$index)
  
  #shuffles minority indicies and repeats that shuffling until the desired number of samples is reached
  indexes <- rep(sample(smote_ids), length.out = n_samples)
  #tabulates how many times each minority instance is used 
  index_len <- tabulate(indexes, NROW(data))
  
  #Initialize matrix for newly generated samples
  out <- data[rep(smote_ids, length.out = n_samples),]
  
  #For each new sample pick a random nearest neighbor to interpoate with (1 to k)
  sampleids <- sample.int(k, n_samples, TRUE)
  #pick distance along parameterized line between current sample and chosen nearest neighbor
  runif_ids <- stats::runif(n_samples)
  
  iii <- 0
  for (row_num in smote_ids) {
    #List indices from 1:n where n is the number of times that sample is used to generate a new sample
    #iii shifts 1:n to fill in the rows of out (e.g. 1:3, 4:6, 7:8, etc.)
    index_selection <- iii + seq_len(index_len[row_num])
    # removes itself as nearest neighbour
    id_knn <- ids[row_num, ids[row_num, ] != row_num]
    
    #need a total of index_len[row_num] new samples
    #calculates Xnew = X1 + t*(X1-Xnn)
    dif <- data[id_knn[sampleids[index_selection]],sapply(data,is.numeric)] - data[rep(row_num, index_len[row_num]),sapply(data,is.numeric)]
    gap <- dif * runif_ids[index_selection]
    out[index_selection,sapply(data,is.numeric)] <- data[rep(row_num, index_len[row_num]),sapply(data,is.numeric)] + gap
    
    # Replace categories with most frequent among nearest neighbors
    cat_to_upgrade <- data[id_knn[sampleids[index_selection]],sapply(data,is.not.numeric)]
    
    if (is.data.frame(cat_to_upgrade)) {
      cat_modes <- as.data.frame( lapply(cat_to_upgrade,function(x) (rep(Mode(x),index_len[row_num]))) )
    } else {
      cat_modes <- Mode(cat_to_upgrade)
    }
    
    out[index_selection,sapply(data,is.not.numeric)] <- cat_modes
    
    
    iii <- iii + index_len[row_num]
  }
  
  out
}

#https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

is.not.numeric <- function(x) {!is.numeric(x)}




check_na <- function(data, step) {
  na_cols <- vapply(data, function(x) any(is.na(x)), FUN.VALUE = logical(1))
  if (any(na_cols)) {
    rlang::abort(paste0(
      "`", step,
      "` cannot have any missing values. NAs found ind: ",
      paste(names(na_cols)[na_cols], collapse = ", "), "."
    ))
  }
}


data(iris)
df <- iris[, c(1, 2, 5)]
df$Species <- factor(ifelse(df$Species == "setosa","rare","common")) 
# df <- rbind(df,df[df$Species=="common",])
df$Mood <- factor(ifelse(runif(nrow(df))<0.5,"Happy","Sad")) 
df$Color <- factor(ifelse(runif(nrow(df))<0.8,"Red","Blue")) 

out <- smote_nc(df, "Species")

out$syn <- c(rep(1,nrow(df)), rep(19,nrow(out)-nrow(df)))

par(mfrow=c(1,2))
plot(df$Sepal.Length, df$Sepal.Width, col=df$Species, pch = 1)
plot(out$Sepal.Length, out$Sepal.Width, col=out$Species, pch = out$syn)
