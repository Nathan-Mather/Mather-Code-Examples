#===================================#
# ==== Abraham And Sun Function ====
#===================================#
  # - Keywords  
  #  - #note I will use the tags #note for things I think are important
  #  - #set will be used for things that need to be set for your specific
  
  # - purpose of code:
  # Put Abraham and Sun Method into a generalizable function 
  # there extra stuff around the function for debugging. We can scrap all that when it's ready for a package 
  
  # -requirments to run 
  # Data.table package: I love data.table and it's fast. IMO it is worth the dependency. 


library(data.table) # for data manipulation 

#===========================#
# ==== helper functions ====
#===========================#
AS_dummy_f <- function(in_data, in_var){
  
                # get list of value for the variable 
                values <- sort(in_data[, unique(get(in_var))])
                
                # for each value, create a dummy
                for(val_i in values){
                  
                  # create a variable name and if it has a minus sign change the name for the new variable 
                  var_name <- paste0(in_var, "_", val_i)
                  var_name <- gsub("-", "m", var_name)
                  
                  # create variable in the data.table 
                  in_data[get(in_var) == val_i, (var_name) := 1]
                  in_data[get(in_var) != val_i, (var_name) := 0]
                  
                  
  }
  # return NULL since I am just editing the input data.set in global env
  return(NULL)
}

# this function takes a vector in_var_split.  It then splits the vector by in_val_delim and returns the in_val_position element of each split. 
ea_scan <- function (in_var_split = NULL, in_val_position = NULL, in_val_delim = NULL) 
{
  out_var <- sapply(strsplit(in_var_split, in_val_delim), function(x) x[in_val_position])
  return(out_var)
}





  

#================================#
# ==== Roxygen documentation ====
#================================#
  
  #' AS_IW
  #' 
  #' impliment methods from "Estimating Dynamic Treatment Effects in Event Studies
  #' with Heterogeneous Treatment Effects" by Abraham and Sun.
  #'
  #'@param in_data Your data set entered as a data.table
  #'@param in_id_var The Variable Name for an id variable identifying individuals in in_data. Entered as a quoted character 
  #'@param in_outcome_var The variable name for the outcome variable of interest. Entered as a quoted character 
  #'@param in_time_var The variable name for a numeric time variable in in_data. This is the period for a given row in the panel data. Entered as a quoted character 
  #'@param in_cohort_var The variable name for a numeric variable in in_data indicating the time of treatment. This needs to be in the same scale as the in_time var
  #' Entered as a quoted character 
  #'@param omitted_relative_treatment pick the relative treatment time effect to exclude as a control. The default is to find one for you (most likely to be -1), but if you want to pick explicitly than enter that here 
  #'@param in_weight_var An optional variable indicating the weight variable. This is for instances where treatment happens at group level, and data is collected ar the group level, but we want the regressions weighted by observation.
  #' @param opt_weight_iw an option to use the in_weight_var for weighting the cohort estimates in the IW estimate. in_weight_var must be specified if the option is set to TRUE. Default is FALSE
  #'@details See the working paper http://economics.mit.edu/files/14964
  #'@examples 
  
  
  
#======================#
# ==== AS Function ====
#======================#
# I see people using require a lot inside functions but the only differents is require throws a warning and library throws an error 
# the function is not going to work without these packages so I would rather it throw an error 
library(systemfit) # for Seamingly unrealated estimation 
library(lfe)       # for fixed effects regs 
library(survey)    # for linear combos 
  
  
AS_IW <- function(in_data                    = NULL,
                  in_id_var                  = NULL,
                  in_outcome_var             = NULL,
                  in_time_var                = NULL,
                  in_cohort_var              = NULL,
                  in_weight_var              = NULL,
                  opt_weight_iw              = FALSE,
                  omitted_relative_treatment = NULL){
  
  
  #====================#
  # ==== do checks ====
  #====================#
  
    # see if user has included a weight variable 
    weight_flag <- !is.null(in_weight_var)
    

    # check that in data is a data.table and exists 
    if(!any(class(in_data) == "data.table")){
      
      stop("in_data must be entered as a data.table")
    }
  
    # make list of variables we need 
    in_cols <- c(in_id_var, in_outcome_var, in_time_var, in_cohort_var, in_weight_var)
    
    # check that columns exist 
    for(col_i in in_cols){
      
      # check that it is a column
      if(!col_i %in% colnames(in_data)){
        stop(paste0(col_i, " is not a column name in in_data"))
      }

      
    }
    
    # check that in_cohort_var and in_time_var are numeric 
    if(!class(in_data[[in_cohort_var]]) %chin% c("numeric", "integer"))stop(paste0(in_cohort_var, " is not numeric or integer"))
    if(!class(in_data[[in_time_var]])  %chin% c("numeric", "integer"))stop(paste0(in_time_var, " is not numeric or integer"))
    
    # check if they set opt_weight_iw =TRUE but do not have a in_weight_var
    if(opt_weight_iw & !weight_flag){
      
      stop("You specificed opt_weight_iw == TRUE without including an in_weight_var. The opt_weight_iw option 
           weights the IW estimate by in_weight_var. So an in_weight_var must be included")
    }
    
    
  #==========================#
  # ==== and set up data ====
  #==========================#

    
    # Make our working data set 
    w_dt <- in_data[, in_cols, with = FALSE]
  

    # change the variable names in our working data set 
    #note not totally sure about this. We could just work with the in_var names the entire time but it 
    # can get a bit clunky and it's probably easier to just change names and put them back at the end. 
    # I could also do everything by numeric reference but I am not a fan of that. Too easy for things to break silently 
    if(weight_flag){
      setnames(w_dt, in_cols, c("ID", "outcome", "time", "cohort", "weight") )
    }else{
      setnames(w_dt, in_cols, c("ID", "outcome", "time", "cohort") )
    }
   
    
    # make a relative treatment variable 
    w_dt[, rel_treat := time - cohort]
  
    # create dummy variables 
    lapply(c("time", "rel_treat", "cohort"), AS_dummy_f, in_data = w_dt)
    
    
    # Check if control cohort exists. That will be observations with NA in the cohort variabe 
    n_control <- nrow(w_dt[is.na(cohort)])
    print(paste0(n_control, " control observations found"))
    
    
    # if there is no NA cohorts to use as control throw an error 
    if(n_control ==0 ){
      
      stop(paste0("No control cohort exists. Need observations with NA ", in_cohort_var, " that can be used as a control"))
    
    }
    
    # now we make relative treatment and cohort dummies 0 for all of the control observations
    # first grab the vars 
    rel_treat_dums <- grep("rel_treat_", colnames(w_dt), value = TRUE)
    cohort_dums <- grep("cohort_", colnames(w_dt), value = TRUE)
    
    # now make the change. 
    w_dt[is.na(cohort) , (rel_treat_dums) := 0]
    w_dt[is.na(cohort) , (cohort_dums) := 0]
    

  #=================#
  # ==== do SUR ====
  #=================#
    
    #note not sure how to do the group version here. Need to talk to Brian or Avi about this. 

    # get lhs vars. We need the dummy vars for each cohort
    lhs_list <- cohort_dums

    # get rhs vars. It's relative treatment variables, but leave on eout as control 
    rhs <- paste0(rel_treat_dums[-1],collapse = " + ")
    
    # make the three formulas 
    formula_list <- lapply(paste0(lhs_list, "~", rhs, "-1"), as.formula)
    
    # name the formula list based on the cohorts i,e, the LHS variable 
    names(formula_list) <- c(gsub("cohort_", "c",lhs_list))
    
    # run system fit 
    #note this is also not matching. Need to figure it out but I can not right now so mocing on
    #note that we exclue the control cohort from this 
    sur_res <- systemfit(formula_list, method = "SUR", data = w_dt[!is.na(cohort)]) # the estiamtes match but 
    Vcov <- vcov(sur_res) ##note  vcov matrix does not match. Not sure what is wronge, maybe robust SE 
    
  
  #========================#
  # ==== Estimate CATT ====
  #========================#
  # CATT is cohort average treatment effects 
  
    
    # create interaction variables for "saturated regression"
    # start by creating a concatinated variable of rel_treat and cohort 
    w_dt[, treat_coh := paste0(rel_treat, "_", cohort)]
  
    # now make dummies for the concatinated variable 
    AS_dummy_f( in_data = w_dt, "treat_coh")
    
    # Drop the NA_NA conrol group variable 
    w_dt[, treat_coh_NA_NA := NULL]
   
    # get all the combinations of relative treatment and cohort in the relevant data set 
    relevant_combos <- w_dt[!is.na(cohort), .N, c("rel_treat", "cohort") ]
    
    
    #==============================#
    # ==== find control cohort ====
    #==============================#

      # get a common "relatvie treatment" accross these groups to use as a control (-1 in AS setting)
      control_rel_treat <- relevant_combos[, list(rel_treat_n = .N), rel_treat]
      control_rel_treat <- control_rel_treat[ rel_treat_n == max(rel_treat_n), rel_treat]
      
      # iRemove anything grater than or equal to zero from the list of options.
      #  these are time periods with potentially hypothezised true effects. So not good for a control
      control_rel_treat <- control_rel_treat[control_rel_treat < 0]
      
      # if there is nothing left throw an error 
      if(length(control_rel_treat) == 0) stop( paste0("Your cohorts do not have any relative treatment variable",
                                                      " values in common that are also less than zero to use as a control.",
                                                      " I suggest trying fewer cohorts to get more overlap in relative treatment effects."))
      
      # Now either check for the one the user asked to use or just pick one 
      if(!is.null(omitted_relative_treatment)){
        
        # now grab the requested omitted_relative_treatment if it is in the list of options 
        control_rel_treat <- intersect(control_rel_treat, omitted_relative_treatment)
        
        # if it is not in the options throw an error 
        if(length(control_rel_treat) == 0){
          stop("the omitted relative variable you requested does not exist for every cohort. Pick another one that does or let the funciton find one for you.")
        }
        
      }else{
      
        # otherwise just pick one and print it out so they know 
        control_rel_treat <- sort(control_rel_treat, decreasing = TRUE)[[1]]
        
        print(paste0("relative treatment of ", control_rel_treat, " is excluded as a control"))
      }
      
      
      # remove it from the relevant combos 
      relevant_combos <- relevant_combos[rel_treat != control_rel_treat]
      
    #=========================#
    # ==== run regression ====
    #=========================#

          
      # # use this to put together RHS of regression 
      relevant_combos[, suffix := paste0(gsub("-", "m", rel_treat), "_", cohort)]
      RHS <- paste0("treat_coh_", relevant_combos$suffix)
      
      # add in time fixed effects
      time_FE <- paste0("time_", sort(unique(w_dt$time))[-1])
      RHS <- paste0(c(RHS, time_FE), collapse = " + ")
      
      # make formula 
      form <- as.formula(paste0("outcome ", " ~ ",
                                RHS,
                                "| ",
                                "ID",
                                "|0|",
                                "ID"))
    
      
      # run regression 
      if(weight_flag){
        reg_res <- felm(form,
                        data = w_dt,
                        cmethod = "reghdfe",
                        weights = w_dt$weight)
      }else{
        reg_res <- felm(form,
                        data = w_dt,
                        cmethod = "reghdfe")
      }

  
      
      reg_tab <- data.table( term          = rownames(reg_res$coefficients),
                                estimate   = as.numeric(reg_res$coefficients),
                                robust_ste = reg_res$cse,
                                t          = reg_res$ctval,
                                p_val      = reg_res$cpval)

  #=================#
  # ==== get IW ====
  #=================#
    
    # Generate counts for each cohort
    #note, this works because it is a balanced panal. NOt sure if this works if it isnt, need to think about it 
      
    # if opt_weight_iw is set, get total weights for each cohort 
    if(opt_weight_iw){
      
      # first get rid of diplicate ID's 
      c_list <- w_dt[, unique(ID), c("cohort", "weight")]
      
      c_list <- c_list[, list(count = sum(weight)), cohort]
      
    # IF NOT just get counts 
    }else{
      
      c_list <- w_dt[, list(count = uniqueN(ID)), cohort]
    }
    
    # Now we need to do linear combinations of the different estimate and adjust the standard errors 
    #note I suppose let's do this with a loop 

    # get relative treatment list to loop over 
    rel_treat_list <- relevant_combos[, unique(rel_treat)]
      
    # create a list for results 
    res_list <- vector("list", length = length(rel_treat_list))
  
    # start for loop 
    for(i in 1:length(rel_treat_list)){
      
      rel_treat_i <- rel_treat_list[[i]]
      
      # Figure out what cohorts have this rel_treat effect 
      rel_coh_i <- relevant_combos[rel_treat == rel_treat_i, cohort]
      
      # make the varibles list that we need 
      vars_i <- paste0("treat_coh_", gsub("-","m",rel_treat_i), "_", rel_coh_i)
      vcov_vars_i <- paste0("c", rel_coh_i, "_rel_treat_", gsub("-","m",rel_treat_i))
     
      # start by calculating the SE adjustment 
      if(length(rel_coh_i) > 1){
        tempb <- as.matrix(reg_tab[ term %chin% vars_i, estimate])
        tempVcov <- as.matrix(Vcov[vcov_vars_i, vcov_vars_i]  )
        temp <-  t(tempb)%*%tempVcov %*% tempb
      }else{
        temp <- 0
      }
      
      # now make list of linear combos and weights 
      weights <- c_list[cohort %in% rel_coh_i]
      weights[, weights := count/sum(count)]
      weights[, name :=  paste0("treat_coh_", gsub("-","m",rel_treat_i), "_", cohort )]
      weight_list <- weights$weights
      names(weight_list) <- weights$name
      
      # Now do linear combo 
      res <- as.data.frame(svycontrast(reg_res, weight_list))
      
      # put it in the result lists
      res_list[[i]] <- data.table(rel_treat = rel_treat_i, coef = res[,1], var = as.numeric(res[,2]^2 + temp))
  
      
    # end for loop 
    }
    
  #===========================#
  # ==== organize results ====
  #===========================#
  
    # stack results 
    IW_res <- rbindlist(res_list)
    
    # sort 
    setorder(IW_res, rel_treat)
    
    # add SE 
    IW_res[, sd := sqrt(var)]
    IW_res[, var := NULL]
    
    # clean up saturated regression table 
    reg_tab[term %like% "treat_coh_", suffix := gsub("treat_coh_", "",term)]
    reg_tab[, rel_treat := ea_scan(suffix, 1, "_")]
    reg_tab[, rel_treat := as.numeric(gsub("m", "-", rel_treat))]
    reg_tab[, cohort :=  as.numeric(ea_scan(suffix, 2, "_"))]
    reg_tab[, suffix := NULL]
    setcolorder(reg_tab, c("term", "cohort", "rel_treat"))
    setorder(reg_tab, cohort, rel_treat)
    
   
    # put it all in one big table
    # not sure what we want this to look like. have to think about it a bit 
    
    # put everything people could ever want it in a big ol list 
    # for now all I can thik of is the CATT estimates, the time FE from the CATT reg, and the IW estimates 
    out_list <- list() #note should initialize this with the length and names that we eventually decide to use 
    
    
    
    out_list[["CATT"]] <- reg_tab[]
    
    out_list[["IW"]] <- IW_res
    
    # return it 
    return(out_list)
  
# end funciton 
}





