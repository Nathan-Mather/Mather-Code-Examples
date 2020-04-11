#============================#
# ==== Do paper searches ====
#============================#

# - Keywords  
#  - #set will be used for things that need to be set for your specific
#       file structure to get the code to run. Things like data directories 
#  - #note I will use the tags #note for things I think are important

# - purpose of code: Take the top newspapers from each state and do a keyword serach on bing to generate a weight matrix 


#=================================#
# ==== laod packages and data ====
#=================================#


# clear objecrts and worksatation 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# set which computer you are using 
opt_nate_com <- TRUE

# decide how long you want to wait on average between searches. and range for random draws 
wait_total <- .5
wait_range <- .25

# set what rank papers to serach 
search_rank <- c(1:5)

# set year or years
# year_search <- c(2004:2016)
year_search <- c(2004:2017)

opt_load_xwalk <- TRUE

library(data.table)
library(easimple)
library(rvest)
library(httr)
library(RCurl) 

if(opt_nate_com){
  
  #set
  # load top paper xwalk 
  paper_xwalk <- fread("c:/Users/Nmath_000/Documents/Research/FIscal Policy Interdependence/data/int/top_state_papers_checked/most_recent/top_state_papers_checked.csv")
  
  # load state xwalk 
  state_xwalk <- fread("C:/Users/Nmath_000/Documents/Research/FIscal Policy Interdependence/xwalks/state_names_google.csv")
  
  # path to existing search xwwalk 
  search_xwalk_path <- "C:/Users/Nmath_000/Documents/Research/FIscal Policy Interdependence/data/weight_matrices/bing_searches/most_recent/bing_paper_searches.csv"
  
  # out path for xwalk of results 
  out_path <- "c:/Users/Nmath_000/Documents/Research/FIscal Policy Interdependence/data/weight_matrices/bing_searches/"
  
  # load special search xwlk 
  special_search <- fread("c:/Users/Nmath_000/Documents/Research/FIscal Policy Interdependence/xwalks/special_search_bing.csv")
  
  # if using faiths comp 
}else{
  
  
  # #set
  # # load top paper xwalk 
  # paper_xwalk <- fread("C:/Users/Faith/Documents/Nate/Fiscal Policy Interdependence/top_state_papers_12_29.csv")
  # 
  # # load state xwalk 
  # state_xwalk <- fread("C:/Users/Faith/Documents/Nate/Fiscal Policy Interdependence/state_names_google.csv")
  # 
  # # path to existing search xwalk 
  # search_xwalk_path <- "C:/Users/Faith/Documents/Nate/Fiscal Policy Interdependence/google_searches/most_recent/google_paper_searches.csv"
  # 
  # # path out for results 
  # out_path <- "C:/Users/Faith/Documents/Nate/Fiscal Policy Interdependence/google_searches/"
  
}

#set example url
# im not gonna try to recreate this from scratch. Go do a search of the terms and www.montgomeryadvertiser.com as an example site. We can edit that portion out to get the searches we need 
example_url_l <- list()
example_url_sub_l <- list()
example_url_l[["2016"]] <- "https://www.bing.com/search?q=site%3awww.montgomeryadvertiser.com+(government+%7c+%22government+spending%22+%7c+%22public+policy%22+%7c+legislature+%7c+%22fiscal+policy%22+%7c+spending)+&filters=ex1%3a%22ez5_16801_17166%22&qs=n&sp=-1&pq=site%3awww.montgomeryadvertiser.com+(government+%7c+%22government+spending%22+%7c+%22public+policy%22+%7c+legislature+%7c+%22fiscal+policy%22+%7c+spending+%7c+test+%7c+other)+&sc=0-147&cvid=EA26EFA0EC56440E8B6BDA349F9D9291&qpvt=site%3awww.montgomeryadvertiser.com+(government+%7c+%22government+spending%22+%7c+%22public+policy%22+%7c+legislature+%7c+%22fiscal+policy%22+%7c+spending)+"
example_url_sub_l[["2016"]] <- "https://www.bing.com/search?q=site%3awww.montgomeryadvertiser.com+(government+%7c+%22government+spending%22+%7c+%22public+policy%22+%7c+legislature+%7c+%22fiscal+policy%22+%7c+spending)+%26+wisconsin+&filters=ex1%3a%22ez5_16801_17166%22&qs=n&sp=-1&pq=site%3awww.montgomeryadvertiser.com+(government+%7c+%22government+spending%22+%7c+%22public+policy%22+%7c+legislature+%7c+%22fiscal+policy%22+%7c+spending)+&sc=0-132&cvid=3A3E1337AD4B45C48E8E3A9DB2C28320&qpvt=site%3awww.montgomeryadvertiser.com+(government+%7c+%22government+spending%22+%7c+%22public+policy%22+%7c+legislature+%7c+%22fiscal+policy%22+%7c+spending)+%26+wisconsin+"
  
# format news links for how we want to do it 
paper_xwalk[, new_website2 := gsub("https\\:\\/\\/", "", new_website)]
paper_xwalk[, new_website2 := gsub("http\\:\\/\\/", "", new_website2)]

# time stamp 
time_stamp <- ea_timestamp()



#==========================#
# ==== make/load xwalk ====
#==========================#
  
  # make search xwalk 
  search_xwalk <- paper_xwalk[, c("newspaper", "state", "new_website2", "n_twit_fol", "twit_rank2", "trend_scaled", "trend_rank", "year"), with = FALSE]
  search_xwalk[, merge := 1]
  
  
  state_list <- state_xwalk[, "state", with = FALSE]
  state_list[, merge := 1]
  
  
  setnames(state_list, c("state"), c("search_state"))
  search_xwalk <- merge(search_xwalk, state_list, by = "merge", allow.cartesian = TRUE)
  
  search_xwalk[, merge := NULL]
  
  # if we are load it, to that 
  #note #dix need to do this part still once I have one started 
  if(opt_load_xwalk){
    
    # load up all the results we already have
    search_xwalk_done <- fread(search_xwalk_path)

    # merg on results. this way if we want to add more papers in the preivous scrtipt to paper_xwalk
    # it will link up seemlessly
    # also dont take rank or trend from the done results. This way we can update the google trends
    # stuff and not have to redo identical searches
    search_xwalk_done <- search_xwalk_done[, -c("n_twit_fol", "twit_rank2", "trend_scaled", "trend_rank")]
    merge_by <- intersect(colnames(search_xwalk), colnames(search_xwalk_done))
    search_xwalk <- merge(search_xwalk, search_xwalk_done, by = merge_by, all.x = TRUE)

    # dont forget to check out this merge
    print("dont forget to check out this merge")
    
  }else{
    search_xwalk[, results := as.character(NA)]
    search_xwalk[, url_used := as.character(NA)]
    c[, flag_zero := as.character(NA)]
  }

#===========================#
# ==== perform searches ====
#===========================#
  
  # subset paper xwalk to those in the rank we are looking for 
  #note #fix doing this with twitter rank for now 
  paper_xwalk <- paper_xwalk[twit_rank2 %in% search_rank, ]
  
  # subset paper xwalk to the years we are going to search 
  paper_xwalk <- paper_xwalk[year %in% year_search, ]
  
  # now sort the xwalk. The reason I am doing this is so that 
  # the searcher will complete seraches in the most usefull order 
  # That is we start with onlt one rank and complete entire years at a time.
  # that way get first get a workable year, then a workable time series, then we beef it up with less popular 
  # papers 
  setorder(paper_xwalk, twit_rank2, -year, state)


  
  # loop over xwalk of paper websites
  # i <- 32
  for(i in 1:nrow(paper_xwalk)){
    
    # keep track of year 
    year_i <- paper_xwalk[i, year]
    
    # keep track of newpaper 
    newspaper_i <- paper_xwalk[i, newspaper]
    
    # keep track of state 
    state_i <- paper_xwalk[i, state]
    
    # print out where we are at
    print(paste0("starting on paper ", newspaper_i, " from ", state_i, " in ", year_i))
    
    # get site we need 
    site_i <- paper_xwalk[i, new_website2]
    print(site_i)
    
    # see which states still need filling out 
    to_do <- search_xwalk[newspaper == newspaper_i & year == year_i & is.na(results)]
    
    # if none, skip this paper 
    if(nrow(to_do) == 0) next()
    
    
    # check if the state it is in needs to be done 
    if(state_i %chin% to_do$search_state){
      # if so do the search 
      
      # get relevent url 
      emample_url_i <- example_url_l[[as.character(year_i)]]
      
      # first change site 
      url_i <- gsub("www\\.montgomeryadvertiser\\.com", site_i, emample_url_i)
      
      # check if that is in the link. If not error because there is an issue 
      if(!grepl("www\\.montgomeryadvertiser\\.com", emample_url_i)) stop("URL does not use www.montgomeryadvertiser.com")
      
      # go to page 
      page_i <- read_html(url_i)
      
      # wait for a while
      wait_l <- (wait_total- wait_range)
      wait_u <- (wait_total + wait_range)
      wait_time <- runif(1, wait_l,wait_u)
      Sys.sleep(wait_time)

      # set results xpath 
      results_xp <- '//*[@id="b_tween"]/span[1]'
      
      # get results node 
      results_node <- html_nodes(page_i, xpath=results_xp)
      
      # get results text 
      results_i <- html_text(results_node, trim = TRUE)
      
      # check if results are zero #Note #fix 
      flag_zero <- FALSE
      if(length(results_i)==0){
        flag_zero <- TRUE
        results_i <- 0
      }else{
        if(!grepl("Results|Result|result|results", results_i)){
          flag_zero <- TRUE
        }
      }
      
      # put it in the xwalk
      search_xwalk[newspaper == newspaper_i & state == search_state & year == year_i, results := results_i]
      
      # put url used in xwalk 
      search_xwalk[newspaper == newspaper_i & state == search_state & year == year_i, url_used := url_i]
      

      #if we are getting zero results here than mark that this one has an issue and move on. 
      # also fill in all the subseraches has having an issue as well so we don't waist time searching there.
      if(flag_zero){
        
        # mark it as an issue 
        search_xwalk[ newspaper == newspaper_i & state == state_i & year == year_i, flag_zero := TRUE]
        
        # Now skip to the next loop because we don't want to spend time searching any of these
        next()
       
      }# close if flag zero 
        
      
    }# close serach for overal state 
    
    # Now loop over all states that still need to be done 
    # j <- 1
    for(j in 1:nrow(to_do)){
      
      # grab state 
      state_j <- to_do[j, search_state]
      print(state_j)
      
      # if it's the same staet as above skip it 
      if(state_i == state_j) next()
      
      # grab search term and make url 
      ## start if there are no special requirments and just using state name 
      if(nrow(special_search[state == state_j & year == year_i,]) ==0){
        
        # grab stat name 
        state_search_j <- gsub(" ", "%20", state_j)
        
        # make URL
        emample_url_sub_i <- example_url_sub_l[[as.character(year_i)]]
        
        url_ij <- gsub("www\\.montgomeryadvertiser\\.com", site_i, emample_url_sub_i)
        # check if that is in the link. If not error because there is an issue 
        if(!grepl("www\\.montgomeryadvertiser\\.com", emample_url_sub_i)) stop("URL does not use www.montgomeryadvertiser.com")
        
        url_ij <- gsub("wisconsin", state_search_j, url_ij )

        # if this is a case with special search terms #note #fix 
      }else{
        
        # grab starter URL with terms needed
        url_base <- special_search[state == state_j & year == year_i, special_url]

        # now just replace the site 
        url_ij <- gsub("www\\.montgomeryadvertiser\\.com", site_i, url_base)
        
        # check if that is in the link. If not error because there is an issue 
        if(!grepl("www\\.montgomeryadvertiser\\.com", url_base)) stop("URL does not use www.montgomeryadvertiser.com")

        
      }
      
     
      # go to page 
      page_i <- read_html(url_ij)
      
      # wait for a while
      wait_l <- (wait_total- wait_range)
      wait_u <- (wait_total + wait_range)
      wait_time <- runif(1, wait_l,wait_u)
      Sys.sleep(wait_time)
      
      # set results xpath 
      results_xp <- '//*[@id="b_tween"]/span[1]'
      
      # get results node 
      results_node <- html_nodes(page_i, xpath=results_xp)
      
      # get results text 
      results_ij <- html_text(results_node, trim = TRUE)
      
      # check if results are zero #Note #fix 
      flag_zero <- FALSE
      if(length(results_ij)==0){
        flag_zero <- TRUE
        results_ij <- 0
      }else{
        if(!grepl("Results|Result|result|results", results_ij)){
          flag_zero <- TRUE
        }
      }
      # put it in the xwalk
      search_xwalk[newspaper == newspaper_i & search_state == state_j & year == year_i, results := results_ij]
      
      # put url used in xwalk 
      search_xwalk[newspaper == newspaper_i & search_state == state_j & year == year_i, url_used := url_ij]
      
      
      #if we are getting zero results here than mark that this one has an issue and move on. 
      # also fill in all the subseraches has having an issue as well so we don't waist time searching there.
      if(flag_zero){
        
        # mark it as an issue 
        search_xwalk[newspaper == newspaper_i & search_state == state_j & year == year_i, flag_zero := TRUE]
        
        
      }
      
      # close loop over states 
    }
    
    # save the xwalk in case the system crashes or something. Don't want to lose the progress we have. 
    ea_save(search_xwalk, 
            in_val_path = out_path,
            in_val_file = "bing_paper_searches.csv",
            in_val_timestamp = time_stamp)
    
    # close for loop over paper websites 
  }
  

