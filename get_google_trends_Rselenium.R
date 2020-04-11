#========================#
# ==== Google trends ====
#========================#

# - Keywords  
#  - #set will be used for things that need to be set for your specific
#       file structure to get the code to run. Things like data directories 
#  - #fix will be used for things that need to be fixed 
#  - #note I will use the tags #note for things I think are important

# - purpose of code:
# take the newspaper results we got from the website and run these 
# newspapers through google trends. 
# this initial run uses one reference group since I have no idea about thier size 
# this will give us an initial look at which newspapers are big and small and allow me to do a 
# more nuanced search with better reference groups that will avoid too much truncation 
#




#=============================#
# ==== load packages/data ====
#=============================#


  rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  options(scipen = 999)
  cat("\f")
  
  
  library(RSelenium)
  library(data.table)
  library(easimple)
  library(rvest)
  library(httr)
  
  # srouce wait to laod functions 
  source("C:/Users/Nmath_000/Documents/Code/fiscal_policy_interdependence/functions/wait_to_load.R")
  source("C:/Users/Nmath_000/Documents/Code/fiscal_policy_interdependence/functions/PaRselenium.R")

  # get inital address for google trends 
  trernds_url <- "https://trends.google.com/trends/explore?geo=US"
  
  # load newspaper data 
  # np_dt <- fread("C:/Users/Nmath_000/Documents/Research/FIscal Policy Interdependence/data/weight_matrices/Newspaper_list/most_recent/50states_newspapers.csv")
  np_dt <- fread("C:/Users/Nmath_000/Documents/Research/FIscal Policy Interdependence/data/weight_matrices/Newspaper_twitter_sub/most_recent/usnpl_twitter_newspapers.csv")
  
#====================#
# ==== SET PARMS ====
#====================#

  # set downloads directory 
  dl_dir <- 'c:/Users/Nmath_000/Downloads/'
  
  opt_clear_download <- FALSE
  
  #====================#
  # ==== IMPORTANT ====
  #====================#
  
  # MAKE SURE your downloads is cleared before you start doing this as it relies and moving files with a specific name 
  # when I have time I will come back and write something to make sure all the downloads by that name are removed prior 
  # to starting the code 
  
  if(opt_clear_download){
    # delete everything in downloads that matches the format of the fiels I will download here 
    dl_files <- list.files(dl_dir)
    to_rem <- grep("multiTimeline|geoMap", dl_files, value = TRUE)
    to_rem <- paste0(dl_dir, to_rem)
    lapply(to_rem,  file.remove)
  }
  
#============================#
# ==== set up R selenium ====
#============================#

  
  # in the future I want to open multiple links to speed this up 
  # I could run each state in a seperate window 
  # get the connection 
  binman::list_versions("chromedriver")
  
  # open up browser 
  # port <- as.integer(runif(1, 1000, 9999))
  port <- 4444L
  driver <- rsDriver(browser=c("chrome"), port = port, chromever = "78.0.3904.70")
  rd <- driver[["client"]]

  
  # cprof <- getChromeProfile('C:/Users/NMATH_~1/AppData/Local/Temp/scoped_dir8612_650503463/Default', "Profile 1")
  
  # rd <-  remoteDriver(browserName = "chrome", extraCapabilities = cprof)
  
  rd$open()
  
  # select implicit wait time 
  rd$setTimeout( milliseconds = 1000000)
  

  
  # get to google trends website 
  #note website is giving me some issues loading. May need to manually check that this loads or 
  # write a wiat to load type thing that keeps trying untill it gets there. 
  rd$navigate("https://Facebook.com")
  rd$navigate(trernds_url)
  
  
#===================#
# ==== set date ====
#===================#
# 
#   # What date do you want to search in? 
#   start_d <- "2012-01-01"
#   end_d   <- "2013-01-01"
  # 
  # # make the relevent section of the url 
  # date_url <- paste0(start_d, "%20", end_d)
  # 
  date_url <- "all"

  
  
#====================#
# ==== Do trends ====
#====================#

  # now pick first paper as a reference paper. I will use "The New York Times"  
  ref <- 'Milwaukee Journal Sentinel'
  
  # now grab the rest of the papers 
  papers <- np_dt$newspaper
  
  # bust this into groups of four or less 
  # first figure out how many goups we need 
  n_g <-  ceiling(length(papers)/4)

  
  
  
#============================#
# ==== Get initial trend ====
#============================#
  # here I will use a signle reference group to get an initial read on the popularity of different papers
  # this isn't good enough because the trends data rounds to 1 and give <1 for very small relative seraches. 
  # this initial search will allow me to rank the paper in order of size and then do another search with more targetd 
  # reference groups 
  
  # create folder for timeline data 
  # irst creat a dated folder to put it in 
  int_mt_dir <- paste0("c:/Users/Nmath_000/Documents/MI_school/Research/FIscal Policy Interdependence/data/int/mt_", Sys.Date())
  int_geo_dir <- paste0("c:/Users/Nmath_000/Documents/MI_school/Research/FIscal Policy Interdependence/data/int/geoMap_", Sys.Date())
  dir.create(int_mt_dir)
  dir.create(int_geo_dir)
  
  
  # create gruop numbers to run 
  all_groups <- 1:n_g
  

  # check how many files are already in there if this has been run already 
  alread_done <- list.files(int_mt_dir)
  
  splitter_f <- function( in_var_split, in_val_position, in_val_delim){
    
    out <- strsplit(in_var_split, in_val_delim)
    
    out <- out[[1]][[in_val_position]]
    
    return(out)
  }
  
  alread_done <- lapply( alread_done, splitter_f,1, "_")
  alread_done <- as.numeric(unlist(alread_done))
  
  to_do <- setdiff(all_groups, alread_done)
  
  # start list for ones that don't download 
  did_not_load_list <- c()
  
  # loop over groups 
  start_time <- Sys.time()
  i <- to_do[[1]]
  for(i in to_do){
    
    # set this to False until it gets tripped 
    did_not_load_i <- FALSE
    
    # remove elements for check 
    print(i)
    up <- i*4
    low <- up-3
    
    
    # get group i of papers
    papers_i <- c(ref, papers[low:up])
    
    # replace & with and because it causes issues 
    papers_i <- gsub("&", "and", papers_i )
    
    # put papers into hyperlink format 
    papers_i_url <-  paste0(gsub(" ", "%20", papers_i), collapse = ",")
    
    # put together URL for the desired search 
    search_i_url <- paste0('https://trends.google.com/trends/explore?date=', 
                           date_url,
                           '&geo=US&q=',
                           papers_i_url)
    # go to that webpage 
    # rd$navigate(search_i_url)
     par_navigate_persist ( in_URL         = search_i_url,
                            remote_driver = rd,
                            xpath_error   = c("/html/body/div[2]/div[2]/div/error/div/div/div[1]", '//*[@id="af-error-container"]/p[1]/ins'),
                            xpath_page    = '/html/body/div[2]/div[2]/div/md-content/div/div/div[1]/trends-widget/ng-include/widget/div/div/div/widget-actions/div/button[1]')
    
    
    
    # Wait a bit of time and let it load 
     stop_time <- runif(1, 5, 10)
     Sys.sleep(stop_time)
    
     
    # it_el <- wait_to_find_el(in_xpath = '/html/body/div[2]/div[2]/div/md-content/div/div/div[1]/trends-widget/ng-include/widget/div/div/div/widget-actions/div/button[1]', remote_driver = rd)
    it_el <- wait_to_find_el(in_using = "css selector",
                             in_xpath = "body > div.trends-wrapper > div:nth-child(2) > div > md-content > div > div > div:nth-child(1) > trends-widget > ng-include > widget > div > div > div > widget-actions > div > button.widget-actions-item.export",
                             remote_driver = rd)
     
    # click button to download "interest over time" data 
    it_el$clickElement()
    
    
    # click button to downlaod all the state data other than te reference gourp
    el <- wait_to_find_el( '/html/body/div[2]/div[2]/div/md-content/div/div/div[7]/trends-widget/ng-include/widget/div/div/div/widget-actions/div/button[1]', remote_driver = rd)
    el$clickElement()

    Sys.sleep(1)
    el <- wait_to_find_el( '/html/body/div[2]/div[2]/div/md-content/div/div/div[10]/trends-widget/ng-include/widget/div/div/div/widget-actions/div/button[1]', remote_driver = rd)
    el$clickElement()
    Sys.sleep(1)

    el <- wait_to_find_el( '/html/body/div[2]/div[2]/div/md-content/div/div/div[13]/trends-widget/ng-include/widget/div/div/div/widget-actions/div/button[1]', remote_driver = rd)
    el$clickElement()
    Sys.sleep(1)

    el <- wait_to_find_el( '/html/body/div[2]/div[2]/div/md-content/div/div/div[16]/trends-widget/ng-include/widget/div/div/div/widget-actions/div/button[1]', remote_driver = rd)
    el$clickElement()
    


  
    #note should ptobably add something that, if this hasn't gone in like 10 seconds, give it another click 
    stop_time <- runif(1, 5, 25)
    Sys.sleep(stop_time)
    
    # note I'm not checking the geo tags because not all of them have it 
    # will make sure I get that later 
    wait <- TRUE
    start_wait <- Sys.time()
    attempts <- 0
    while(wait){
      
      # get files in downloads 
      dl_files <- list.files(dl_dir)
      
      # check how many are in each type 
      mt_dls <- length(grep("multiTimeline.csv", dl_files, value = TRUE))
      # geo_dls <- length(grep("geoMap", dl_files, value = TRUE))
      
      # check that the latest ones we want are there 3
      check_1 <- mt_dls == 1
      # check_2 <- geo_dls == i*4
      
      # if they are, set wait to FALSE, if not keep waiting
      if(check_1 ){
        wait <- FALSE
      }else{
        time <- Sys.time() - start_wait
        # if its been really long break it 
        if(time > 10){
          # give the button another click 
          it_el <- wait_to_find_el(in_xpath = '/html/body/div[2]/div[2]/div/md-content/div/div/div[1]/trends-widget/ng-include/widget/div/div/div/widget-actions/div/button[1]', remote_driver = rd)
          
          it_el$clickElement()
          start_wait <- Sys.time()
          attempts <- attempts + 1
        }
        if(attempts > 10){
          
          did_not_load_i <- TRUE
          wait <- FALSE
          
          
        }
      }
      
    }
    
    # if it didn't downlaod add it to the list 
    if(did_not_load_i){
      did_not_load_list <- c(did_not_load_list, i)
    }
    
    # if it did load move the file 
    if(!did_not_load_i){
      # now move the multi-time file to another folder 
      dl_files <- list.files(dl_dir)
      mt_file <- grep("multiTimeline.csv", dl_files, value = TRUE)
  
      # then copy all this in there 
      mt_to <- paste0(int_mt_dir, "/",i,"_", mt_file)
      mt_from <- paste0(dl_dir, mt_file)
      file.rename(from = mt_from, to = mt_to)
    }
  }

  end_time <- Sys.time()
  end_time - start_time
  
 
  #===========================#
  # ==== move all of this ====
  #===========================#

  dl_files <- list.files(dl_dir)
  geo_files <- grep("geoMap", dl_files, value = TRUE)

  # then copy all this in there
  geo_to <- paste0(int_geo_dir, "/", geo_files)
  geo_from <- paste0(dl_dir, geo_files)
  mapply(file.copy, from = geo_from, to = geo_to)


  #===============================================#
  # ==== oops check for duplicate paper names ====
  #===============================================#

    np_dt[, paper_count := .N, newspaper]
  
  dup_newspaper <- np_dt[ paper_count > 1]
  setorder(dup_newspaper, "newspaper")


  #===========================#
  # ==== close connection ====
  #===========================#
  driver[["server"]]$stop()


