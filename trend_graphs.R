#===========================#
# ==== opiods trends ====
#===========================#

# This will match the trends do file 

#=========================#
# ==== set parameters ====
#=========================#
  
  # ls() gets everything in the glogal enviorment. Then we remove all of that from the global enviorment 
  rm(list = ls())
  
  # this sends ctrl+L to the Console which clears the console 
  cat("\f")
  
  # load packages
  library(tidyverse) 
  library(readxl)  
  library(haven) # use this to load in dta files 
  library(ggplot2)
  library(ggforce)
  
  # check what users are in the users folder. 
  # List.files creates a list of all files ir folders in the file path specified 
  users_list <- list.files("C:/Users/")
  
  # check which of us shows up in the Users folder 
  # Grep searches user_list for either of our user names. Any objects that match are returned. Should always just be me or you 
  # we can use the or | within the character string here because it is evaluated as a regular expression 
  user <- grep("bajacob|Nmath_000", users_list, value = TRUE)
  
  #set the base path for the opiods folder
  # Paste the character strings together with the user to get the full path 
  # the regular paste command leaves a space between the character strings, so we want paste0
  # also note that in R file paths need forward slashes 
  opiods_fold <- paste0("c:/Users/", user, "/Box Sync/Opioids")
  
  # set the path for other folders 
  raw_naep <- paste0(opiods_fold, "/raw_data/naep/")
  raw_cov <- paste0(opiods_fold, "/raw_data/covariates/")
  raw_op  <- paste0(opiods_fold, "/raw_data/opioids/")
  raw_ex  <- paste0(opiods_fold, "/raw_data/expenditures/")
  
  fin <- paste0("c:/Users/", user, "/Box Sync/Opioids/final_data/")
  res <- paste0( "c:/Users/", user, "/Box Sync/Opioids/results/R_figs/")


#======================#
# ==== set up data ====
#======================#

  # load in stata file since the R file still has the excel reading issue 
  op_dt <- read_dta(paste0(fin, "opioids.dta"))

  
  # do some changes brian has 
  op_dt <- op_dt %>% 
    
    # group data by state year for next operations 
    group_by(state, year) %>% 
    
    # create new variables and remember it is grouped 
    mutate(avg_all = mean(std_avg)) %>% 
    mutate(p10_all = mean(std_p10)) %>% 
    mutate(std_gapall = mean(std_gap9010)) %>% 
    
    # don't forget to uncgroup to avoide mistakes later on 
    ungroup()

  # rename some stuff 
  op_dt <- op_dt %>% 
    rename(single = singleparent) %>% 
    rename(immig = immigrant) %>% 
    rename( births = outwedlock) %>% 
    rename(noins = no_ins) %>% 
    rename(prek = pre35lag5)
  
  # get national expenditures per pupil by summing up state expenditures 
  # first get unique state year observations of rtotppe  
  sy_rtotppe <- unique(select(op_dt, state, year, rtotppe))
  
  # confirm that that there is only one rtotppe per state year by looking for dupplicate now 
  # by just state year 
  sy_rtotppe %>%
    select( state, year) %>% 
    
    # note that the . takes the data from the pipe 
    filter(duplicated(.))
  
  # all good. Now sum states by year. First drop national from the data. 
  nat_rtotppe <- filter(sy_rtotppe, state != "National") %>% 
    group_by( year) %>% 
    summarise(rtoppe_nat = mean(rtotppe)) %>% 
    ungroup()
  
  # now att national onto the data to merge back onto full set 
  nat_rtotppe <- mutate(nat_rtotppe, state = "National")
  
  # do the merge 
  op_dt <- left_join(op_dt, nat_rtotppe, by = c("state", "year")) %>% 
    
    # fill in rtoppe at national level with new value we calculated 
    # Here we have to use ifelse rather than replace because we want to replace it with elements of anoter vector 
    # only when that vector also satisfies the condition. 
    mutate( rtotppe = ifelse(state == "National", rtoppe_nat, rtotppe)) %>% 
    select(-rtoppe_nat)
  

#==========================#
# ==== Figure  example ====
#==========================#
# HIGHLIGHT AND HIT CTRL + SHIFT + C TO UNCOMMENT! 
  
# # this was just for me to more easily play aruond with graph options and get the look I want 
# # could delete it but I figured it would be helpful for anyone reading this to go through and 
# # more easily see each step. 
#   
#   # will do a loop of a function, for now just get an example 
#   grade_i <- 8
#   subject_i <- 'Reading'
#   
#   # subset data 
#   plot_dt <- filter(op_dt, grade == grade_i & subject == subject_i & state=="National" & year>1990 ) %>% 
#     
#     # select columns we actually need 
#     select(year, std_avg, std_p10)
#   
#   
#   # we could do this all in one go put I am splitting it up so we can see each step 
#   
#   
#   # you start by initializing a plot. In here we can include our data and an aes or aesthetic. The aes is used to 
#   # designate the variables as well as various things like color or line type.. We will have multple y variables and 
#   # colors so don't want to define tthose here 
#   plot_ex <- ggplot(data = plot_dt, aes(x = year))
#   
#   # add a basic theme. we will edit specifics 
#   plot_ex <- plot_ex + theme_classic()
#   
#   # not we add on our first line. Now we define an aes for this specific line. 
#   plot_ex <- plot_ex + geom_line(aes(y = std_avg, color = "Average Score (std)"))
#   
#   # Now we add the second line. 
#   # hadly (author of ggplot) doesn't like two axis plots so this is a pit of a hack. but we 
#   # transform the std_p10 data so it is on the same scale as the avg_score data 
#   plot_ex <- plot_ex + geom_line(aes(y = (std_p10 +1.4)/2, colour = "10th Percentile (std)"))
#   
#   
#   # see where we are at
#   plot_ex 
#   
#   #  Now add the secondary axis 
#   # and, very important, reverting the above transformation on std_p10
#   plot_ex <- plot_ex + scale_y_continuous(sec.axis = sec_axis(~.*2 -1.4, name = "10th Percentile (std)"))
# 
#   # modifying colours
#   plot_ex <- plot_ex + scale_colour_manual(values = c("red", "blue"))
#   
#   # lets change axis colots to match 
#   plot_ex <- plot_ex + theme( axis.line.y.right = element_line(color = "red"), 
#                               axis.ticks.y.right = element_line(color = "red"),
#                               axis.title.y.right = element_text(colour = "red"),
#                               axis.text.y.right  = element_text(color = "red"),
#                               axis.line.y.left= element_line(color = "blue"), 
#                               axis.ticks.y.left = element_line(color = "blue"),
#                               axis.title.y.left = element_text(colour = "blue"),
#                               axis.text.y.left  = element_text(color = "blue")
#                               )
#   
#   
#   
#   # modeify labels 
#   plot_ex <- plot_ex + labs(y = "Average Score (std)",
#                 x = "Year",
#                 colour = "Parameter")
#   
#   # modify theme position 
#   plot_ex <- plot_ex + theme(legend.position = "bottom")
#   
#   # put in verticle line 
#   plot_ex <- plot_ex + geom_vline(xintercept  = 2012.9)
# 
#   # put in more x axis labels 
#   # use last two digits of year 
#   plot_ex <- plot_ex + scale_x_continuous(breaks = unique(plot_dt$year),
#                                            labels = substr(uniqe(plot_dt$year), 3,4))
#   
#   # rotate x axis labels 
#   # plot_ex <- plot_ex + theme(axis.text.x=element_text(angle=-70, hjust=-.5))
#   
#   # adjust position of y axis lables plot_ex
#    # plot_ex2 <- plot_ex + theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))
#    
#   plot_ex <- plot_ex + theme(axis.text.y = element_text( margin = margin(t = 0, r = 5, b = 0, l = 8)))
#   plot_ex <- plot_ex + theme(axis.text.y.right = element_text( margin = margin(t = 0, r = 10, b = 0, l = 8)))
#    
#   # add a title 
#   plot_ex <- plot_ex + ggtitle("NAEP Trends: Reading Grade 8")
#   
#   # adjust size of title 
#   plot_ex <- plot_ex + theme(plot.title = element_text(hjust = 0.5, size = 20),
#                              legend.title = element_blank())
#   
#   # adjust text size 
#   plot_ex <- plot_ex + theme( text = element_text(size=15))
#   
#   # check out final version 
#   plot_ex
#  
#   # set size of plot to save 
#   # note if you dont do this ggsave uses the aspect ratio in the plot window which
#   # is silly 
#   aspect_ratio <- 5/3
# 
# 
#   # save it to send a sample to brian 
#   ggsave("c:/Users/Nmath_000/Box Sync/Opioids/results/R_figs/sample_fig.png",
#          plot = plot_ex,
#          height = 5 , 
#          width = 5 * aspect_ratio)
#   
#   

  
#=============================================#
# ==== Write function for two axis graphs ====
#=============================================#

  # # define variables for debug 
  #   plot_dt <- filter(op_dt, grade == "8" & subject == "Reading" & state=="National" & year>1990 ) %>%
  # 
  #     # select columns we actually need
  #     select(year, std_avg, std_p10)
  # in_data <- plot_dt
  # plot_title <- "NAEP Trends: Reading Grade 8"
  # x_var <- "year" 
  # x_lab  <- "Year"
  # y1_var <- "std_avg"
  # y1_lab <- "Average Score (std)"
  # y2_var <- "std_p10"
  # y2_lab <- "10th Percentile (std)"
  # legend_position = "bottom"
  # write this as a function so it is easier to loop over for the different variables
  two_axis_ploter <- function(in_data,
                              plot_title,
                              x_var,
                              x_lab,
                              y1_var,
                              y1_lab, 
                              y2_var,
                              y2_lab,
                              legend_position = "bottom"){
    
    # everthing is going to be the same as the above example just combinined into one call with the "+" 
    # and substitute out function inputs for hard codes above 
    
    # first we need to figure out the transformation to get the proper second axis 
    max1 <- max(in_data[[y1_var]], na.rm = TRUE)
    min1 <- min(in_data[[y1_var]], na.rm = TRUE)
    max2 <- max(in_data[[y2_var]], na.rm = TRUE)
    min2 <- min(in_data[[y2_var]], na.rm = TRUE)
    
   
    # Now get range transformation. need range of y2 to match y1 
    y2_trans <- (max1 - min1)/(max2-min2)
    
    # and shift so transformed max2 = max 1
    y2_shift <- max1-max2*y2_trans
    
    
    # now get year range 
    max_x <-  max(in_data[[x_var]])
    min_x <-  min(in_data[[x_var]])
    
    # now get non NA data for left and right Y axis. We don't want to cut up the trend lines just because we dont have data 
    # for every year 
    y1_data <- filter(in_data, !is.na(get(y1_var)))
    y2_data <- filter(in_data, !is.na(get(y2_var)))
    
    # get a color vector. The order of the variables switches based on alphabet 
    # so we need to set the order of the colors accordign to what ggplot has them in
    # we also want to switch the order in one case so that the legends are all the same 
    if(y1_lab > y2_lab){
      colors_v <- c("red", "blue")
      switch_leg <- TRUE
    }else{
      colors_v <-  c( "blue", "red")
      switch_leg <- FALSE
    }
    
    # note we have to replace year with get(x_var) to get the string "year" 
    out_plot <- ggplot(data = in_data, aes(x = get(x_var))) +
      
      theme_classic() +
    
      # note the get 
      geom_line(data = y1_data, aes(y = get(y1_var), color = y1_lab)) +
  
      # add second line with transformation we found above 
      geom_line(data = y2_data, aes(y = get(y2_var)*y2_trans + y2_shift, colour = y2_lab)) +
      
      # add second axis. adjust with above transofmarions and use inpt names 
      scale_y_continuous(breaks =function(x) pretty(x, n = 6),
                          expand = expand_scale(.25),
                          sec.axis = sec_axis(~(. -y2_shift)/y2_trans, name = y2_lab,
                                             breaks =function(x) pretty(x, n = 6))) +
      
      # add title 
      ggtitle(plot_title) + 

      # change colors of lines and switch order of legend if need be 
      scale_colour_manual(values = colors_v,
                          guide = guide_legend(reverse = switch_leg)) +
      
      # modify labes 
      labs(y = y1_lab,
           x = x_lab,
           colour = "Parameter") + 
      
      # put in verticle line 
      geom_vline(xintercept  = 2012.9) + 
      
      # put in more x axis labels 
      scale_x_continuous( breaks = unique(in_data[[x_var]]),
                          labels = paste0("'", substr(unique(in_data[[x_var]]), 3,4))) +
      
      #  do all the theme stuff 
      #note legend_position is an entry into the funtion. I think it looks good inthe graph but it just 
      # doesn't fit that way on all of them so make it adjustable 
      theme(axis.line.y.right  = element_line(color = "red"), # adjust colors of axis 
            axis.ticks.y.right = element_line(color = "red"),
            axis.title.y.right = element_text(color = "red"),
            axis.text.y.right  = element_text(color = "red",
                                              margin = margin(t = 0, r = 10, b = 0, l = 8)), # adjust space on right y axis 
            axis.line.y.left   = element_line(color = "blue"), 
            axis.ticks.y.left  = element_line(color = "blue"),
            axis.title.y.left  = element_text(color = "blue"),
            axis.text.y.left   = element_text(color = "blue",
                                              margin = margin(t = 0, r = 5, b = 0, l = 8)),  # adjust spae on y ais left 
            legend.position = legend_position,                    # adjust legend position base on input
            plot.title = element_text(hjust = 0.5, size = 20), 
            text = element_text(size=15),
            legend.title = element_blank())
      
    

    
    # return the fina plot 
    return(out_plot)
    
  }

#=====================#
# ==== figure one ====
#=====================#
  
  # loop over grade and subject 
  for(grade_i in c(4,8)){
    for(subject_i in c("Math", "Reading")){
      
      # subset the data 
      # subset data 
      plot_dt <- filter(op_dt, grade == grade_i & subject == subject_i & state=="National" & year>1990 ) %>% 
        
        # select columns we actually need 
        select(year, std_avg, std_p10)
      
      # make the title 
      title_i <- paste0(subject_i, " Grade ", grade_i)
      
      # run the plot function 
      # plot_title,  x_var, x_lab, y1_var, y1_lab, y2_var, y2_lab
      plot_i <- two_axis_ploter(in_data    = plot_dt,
                                plot_title = title_i,
                                x_var      = "year",
                                x_lab      = "Year",
                                y1_var     = "std_p10",
                                y1_lab     = "10th Percentile (std)",
                                y2_var     = "std_avg",
                                y2_lab     = "Average Score (std)")

      # print the plot 
      plot_i
      
      #note if you dont do this ggsave uses the aspect ratio in the plot window which
      # is silly. I'll set it to  5 to 3 for now just for starters 
      aspect_ratio <- 5/3
      height_i <- 5
      
      
      # save the plot. Ill do it as pdf to match stata. can also do other formats 
      ggsave(paste0(res, "fig1_trends_", subject_i, "_", grade_i, ".png"),
             plot = plot_i, 
             height = height_i , 
             width = height_i * aspect_ratio)
      
      print(plot_i)
    }
  }

  

  #===============================#
# ==== variable label xwalk ====
#===============================#
  
  
  # we are going to make the xwalk for variables and lables.
  
  # Define a list. I am using list() and not specifying the length becaues the data we are going to add is so small
  # for large data sets you will see time gains from specifying the length
  label_xwalk <- list()
  
  # This is probably better to just do in excel and import but here is one way to do it right in R 
  # we could just define two vectors. The advantage of this way is we can see the 
  # variable and label side by side and so we should catch mistakes more easily 
  label_xwalk[[1]]  <- data.frame( var = "dr"       , label = "Opioid deaths (per 100k)" )
  label_xwalk[[2]]  <- data.frame( var = "povrate"  , label = "% child poverty")
  # label_xwalk[[3]]  <- data.frame( var = "xpovrate" , label = "% child extreme poverty")
  # label_xwalk[[4]]  <- data.frame( var = "abuse_rate"    , label = "Abuse/neglect rate (per 1,000)" )
  label_xwalk[[5]]  <- data.frame( var = "esl"      , label = "% child non-native English")
  # label_xwalk[[8]]  <- data.frame( var = "noins"    , label = "% children with no health insurance")
  label_xwalk[[9]]  <- data.frame( var = "rtotppe"  , label = "Per pupil expenditures ($2017)")
  label_xwalk[[10]] <- data.frame( var = "prek"     , label = "% children in preschool")
  label_xwalk[[11]] <- data.frame( var = "single"   , label = "% children living in single-parent families")
  label_xwalk[[12]] <- data.frame( var = "births"   , label = "% of births to unmarried women")
  label_xwalk[[13]] <- data.frame( var = "unemp"    , label = "% kids with 1+ unemployed parent")
  label_xwalk[[14]] <- data.frame( var = "immig"    , label = "% kids from immigrant families")

  # collapse this into on data.fram
  label_xwalk_dt <- bind_rows(label_xwalk)
  
#=========================#
# ==== figures 2-9, 12 ====
#=========================#

  
  # subset the data 
  fig_2_9_df <- filter(op_dt, year > 2002 & state == "National") %>% 
  
    # select the columns we need 
    select(year, state, p10_all, label_xwalk_dt$var) %>% 
    
    # remove duplicates 
    unique()
  
  
  # loop over our list of varialbes and labels 
  for(i in 1:nrow(label_xwalk_dt)){
    
    # grab the y2 variable 
    y2_var_i <- label_xwalk_dt$var[[i]]
    
    # grab the y2 label 
    y2_lab_i <- label_xwalk_dt$label[[i]]
    
    # plug into function 
    plot_i <- two_axis_ploter(in_data    = fig_2_9_df,
                              plot_title = "Demographic and Economic Trends",
                              x_var      = "year",
                              x_lab      = "Year",
                              y1_var     = "p10_all",
                              y1_lab     = "10th Percentile NAEP Scores (std)",
                              y2_var     = y2_var_i,
                              y2_lab     = y2_lab_i,
                              legend_position = "bottom")
    
    
    # go ahead and save it 
    aspect_ratio <- 5/3
    height_i <- 5
    
    # save the plot. Ill do it as pdf to match stata. can also do other formats 
    ggsave(paste0(res, "trends_", y2_var_i, ".png"),
           plot = plot_i, 
           height = height_i , 
           width = height_i * aspect_ratio)
    print(plot_i)
    
    
  }

  
#============================#
# ==== figures 10 and 11 ====
#============================#

  data.frame( var = "dr"       , label = "Opioid deaths (per 100k)" )
  data.frame( var = "kg"       , label = "Opioid prescriptions (kg per 100k)")
  data.frame( var = "foster_rate" 	, label = "Number of Chilfren in Foster Care (per 1,000)")
  
  # subset the data 
  fig_10_11_df <- filter(op_dt, year >= 2002 &  year <= 2018 & state == "National") %>% 
    
    # select the columns we need 
    select(year, state, kg, dr, foster_rate) %>% 
    
    # remove duplicates 
    unique()
  
  # plot 10 
  plot_10 <- two_axis_ploter(in_data    = fig_10_11_df,
                            plot_title = "Demographic and Economic Trends",
                            x_var      = "year",
                            x_lab      = "Year",
                            y1_var     = "kg",
                            y1_lab     = "Opioid prescriptions (kg per 100k)",
                            y2_var     = "dr",
                            y2_lab     = "Opioid deaths (per 100k)",
                            legend_position = "bottom")
  
  # plot 11
  plot_11 <- two_axis_ploter(in_data    = fig_10_11_df,
                            plot_title = "Demographic and Economic Trends",
                            x_var      = "year",
                            x_lab      = "Year",
                            y1_var     = "dr",
                            y1_lab     = "Opioid deaths (per 100k)",
                            y2_var     = "foster_rate",
                            y2_lab     = "Number of Chilfren in Foster Care (per 1,000)",
                            legend_position = "bottom")

  # save them 

  aspect_ratio <- 5/3
  height_i <- 5
  ggsave(paste0(res, "trends_kg_overdose.png"),
         plot = plot_10, 
         height = height_i , 
         width = height_i * aspect_ratio)
  print(plot_i)
  ggsave(paste0(res, "trends_overdose_fc.png"),
         plot = plot_11, 
         height = height_i , 
         width = height_i * aspect_ratio)
  print(plot_i)
  
  
#================================#
# ==== replicate infograpgic ====
#================================#
# i.e. figure 2 
  
  # ill do this with a loop like the stata code. We could also facet this if we want all of them in
  # one file 
  # grade_i <- 8
  # subject_i <- 'Reading'

  # loop over grade and subject 
  for(grade_i in c(4,8)){
    for(subject_i in c("Math", "Reading")){
  
  #======================#
  # ==== Set up data ====
  #======================#

    # so above we added each of the two lines individually to the plot. This is fine and 
    # in that case necessary because we had to do the transformation. in general, an 
    # easier way to get multiple lines is to make the data long and use a by group 
    # get variables we need and make data long 
    fig_2_dt <- filter(op_dt, year > 1990 & state == "National" & grade == grade_i & subject == subject_i) %>% 
      
      # select the columns we need 
      select(year, grade, p10score, p25score, p50score, p75score, p90score) %>% 
      
      # remove duplicates 
      unique() 
    
      # make data long so we can do color by group 
      fig_2_dt_l <- pivot_longer(fig_2_dt,  cols = c(p10score, p25score, p50score, p75score, p90score), names_to = "percentile")
      
      
      # designate years of interst 
      # math (92, 96, 00, 03, 11, 19)   reading (94, 98, 03, 11, 19) 
      if(subject_i == "Reading"){
        point_years <- c(1994, 1998, 2003, 2011, 2019)
      }else{
        point_years <- c(1992, 1996, 2000, 2003, 2011, 2019)
      }
      
      # we are going to label years of interest and leave others blank 
      x_labs <- paste0("'", substr(unique(fig_2_dt_l$year), 3,4))
      x_ticks <-  unique(fig_2_dt_l$year)
      x_labs[which(!x_ticks %in% point_years)] <- ""
      
  #===============================#
  # ==== basic plot structure ====
  #===============================#
      
      # get accomodations cut year 
      accom_cut <- ifelse(subject_i == "Math", 1996, 1998)

      accom_dt <-  filter(fig_2_dt_l, year >= accom_cut)
      no_accom_dt <- filter(fig_2_dt_l, year <= accom_cut)
  
    # now make the plot 
    # note we have to replace year with get(x_var) to get the string "year" 
    out_plot <- ggplot(data = fig_2_dt_l, aes(x = year)) +
      
       #set the basic theme (we alter it some)
      # theme_classic() +
        theme_bw() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
      
      # add the lines. color by name, for accomodaitons 
      geom_line(data = accom_dt, aes( y = value, color = percentile, linetype = "a")) +
      geom_line(data = no_accom_dt, aes( y = value, color = percentile, linetype = "d")) +
      
      # add title 
      ggtitle(paste0( subject_i, " Grade ", grade_i)) + 
      
      # change colors of lines. We can pick whatever. These are specified with hexidecimal codes 
      # And are supposed to be colorblind friendly 
      # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
      scale_colour_manual(values= c("#999999", "#E69F00", "#56B4E9", "#009E73", "#CC79A7")) +
      
      # modify labes 
      labs(y = "Scale Score",
           x = "Year",
           colour = "Percentile") + 
      
      
      # put in more x axis labels 
      # also set the x axis to go to 2023 to leave room for the labels 
      scale_x_continuous(breaks = unique(fig_2_dt_l$year),
                         labels = x_labs,
                         limits = c(1992, 2023)) +
        scale_y_continuous(breaks =function(x) pretty(x, n = 6)) +
      
      #  do all the theme stuff 
      #note we are taking the legend off so we can just insert text ourselves 
      theme( legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 20),
            text = element_text(size=15),
            axis.text.y = element_text( margin = margin(t = 0, r = 5, b = 0, l = 8)))
    

  #=====================#
  # ==== add labels ====
  #=====================#
      
    # Get the y max and add 15 to it for the position of the "Percentile" label 
    max_y <- max(fig_2_dt_l$value)
    header_data <- data.frame(label = "Percentile", xpos = 2020, ypos = max_y + 15)
    
    # add the word Percentil 
    out_plot <- out_plot + geom_text(mapping = aes( x = xpos, y = ypos),
                                     size = 6,
                                      data = header_data, 
                                      label = header_data$label)
      
    # now get locations for each percentile label  
    label_data <- filter(fig_2_dt_l, year == 2019) %>% 
      
      # add proper label for each group to the data 
      mutate( label = str_extract(percentile, "[0-9]+")) %>% 
      mutate( label = paste0(label, "th"))
    
    # now add in the percentil labels 
    out_plot <- out_plot + geom_text(mapping = aes( x = 2022, y = value),
                                     size = 6,
                                     data = label_data, 
                                     label = label_data$label)
    
    
  #=====================#
  # ==== add points ====
  #=====================#
  # here we are adding lables and points for specific years 
  # Brian said we might want different years or not at all, but here is how we do it
  # 3 can change years or delete 
    
    # subset data to these years 
    point_dt <- filter(fig_2_dt_l, year %in% point_years)
    
    # add points 
    out_plot <- out_plot + geom_point(data = point_dt,
                                       aes(x = year, 
                                           y = value, 
                                           color = percentile), 
                                       shape = 1,
                                       size = 3.5)
    out_plot
    
    # add lables 
    out_plot <- out_plot + geom_text(mapping = aes( x = year, y = value, color = percentile),
                                     data = point_dt, 
                                     label = round(point_dt$value),
                                     nudge_y = 6)

    
    #====================#
    # ==== save them ====
    #====================#
    # save the plot. basic plot 
    # save as pdf
    # aspect_ratio <- 5/3
    # height_i <- 5
    # ggsave(paste0(res, "fig2_trends_", subject_i, "_", grade_i, ".pdf"),
    #        plot = out_plot, 
    #        height = height_i , 
    #        width = height_i * aspect_ratio)
    
    # save as png
    aspect_ratio <- 5/4
    height_i <- 5
    ggsave(paste0(res, "fig2_trends_", subject_i, "_", grade_i, ".png"),
           plot = out_plot, 
           height = height_i , 
           width = height_i * aspect_ratio)

    
    
    }# close loops 
  }
  
  

  
