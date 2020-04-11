Hello. This repository is an example of some of the code I have written for various projects.
I mostly use Data.table but have experience with dplyr and tidyverse as well. A few of these 
also use some functions from in-house packages that I used and contributed to while working 
at Education Analytics. 


Functional programming: 

Abraham_Sun_Function: Put Abraham and Sun Method into a generalizable function 

ea_load_2: detect the data type and load it in if it is one of the many supported styles. 
Look for and solve various known issues and attempt to detecta and warn about others 

nm_find_linear_combos: A Fase version of caret::findLinearCombos that impliments Rook Pivoting 
for the LU decomposition rather than a QR decomposition. This was part of a larger project to 
speed up principal component analysis for Value added regressiosn 

partial_name_match: Essentialy a wrapper for  RecordLinkage::compare.linkage that makes it easier 
to do a fuzzy merge and look through potential matches. 



Graphs: 

trend_graphs: Create graphs according to some specific instructions to match an existing inforgraphic
exampels from the code are fig1_trends_Reading_8 and fig2_trends_Math_4



Web scraping: 

automated_bing_search: Autmatically search bing for a large number of terms. Using rvest,
httr, RCurl

get_google_trends_Rselenium: Put a large number of terms into google trends and collect data. 
This incolves interacting with the webiste and so I use Rselenium. 




These are just a handful of examples from the many different things I have worked on.


