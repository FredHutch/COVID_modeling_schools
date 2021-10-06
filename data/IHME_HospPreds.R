library(dplyr)
library(ggplot2)
require(lubridate)

IHMEpredfiles <- c("data/IHMEProjections/2020_04_05.05.us/Hospitalization_all_locs.csv",
           "data/IHMEProjections/2020_04_09.02/Hospitalization_all_locs.csv",
           "data/IHMEProjections/2020_04_12.02/Hospitalization_all_locs.csv")
IHMEpreddata <- read.csv(IHMEpredfiles[1])

IHMEpreddata <- IHMEpreddata %>%
  mutate(dataseries = 1)

for (i in 2:length(IHMEpredfiles)){
  temp <- read.csv(IHMEpredfiles[i])
  temp <- temp %>%
    mutate(dataseries = i)
  IHMEpreddata <- bind_rows(IHMEpreddata, temp)
}

rm(temp)

# Full list of states
states <- as.vector(unique(IHMEpreddata$location_name)[c(1:55)])

# Data on states
statepop <- read.csv("~/Documents/Home/corona/states.csv", header = TRUE, stringsAsFactors = FALSE)
statepop <- as.data.frame(statepop)
data(state)
abbr <- data.frame(state.name, state.abb)
statepop <- left_join(statepop, abbr, by = c("State" = "state.abb"))

# States to be plotted
CDCStates <- c("Utah", "Tennessee", "Oregon", "Ohio", "New York", "New Mexico", "Minnesota", "Michigan", "Maryland", "Iowa", "Georgia", "Connecticut", "Colorado", "California")

IHMEplotdata <- IHMEpreddata %>%
  filter(location_name %in% states) %>%
  left_join(statepop, by = c("location_name" = "state.name")) %>%
  mutate(HospRate = allbed_mean/Population*100000,
         doy = yday(date))

write.csv(IHMEplotdata, "data/IHMEProjections/IHMEplotdata.csv", row.names = FALSE)



# [1] "Wyoming"                                                     
# [2] "Wisconsin"                                                   
# [3] "West Virginia"                                               
# [4] "Washington"                                                  
# [5] "Virginia"                                                    
# [6] "Vermont"                                                     
# [7] "Utah"                                                        
# [8] "United States of America"                                    
# [9] "Texas"                                                       
# [10] "Tennessee"                                                   
# [11] "South Dakota"                                                
# [12] "South Carolina"                                              
# [13] "Rhode Island"                                                
# [14] "Pennsylvania"                                                
# [15] "Other Counties, WA"                                          
# [16] "Oregon"                                                      
# [17] "Oklahoma"                                                    
# [18] "Ohio"                                                        
# [19] "North Dakota"                                                
# [20] "North Carolina"                                              
# [21] "New York"                                                    
# [22] "New Mexico"                                                  
# [23] "New Jersey"                                                  
# [24] "New Hampshire"                                               
# [25] "Nevada"                                                      
# [26] "Nebraska"                                                    
# [27] "Montana"                                                     
# [28] "Missouri"                                                    
# [29] "Mississippi"                                                 
# [30] "Minnesota"                                                   
# [31] "Michigan"                                                    
# [32] "Massachusetts"                                               
# [33] "Maryland"                                                    
# [34] "Maine"                                                       
# [35] "Louisiana"                                                   
# [36] "Life Care Center, Kirkland, WA"                              
# [37] "King and Snohomish Counties (excluding Life Care Center), WA"
# [38] "Kentucky"                                                    
# [39] "Kansas"                                                      
# [40] "Iowa"                                                        
# [41] "Indiana"                                                     
# [42] "Illinois"                                                    
# [43] "Idaho"                                                       
# [44] "Hawaii"                                                      
# [45] "Georgia"                                                     
# [46] "Florida"                                                     
# [47] "District of Columbia"                                        
# [48] "Delaware"                                                    
# [49] "Connecticut"                                                 
# [50] "Colorado"                                                    
# [51] "California"                                                  
# [52] "Arkansas"                                                    
# [53] "Arizona"                                                     
# [54] "Alaska"                                                      
# [55] "Alabama" 
# 
# 
