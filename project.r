
library(ggplot2)
library(tidymodels)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(rpart)



my_data <- read_csv("nypd_shooting.csv")
print(colnames(my_data))

race_bar <- ggplot(my_data, aes(x = PERP_RACE, fill = PERP_RACE)) +
geom_bar(stat = "count") +
labs(title = "Shootings by NYPD to different Races",
subtitle = "Black tops every other race.")

# Question to ask=> What age group and which race in NY
# has higest police shootings?

print(my_data$STATISTICAL_MURDER_FLAG[1:5])

bar_plot_age <- function(){
    data <- filter(my_data, PERP_AGE_GROUP!= "(null)")
    graph <- ggplot(data, aes(PERP_AGE_GROUP, fill = PERP_AGE_GROUP)) + 
        geom_bar(stat = "count")+
        labs(titel = "The number of different age groups getting shot")
    return(graph)
}

bar_plot_race <- function(){
    data <- filter(my_data, PERP_RACE!= "(null)")
    graph <- ggplot(data, aes(PERP_RACE, fill = PERP_RACE)) + 
        geom_bar(stat = "count") + 
        labs(title = "The number of different races getting shot by NYPD")
    return(graph)
}
plot1 <- bar_plot_age()
plot2 <- bar_plot_race()

grid.arrange(plot1, plot2, nrow = 2)


print(my_data$OCCUR_DATE[1:3])
line_graph_race <- function(){
    data <- filter(my_data, PERP_RACE != "(null)")
    data <- filter(data, OCCUR_DATE != "(null)")
    
    data$OCCUR_DATE <- (as.Date(data$OCCUR_DATE, format = "%m/%d/%Y"))
    print(data$OCCUR_DATE[1:5])
    data$binned <- cut(data$OCCUR_DATE, breaks = 10)
    print(data$binned[1:2])

    

    graph <- ggplot(data, aes(x = binned, group = PERP_RACE,
     color = PERP_RACE)) + 
        geom_freqpoly(stat = "count") + 
        labs(title = "The time graph for NYPD shooting different race of people over one year",
        subtitle = "The count of BLACK people getting killed is significantly higher than any other race and then it is followed by White Hispanic.")

    return(graph)
}



print(line_graph_race())

set.seed(123)


