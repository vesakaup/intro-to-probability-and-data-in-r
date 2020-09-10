library(gridExtra)
library(ggplot2)
library(dplyr)
library(maps)
library(evaluate)
library(scales)

load("brfss2013.RData")
summary(brfss2013$smokday2)



brfss_c <-brfss2013 %>% mutate(smoke_everyday = as.factor(ifelse(smokday2 =='Every day', 'yes', 'no')))
brfss_c <- brfss_c %>% filter(!is.na(smoke_everyday))
                                      
summary(brfss_c$smoke_everyday)
brfss_c<- brfss_c %>%
        filter(!is.na(brfss_c$sex))



brfss_men <- brfss_c %>% filter(brfss_c$sex=='Male')
brfss_women <- brfss_c %>% filter(brfss_c$sex=='Female')

smoke_all <- data.frame(smoke= brfss_c$smoke_everyday,
                         state = tolower(brfss_c$X_state),
                         gender = brfss_c$sex)
smoke_men <- data.frame(smoke= brfss_men$smoke_everyday,
                         state = tolower(brfss_men$X_state),
                         gender = brfss_men$sex)
smoke_women <- data.frame(smoke= brfss_women$smoke_everyday,
                           state = tolower(brfss_women$X_state),
                           gender = brfss_women$sex)

smoke_all_state <- smoke_all %>% group_by(state) %>% summarize(mean= mean(smoke=='yes'))
smoke_men_state <- smoke_men %>% group_by(state) %>% summarize(mean= mean(smoke=='yes'))
smoke_women_state <- smoke_women %>% group_by(state) %>% summarize(mean= mean(smoke=='yes'))

states <- map_data('state')

map_all <- ggplot(smoke_all_state, aes(fill = mean)) + 
        geom_map(aes(map_id = state), map = states) + expand_limits(x = states$long, y = states$lat) +
        ggtitle('Share of everyday smokers') +
        guides(fill=guide_legend(title="Share"))

map_men <- ggplot(smoke_men_state, aes(fill = mean)) + 
        geom_map(aes(map_id = state), map = states) + expand_limits(x = states$long, y = states$lat) +
        ggtitle('Share of everyday smokers (men)') +
        guides(fill=guide_legend(title="Count"))

map_women <- ggplot(smoke_women_state, aes(fill = mean)) + 
        geom_map(aes(map_id = state), map = states) + expand_limits(x = states$long, y = states$lat) +
        ggtitle('Share of everyday smokers (women)') +
        guides(fill=guide_legend(title="Count"))

grid.arrange(map_all, map_men, map_women)


ggplot(data=brfss_c, aes(x=smokday2, y=physhlth, fill=sex)) + geom_boxplot()

by(brfss_c$physhlth, brfss_c$smokday, summary)

ggplot(data=brfss_c,  aes(x= educa, fill=smoke_everyday)) + geom_bar(position='fill') + coord_flip() + 
                scale_y_continuous() +
                labs(y='share of everyday smokers', x='education level')


