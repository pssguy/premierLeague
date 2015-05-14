library(engsoccerdata)
library(dplyr)
library(ggplot2)
library(curleylab) #only used for curleytheme() ggplot theme - could make your own or use theme_bw()


df <- engsoccerdata2 %>% filter(Season!=1939) 

df <- 
  rbind(df %>% filter(home=="Arsenal") %>% select(Season, Date, team=home, opp=visitor, gf=hgoal, ga=vgoal, tier),
        df %>% filter(visitor=="Arsenal") %>% select(Season, Date, team=visitor, opp=home, gf=vgoal, ga=hgoal, tier)
  )


df$Date <- as.Date(df$Date, format="%Y-%m-%d")

df <- df %>% arrange(Date) %>% mutate(result = ifelse(gf>ga, "W", ifelse(ga>gf, "L", "D")))

df <- df %>% group_by(Season) %>% mutate(n = row_number())
df$result <- factor(df$result)


ggplot(df, aes(n, Season, fill=result, color=result)) + geom_tile() + curleytheme() + xlab("Game in Season") +
  scale_y_continuous(breaks = c(1890,1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010))+
  coord_fixed(ratio=1) +
  scale_fill_manual(values = c("skyblue3", "ghostwhite", "firebrick1"))+
  scale_color_manual(values = c("black", "black", "black"))


# to add in 2014/15 data need to get from external source. I'll add all 2014/15 data to engsoccerdata at end of the season.


ggplot(df, aes(n, Season, fill=result, color=result)) + geom_tile() + curleytheme() + xlab("Game in Season") +
  scale_y_continuous(breaks = c(1890,1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010))+
  coord_fixed(ratio=1) +
  scale_fill_manual(values = c("green", "red", "blue"))+
  scale_color_manual(values = c("black", "black", "black"))