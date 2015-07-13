

##########################################################################################

library(RMySQL)
library(plyr)
library(reshape2)
library(ggplot2)
library(shiny)

m <- dbDriver("MySQL")

## IF YOU GET AN ERROR for too many connections open, use the following code

cons <- dbListConnections(MySQL())
for (con in cons){
  dbDisconnect(con)
}

get_episode_ratings <- function(ratings_story_id,driver=m,group="stage4-infinite"){
  withProgress(message='Retrieving data', detail='This will take a minute', value=0, {
    setProgress(value=0, detail='making connection')})
  con <- dbConnect(m, group = group)
  SQLstatement <- paste("SELECT ratings_rating, ratings_elapsed, ratings_origin, date(ratings_timestamp) as date
                          from infinite.user_ratings
                          where ratings_user_id < 1000000000
                          and ratings_platform in ('ANDROID','IPHONE','WINDOWPH',1)
                          and ratings_rating in ('COMPLETED','SKIP','THUMBUP','SHARE','START','SRCHSTART','SRCHCOMPL')
                          and ratings_story_id = ",ratings_story_id,
                            sep=''
      )
      rs <- dbSendQuery(con, SQLstatement)
      df <- fetch(rs, n=-1)
      dbDisconnect(con)
      return(df)
}

get_episode_name <- function(id, driver=m, group="stage4-infinite"){
    con <- dbConnect(m, group = group)
  SQLstatement <- paste("SELECT episode.thing_title as episode, podcast.thing_title as podcast
                        from cms.thing as episode
                        join cms.object_assign oa
                        on episode.thing_id = oa.object_child_id
                        join cms.thing as podcast
                        on oa.object_parent_id = podcast.thing_id
                        where object_parent_type_id = 31
                        and episode.thing_id = ",id,
                            sep=''
      )
      rs <- dbSendQuery(con, SQLstatement)
      df <- fetch(rs, n=-1)
      dbDisconnect(con)
      return(df)
}

get_other_episode_ratings <- function(ratings_story_id, start.date,end.date,driver=m, group="stage4-infinite"){
  withProgress(message='Retrieving data', detail='This will take a minute', value=0, {
    setProgress(value=0, detail='making connection')})
  con <- dbConnect(m, group = group)
  SQLstatement <- paste("SELECT ratings_story_id, ratings_rating, ratings_elapsed, ratings_origin, ratings_timestamp
                          from infinite.user_ratings ur
                          join cms.thing
			  on ur.ratings_story_id = thing.thing_id
			  join cms.object_assign oa
                          on ur.ratings_story_id = oa.object_child_id
                          where object_parent_id = (
                            						SELECT object_parent_id from cms.object_assign
                          							WHERE object_child_id = ",ratings_story_id,
                          							" and object_parent_type_id = 31)
                          and thing_date >= '",start.date,"'
                          and thing_date <= '",end.date,"'
                          and ratings_user_id < 1000000000
                          and ratings_platform in ('ANDROID','IPHONE','WINDOWPH',1)
                          and ratings_rating in ('COMPLETED','SKIP','THUMBUP','SHARE','START','SRCHSTART','SRCHCOMPL')
                          and ratings_origin in ('FEATURED','SHARED','SEARCH','FOLLOWING','RATED')",
                        sep=''
  )
  rs <- dbSendQuery(con, SQLstatement)
  df <- fetch(rs, n=-1)
  dbDisconnect(con)	
  return(df)
}

##############################
# Call Get Ratings Functions #
##############################

# ep <- get_episode_ratings(398701555)
# other_eps <- get_other_episode_ratings(398701555,start.date='2015-03-01',end.date='2015-06-01')

#################
# Graph episode #
#################
Combine_data <- function(ep,other_eps){
        withProgress(message='Combining Data', detail='This will take a minute', value=0, {
        setProgress(value=0, detail='making connection')})
        
        min <- c()
        for(i in 0:max(ep$ratings_elapsed/60)){
          min[i] <- (nrow(ep) - nrow(ep[ep$ratings_rating %in% c('SKIP','START','SRCHSTART') & ceiling(ep$ratings_elapsed/60) <= i-2,])) / nrow(ep)
        }
        
        # qplot(seq_along(min),min) + ylim(0,max(min)) + xlab('Minutes elapsed') + ylab('Percent users listening') + geom_line(colour='green') +
        #   theme(axis.text.x=element_text(size=16),
        #         axis.text.y=element_text(size=16),
        #         axis.title=element_text(size=14,face='bold'))
        
        ################################
        # Graph other episodes average #
        ################################
        #if(nrow(other_eps) >= 0)
	
        other_episode_table <- data.frame(table(other_eps$ratings_story_id))
        other_eps <- other_eps[other_eps$ratings_story_id %in% other_episode_table$Var1[other_episode_table$Freq >= 200],]
        oet <- data.frame(table(other_eps$ratings_story_id))
        colnames(oet) <- c('story_id','total_ratings')
        
        durations <- c()
        for(i in oet$story_id){
          durations <- c(durations,max(other_eps$ratings_elapsed[other_eps$ratings_story_id == i & other_eps$ratings_rating %in% c('COMPLETED','SRCHCOMPL')]))
        }
        
        oet$durations <- durations
        
        ep_duration <- max(ep$ratings_elapsed[ep$ratings_rating=='COMPLETED'])
        oet <- oet[oet$durations >= ep_duration*0.8 & oet$durations <= ep_duration*1.2,]
        
        other_eps <- other_eps[other_eps$ratings_story_id %in% oet$story_id,]
        
        all_min <- c()
        for(i in 0:max(other_eps$ratings_elapsed/60)){
          all_min[i] <- (nrow(other_eps) - nrow(other_eps[other_eps$ratings_rating %in% c('SKIP','START','SRCHSTART') & ceiling(other_eps$ratings_elapsed/60) <= i-2,])) / nrow(other_eps)
        }
        
        # ##########################
        # Combine Data ############
        # ##########################
        
        graph_this <- data.frame("episode"=min,"all_other"=all_min[seq(1:length(min))], "minutes_elapsed" = seq(0,length(min)-1))
        
        return(graph_this)
}

##################################
# Save Ratings Rates for display #
##################################
Rates_Summary <- function(ep, other_eps){

    ept <- dcast(ep, ratings_origin ~ ratings_rating, fun.aggregate = length)
    ept$TOTAL <- rowSums(ept[,-1])
    episode_skip_rate <- sum(ept$SKIP) / sum(ept[,-1])
    episode_thumbup_share_rate <- (sum(ept$THUMBUP) + sum(ept$SHARE)) / sum(ept$TOTAL)
      
    other_episode_summary <- dcast(other_eps, ratings_story_id ~ ratings_rating, fun.aggregate = length)
    other_episode_average_skip_rate <- sum(other_episode_summary$SKIP) / sum(other_episode_summary[,-1])
    other_episode_average_thumbup_share_rate <- (sum(other_episode_summary$THUMBUP) + sum(other_episode_summary$SHARE)) / sum(other_episode_summary[,-1])
    
    stats_list <- list(
                        "episode_summary" = ept,
                        "episode_skip_rate"= episode_skip_rate,
                        "episode_thumbup_share_rate"= episode_thumbup_share_rate,
                        "other_episode_summary" = other_episode_summary,
                        "other_episode_average_skip_rate" = other_episode_average_skip_rate,
                        "other_episode_average_thumbup_share_rate" = other_episode_average_thumbup_share_rate
                 )
    
    return(stats_list)
}

Main_Function <- function(id, start.date, end.date){
  ep <- get_episode_ratings(id)
  other_eps <- get_other_episode_ratings(id, start.date, end.date)
  stats_list <- Rates_Summary(ep, other_eps)
  graph_this <- Combine_data(ep, other_eps)
  titles <- get_episode_name(id)
  outs <- list("graph_this" = graph_this,
               "stats" = stats_list,
               "titles"= titles)
  
  return(outs)
}


#########
# Graph #
#########
#
#ggplot(graph_this,aes(x=minutes_elapsed)) +
#  geom_line(aes(y=episode, colour="This Episode")) +
#  geom_line(aes(y=all_other, colour="Average of Similar Episodes")) +
#  xlab('Minutes Elapsed') +
#  ylab('Percent of listeners still listening') +
#  ylim(0,1) +
#  theme(
#    axis.text=element_text(size=14),
#    axis.ticks=element_blank(),
#    axis.line=element_blank(),
#    panel.border=element_blank(),
#    panel.grid.major=element_line(color='gray',size=1),
#    legend.position="top",
#    legend.title=element_blank(),
#    legend.text=element_text(size=16)
#    )

shinyServer(function(input, output) {
  #another reactive function for getting episode IDs from
  
  #get_episode_IDSfromprogramname <- reactive({
  #  ##some function that relies on input$program_name
  #  
  #})
  
  #output$Episode_IDs <- renderText({
  #  get_episode_IDSfromprogramname()
  #  })
  
  
  outs <- reactive({
    Main_Function(id=input$episode_id,
                        start.date=input$start.end.date[1],
                        end.date=input$start.end.dates[2]
                        )
  })
  
  output$minutes_elapsed <- renderPlot({
        #outs <- Main_Function(id=input$episode_id,
        
	#                start.date=input$start.end.date[1],
        #                end.date=input$start.end.dates[2]
        #                )
        
        ggplot(outs()$graph_this,aes(x=minutes_elapsed)) +
            geom_line(aes(y=episode, colour="This Episode")) +
            geom_line(aes(y=all_other, colour="Average of Similar Episodes")) +
            xlab('Minutes Elapsed') +
            ylab('Percent of listeners still listening') +
            ylim(0,1) +
            theme(
              axis.text=element_text(size=14),
              axis.ticks=element_blank(),
              axis.line=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_line(color='gray',size=1),
              legend.position="top",
              legend.title=element_blank(),
              legend.text=element_text(size=16)
            )
  })
  
  output$all_text <- renderUI({
    #outs <- Main_Function(id=input$episode_id,
    #                    start.date=input$start.end.date[1],
    #                    end.date=input$start.end.dates[2]
    #                    )
    
    str1 <- paste("<h1><b>", outs()$titles$episode,"</b> from <b>",outs()$titles$podcast,"</b></h1>",sep='')
    str2 <- paste("Total Listens in NPR One: <b>", sum(outs()$stats$episode_summary$TOTAL),"</b>",sep='')
    str3 <- paste("Comparing to <b>",nrow(outs()$stats$other_episode_summary),"similar episodes </b> of the same program with roughly equal durations in the date range specified", sep=' ')
    str4 <- paste("This episode's skip rate was ",round(100*outs()$stats$episode_skip_rate,1),"%, versus a skip rate of ",round(100*outs()$stats$other_episode_average_skip_rate,1),"% across similar episodes",sep='')
    str5 <- paste("This episode's Mark Interesting / Share rate was ",round(100*outs()$stats$episode_thumbup_share_rate,1),"% versus a rate of ",round(100*outs()$stats$other_episode_average_thumbup_share_rate,1),"% across similar episodes",sep='')
    HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
  })
  
    
})
