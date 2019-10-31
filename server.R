# define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$map<-renderPlot({
        targetDf <- birdDf
        if (input$breedSeason == 'True') {
            targetDf <- targetDf %>% filter(., breed == 1)
        } else if (input$breedSeason == 'False') {
            targetDf <- targetDf %>% filter(., breed == 0) 
        }
        
        # load map data
        if (input$state == 'All') {
            state_map <- map_data('state')
        } else {
            state_map <- map_data('state', region=input$state)
        }
        
        # summarize and join map data
        if (input$birdName == 'All') {
            bird_loc_summary <- targetDf %>% group_by(., region, subregion) %>%
                summarise(., observation=n())
        } else {
            bird_loc_summary <- targetDf %>% filter(., common.name==input$birdName) %>%
                group_by(., region, subregion) %>%
                summarise(., observation=n())
        }
        bird_loc_summary <- left_join(county_map, bird_loc_summary, by=c('region', 'subregion'))
        
        ggplot(bird_loc_summary) +
            expand_limits(x=county_map$long, y=county_map$lat) +
            scale_fill_continuous(high='blue', low='white', na.value='white') +
            geom_polygon(aes(x=long, y=lat, group=group, fill=observation), color='grey40', size=0.1) +
            theme_map() +
            geom_polygon(data=state_map, mapping=aes(long, lat, group=group), fill=NA, color='black', size=0.4)
    })
    
    output$state_summary<-renderPlot({
        targetDf <- birdDf
        if (input$breedSeason == 'True') {
            targetDf <- targetDf %>% filter(., breed == 1)
        } else if (input$breedSeason == 'False') {
            targetDf <- targetDf %>% filter(., breed == 0) 
        }
        
        if (input$birdName == 'All') {
            bird_state_summary <- targetDf %>% group_by(., region, breed) %>%
                summarise(., observation=n())
        } else {
            bird_state_summary <- targetDf %>% filter(., common.name == input$birdName) %>%
                group_by(., region, breed) %>%
                summarise(., observation=n())
        }
        
        x_axis = bird_state_summary %>% group_by(., region) %>% 
          summarise(., observation=sum(observation)) %>%
          arrange(., desc(observation))
        
        ggplot(data=bird_state_summary[bird_state_summary$region %in% x_axis[1:10,]$region,], aes(x=reorder(region, observation), y=observation, fill=factor(ifelse(breed == 1, TRUE, FALSE)))) +
            geom_bar(stat='identity', position="stack") +
            scale_x_discrete(name='state') +
            scale_fill_discrete(name="breed") +
            theme(legend.position='bottom') +
            coord_flip()
    })
    
    output$month_summary<-renderPlot({
        targetDf <- birdDf
        if (input$birdName != 'All') {
            targetDf <- targetDf %>% filter(., common.name == input$birdName)
        }
        if (input$state != 'All') {
            targetDf <- targetDf %>% filter(., region == input$state)
        }
        
        bird_month_summary <- targetDf %>% group_by(., month=format(date, '%b')) %>%
            summarise(., observation=n(), breed=ifelse(max(breed) == 1, TRUE, FALSE))
        ggplot(data=bird_month_summary, aes(x=factor(month, levels=month.abb), y=observation)) +
            geom_col(aes(fill=breed)) +
            scale_x_discrete(name='month', limits=month.abb)
    })
    
    output$bird_summary<-renderPlot({
        targetDf <- birdDf
        if (input$state != 'All') {
            targetDf <- targetDf %>% filter(., region == input$state)
        }
        
        if (input$breedSeason == 'True') {
            targetDf <- targetDf %>% filter(., breed == 1)
        } else if (input$breedSeason == 'False') {
            targetDf <- targetDf %>% filter(., breed == 0) 
        }
        
        bird_summary <- targetDf %>% group_by(., name=common.name) %>%
            summarise(., observation=n())
        ggplot(data=bird_summary, aes(x=reorder(name, observation), y=observation)) +
            geom_col(fill='light blue') +
            scale_x_discrete(name='name') +
            coord_flip()
    })
    
    output$table <- renderDataTable({
        datatable(birdDf, rownames=FALSE) %>%
            formatStyle(input$selected, background='skyblue', fontWeight='bold')
    })
})