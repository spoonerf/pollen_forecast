#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)
library(PostcodesioR)
library(rgdal)
library(cplm)
library(ggplot2)
library(dplyr)
library(gganimate)
library(lubridate)
library(weatherr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Grass Pollen Forecast"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
            selectInput("date", "Forecast:",
                        c("Today" = Sys.Date(),
                          "Tomorrow" = Sys.Date()+1,
                          "Day after tomorrow" = Sys.Date()+2)),
            
            textInput("postcode",
                      "Postcode:",
                      "SW1A 2AA")
        ),
         mainPanel(
             imageOutput("pollenPlot", width = "590px", height = "590px")
        )
        
       
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


     output$pollenPlot <- renderPlot({
         
         model <- readRDS("pollen_model.RDS")
         hs_model <- readRDS("heat_sum_model.RDS")
         grass <- raster("grass_grains_mean.tif")
         dist2c_ras  <- raster("distance_to_sea.tif")
         ann_mean_temp_ras <- raster("annual_mean_temp_1981_2010.tif")
         
         
         en <- SpatialPoints(postcode_lookup(input$postcode)[,c("eastings", "northings")])
         lon <- as.numeric(postcode_lookup(input$postcode)[,("longitude")])
         lat <- as.numeric(postcode_lookup(input$postcode)[,("latitude")])
         
         doty <- yday(input$date)
         ann_mean_temp <- raster::extract(ann_mean_temp_ras, en)
         dist2c <- raster::extract(dist2c_ras, en)
         
         new_data <- data.frame(doty = doty, 
                                   ann_mean_temp = ann_mean_temp,
                                   dist2c = dist2c)
         
         heat_sum <- predict(hs_model, new_data)
         
         
         ggrm <- raster::extract(grass, en, buffer = 5000, fun = mean)/cellStats(grass, mean)
         
         
         df_int <- locationforecast(lat, lon, exact = FALSE) 
         
         df_int <- df_int %>% 
             filter(interval == 1) %>% 
             select(timefrom,  precipitation) %>% 
             mutate(day = as.Date(timefrom)) %>% 
             group_by(day) %>% 
             summarise(total_precip = sum(precipitation))
         
         
         df <- locationforecast(lat, lon) 
         
         df_out <- df %>% 
             select(time, temperature, windSpeed_mps, humidity) %>% 
             mutate(day = as.Date(time)) %>% 
             group_by(day) %>% 
             summarize(mean_temp = mean(temperature),max_temp = max(temperature), mean_windsp = mean(windSpeed_mps), mean_rel_hum = mean(humidity)) %>% 
             left_join(., df_int, by = "day") %>% 
             filter(day == input$date)
         

         new_data <- data.frame(max_temp = df_out$max_temp, 
                                total_precip = df_out$total_precip,
                                mean_rel_hum = df_out$mean_rel_hum,
                                ann_mean_temp = ann_mean_temp,
                                heat_sum = heat_sum,
                                season = ifelse(doty >= 154 & doty <= 214, "1","0"), #pollen season days for poacae
                                mean_windsp = df_out$mean_windsp,
                                ggrm = ggrm)
         
         pollen <- round(predict(model, new_data), 2)

         risk <- case_when( pollen < 30 ~ "Low risk of hayfever.",
                             pollen >= 30 & pollen <= 49 ~ "Moderate risk of hayfever.",
                            pollen >= 49 & pollen <= 149 ~ "High risk of hayfever.",
                            pollen > 149 ~ "Very high risk of hayfever.")
         n <- floor(pollen)
         x <- runif(n, 0,1)
         y <- runif(n, 0,1)
         
         df <- data.frame(x, y)
         
         ggplot(df, aes(x = x, y = y))+
          #   geom_point(color = "orange", shape = 8)+
             theme_void()+
             ggtitle(paste0(pollen, " grams per cubic metre.\n ", risk))+
             coord_equal()+
             theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5))



         })
     
}

# Run the application 
shinyApp(ui = ui, server = server)
