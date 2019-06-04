# NAME: Shubham Kumar 
# PRN: 18030142032
                            ### INDIAN CRIME RATE ANALYSIS REPORT(2001-2012) ###
#####################################################################################################################
# Loading Essential Libraries 
library(shiny)
library(ggplot2) # Visualization
library(gganimate) # Visualization
library(ggthemes) # Visualization
library(maptools) # Visualization - Map
library(treemap) # Visualization - TreeM
suppressPackageStartupMessages(library(dplyr)) # Querying DataSets
library(RColorBrewer) # Visualization 
set.seed(8000)
#######################################################################################################################
# Loading Data File 
d = read.csv('./01_District_wise_crimes_committed_IPC_2001_2012.csv', header=T)
# Data Size on RAM
print(object.size(d), units="MB")
View(d)
# Loading India Shape File
shp <- readShapeSpatial('./india_shapefile/Admin2.shp')
#######################################################################################################################
# Data Visualization
dim(d) # Getting Dimension of data
colnames(d) # Getting Column Names
summary(d) # Generating summary report of the table
head(d);tail(d) # Data Visualization
######################################################################################################################
# Cleaning - Renaming - Filtering
d$st.dt = paste(tolower(d$STATE.UT),"-",tolower(d$DISTRICT)) #creating new col with st-dist name

names(d) = c('st','dis','yr','mur','attmur','homici','rape','cusrape','otrape','kidn','kidgr','kidoth','dac','dacpre',
             'rob','burg','theft','autoth','theftot','riots','breoftru','cheating','counterf','arson','hurt','dowrydea',
             'asswo','inswo','curbyhus','impogrfor','deaneg','othipc','totipc','st.dt') #renameing columns

d[d$dis=='DELHI UT TOTAL',2] = 'TOTAL' #renaming delhi ut total to total

d = d[d$dis!='TOTAL',]# Removing Totals data,as it doubling the figures

d$othipc = NULL # Removing other ipc as it's skewing the data
for( i  in 1:nrow(d)) {
  d$totipc[i] = sum(d[i,4:31])
}
# Adding Population Data
popu = data.frame(yr=c(2001:2012),popBillion=c(1070000000,1080000000,1100000000,1120000000,1140000000,1160000000,1170000000,1190000000,1210000000,1230000000,1240000000,1260000000))
d = merge(d,popu)
##########################################################################################################################
# Creating Subsets
totby_pop_yr = d %>% select(yr,totipc,popBillion) %>% group_by(yr) %>% summarise(totipc = sum(totipc),pop=mean(popBillion))

totby_pop_yr = totby_pop_yr %>% mutate(perlakh =round((totipc/pop)*100000,0)) #adding per lakh

totby_st_yr = d %>% select(yr,st,totipc,popBillion) %>%  group_by(st,yr) %>%  summarise(totipc=sum(totipc),pop = mean(popBillion))

totby_st = totby_st_yr %>% group_by(st) %>% summarise(totipc=sum(totipc),pop = max(pop))
popwise = totby_st %>% select(st,totipc) %>% arrange(desc(totipc))

totby_dis_yr = d %>% select(dis,totipc,yr,st.dt) %>%  group_by(dis,yr,st.dt) %>%  summarise(totipc = sum(totipc))

totby_dis = totby_dis_yr %>% group_by(dis,st.dt) %>% summarise(totipc=sum(totipc))
###########################################################################################################################
yearPlot <- function() {
  options(repr.plot.width=10, repr.plot.height=6)
  return(ggplot(totby_pop_yr) +geom_bar(aes(yr,totipc,fill=yr),stat='identity') +
    xlab("Year") +ylab("Total Crimes") + ggtitle("Crimes per Year") + guides(fill=FALSE) + xlim(2000, 2013) + theme_classic())
  
}

peoplePlot <- function() {
  options(repr.plot.width=12, repr.plot.height=8)
  
  return(ggplot(totby_pop_yr) + geom_bar(aes(yr,perlakh,fill=yr),stat='identity') +
    xlab("Year") +ylab("Per Lakh") + ggtitle("Crimes per Lakh People") + guides(fill=FALSE) + xlim(2000, 2013) + theme_classic())
  
}

statePlot <- function() {
  options(repr.plot.width=12, repr.plot.height=8)
  return(ggplot(totby_st) + geom_bar(aes(st,totipc,fill=totipc),stat='identity') +
    xlab("Year") +ylab("Crimes") + ggtitle("Crimes per State") + guides(fill=FALSE) + theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

districtPlot <- function() {
  options(repr.plot.width=12, repr.plot.height=6)
  return(ggplot(totby_dis,aes(dis,totipc)) + geom_point(aes(size=totipc,colour=dis,label=dis)) + 
    geom_text(aes(label=ifelse(totipc>200000,as.character(dis),'')),hjust=0.01,vjust=0) +
    xlab("District") +ylab("Crimes") + ggtitle("Crimes per District") + 
    guides(size=FALSE,colour=FALSE) + theme_classic() +
    theme(axis.text.x=element_blank(),axis.line.x = element_blank()))
}

#######################################################################


#######################################################################

indPlot <- function() {
  shp.f <- fortify(shp, region = "ST_NM")
  
  imr = totby_st[,-3]
  names(imr)[1]<-"id"
  names(imr)[2]<-"count"
  
  # Merge shapefile with csv file
  merge.shp.coef<-merge(shp.f,imr, by="id", all.x=TRUE)
  final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]
  
  options(repr.plot.width=16, repr.plot.height=12)
  # Creating the map
  return(ggplot() + geom_polygon(data = final.plot,aes(x = long, y = lat, group = group, fill = count ),
                          color = "red", size = 0.25) + coord_map())
  
  
}

ind = indPlot()



ui <- fluidPage(
  titlePanel("Reported Crimes from 2001-2012 in India"),
  sidebarLayout(
    sidebarPanel(
      h3("Stats"),
      selectInput("countryInput", "Visualization - No. of Reported Crimes",choices = c("Distribution Scenario" = 1 ,"Year-Wise" = 2 , "State-Wise" = 3, 
                                                        "District-Wise" = 4,"Per Lakh Population-Wise" = 5),selected = 1),
      h5("Created By: Shubham Kumar[18030142032] and Subodh Dharmadhikari[18030142043]")
    ),
    
    mainPanel(
      plotOutput("coolplot",height = "500px"),
      htmlOutput("textplot")
    )
  )
  
)


server <- function(input, output) {
  
  output$coolplot <- renderPlot({
    col <- as.numeric(input$countryInput)
    if(col==1){
      plot(ind)
    } else if(col==2) {
      plot(yearPlot())
    } else if(col==3) {
      plot(statePlot())
    } else if(col==4) {
      plot(districtPlot())
    } else if(col==5) {
      plot(peoplePlot())
    }
  })
  
  output$textplot <- renderText({
    col <- as.numeric(input$countryInput)
    if(col==1){
      br()
      HTML("The lighter color shows highest crimes reported in the state.")
    } else if(col==2) {
      br()
      HTML("The Crime Rate was lowest in 2003 but increasing after it and the highest in 2012.")
    } else if(col==3) {
      br()
      HTML("The Graph is showing the Most Crime affected States.")
    } else if(col==4) {
      br()
      HTML(" The Graph is showing the District WiseC Crime reported. ")
    } else if(col==5) {
      br()
      HTML("The Graph is showing the Population Affected by the Crimes during this period.")
    }
  })
}

shinyApp(ui = ui, server = server)

