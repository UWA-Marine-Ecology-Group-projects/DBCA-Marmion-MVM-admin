### Libraries
library(gargle)
library(googlesheets4)
library(htmltools)
library(leafem)
library(leaflet)
library(leaflet.extras) # NEW
library(leafgl)
library(forcats)
library(mongolite)
library(plyr)
library(purrr)
library(raster)
library(rdrop2)
library(rgdal)
library(rgeos)
library(rrapply)
library(sf)
library(shiny)
library(shinyalert)
library(shinyauthr)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus) # NEW
library(shinydisconnect)
library(shinyjs)
library(shinymanager)
library(shinysurveys)
library(shinyFeedback)
library(shinyRadioMatrix)
library(shinyTree)
library(shinyWidgets)
library(shinyvalidate) # NEW
library(sodium)
library(stringr)
library(tidyr)
library(tibble)
library(RColorBrewer)
library(rrapply)
library(stringi)
library(ggplot2)
library(dplyr) # load last to stop issues with plyr 
library(profvis)
library(GlobalArchive)

## Google drive Authentication ----
gs4_auth(cache = "secrets", email = TRUE)

# MongoDB  ----
load("secrets/host.rda")
load("secrets/username.rda")
load("secrets/password.rda")

options(mongodb = list(
  "host" = host,
  "username" = username,
  "password" = password
))

databaseName <- "marmion"

loadData <- function(collection) {
  # Connect to the database
  db <- mongo(collection = collection,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName),
              options = ssl_options(weak_cert_validation = TRUE))
  # Read all the entries
  data <- db$find()
  data
}

# log in information ----
credentials <- readRDS("secrets/credentials.rds")

set_labels(
  language = "en",
  "Please authenticate" = "DBCA’s Marmion MVM Admin",
  "Username:" = "Username:",
  "Password:" = "Password:"
)

# read in key for days ----
days <- read.csv("data/days-scale.csv") %>%
  dplyr::select(days, mean)

# read in matrix q's ----
matrix <- read.csv("data/marmion_activitylist - questions.csv", na.strings=c("","NA")) %>%
  rename(value = response.items.for.matrix.only)

q.9 <- matrix %>%
  filter(question.number == 9)

q.10 <- matrix %>%
  filter(question.number == 10)

q.11 <- matrix %>%
  filter(question.number == 11)

## Read in activity list (downloaded from Googledrive) ----
activities <- read.csv("data/marmion_activitylist - activity-list.csv", na.strings=c("","NA"))

# Create a list of the subcategories for the accordion ----
activity.list <- activities %>%
  dplyr::mutate(nice.title = paste(Category, Sub.category,Activity, sep = " - "),
                nice.cat = Category,
                nice.sub = Sub.category,
                nice.act = Activity) %>%
  dplyr::filter(!Category %in% c("Local knowledge", "Other", "Pressures and threats")) %>%
  dplyr::mutate(category = tolower(stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = "")))) %>%
  dplyr::mutate(subcategory = tolower(stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = "")))) %>%
  dplyr::mutate(activity = tolower(stringr::str_replace_all(.$Activity, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = "")))) %>%
  
  dplyr::mutate(nice.sub.dropdown = paste(nice.cat, nice.sub, sep = " - ")) %>%
  dplyr::mutate(nice.act.dropdown = paste(nice.cat, nice.sub, nice.act, sep = " - ")) %>%           
  glimpse()

other.list <- activities %>%
  dplyr::mutate(nice.title = paste(Category, Sub.category, sep = " - "),
                nice.cat = Category,
                nice.act = Sub.category) %>%
  dplyr::filter(Category %in% c("Other")) %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_"))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_"))) %>%
  glimpse()

values.list <- activities %>%
  dplyr::mutate(nice.title = paste(Category, Sub.category, sep = " - "),
                nice.cat = Category,
                nice.act = Sub.category) %>%
  dplyr::filter(Category %in% c("Local knowledge")) %>%
  dplyr::mutate(category = tolower(stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = "")))) %>%
  dplyr::mutate(subcategory = tolower(stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_","__" = "_",  "\\_$" = "")))) %>%
  glimpse()

pressures.list <- activities %>%
  dplyr::mutate(nice.title = paste(Category, Sub.category,Activity, sep = " - "),
                nice.cat = Category,
                nice.sub = Sub.category,
                nice.act = Activity) %>%
  dplyr::filter(Category %in% c("Pressures and threats")) %>%
  dplyr::mutate(category = tolower(stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = "")))) %>%
  dplyr::mutate(subcategory = tolower(stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = "")))) %>%
  dplyr::mutate(activity = tolower(stringr::str_replace_all(.$Activity, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = "")))) %>%
  
  dplyr::mutate(nice.sub.dropdown = paste(nice.cat, nice.sub, sep = " - ")) %>%
  dplyr::mutate(nice.act.dropdown = paste(nice.cat, nice.sub, nice.act, sep = " - ")) %>%           
  glimpse()

## Read in data from mongo ----
submitted.answers <- loadData("answers") %>% filter(!is.na(time)) %>%
  separate(., time, into = c("date", "time"), sep = "-") %>%
  mutate(year = str_sub(.$date, start = 1, end = 4)) %>%
  mutate(month = str_sub(.$date, start = 5, end = 6)) %>%
  mutate(day = str_sub(.$date, start = 7, end = 8)) %>%
  mutate(hour = as.numeric(str_sub(.$time, start = 1, end = 2))) %>%
  mutate(hour = if_else(timezone %in%c("Etc.UTC"), hour + 8, hour)) %>%
  mutate(min = str_sub(.$time, start = 3, end = 4)) %>%
  mutate(sec = str_sub(.$time, start = 5, end = 6)) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  left_join(days) %>%
  dplyr::mutate(source = if_else(name %in% c("Peter Mccabe", "Bob lang", "David leadbeater", "Judith Massam"), "face", source)) %>%
  dplyr::mutate(source = str_replace_all(.$source, "face2", "face")) %>%
  # filter(!source %in% c("NA", NA, "test")) %>% # TURN THIS OFF FOR TESTING
  glimpse()

user.ids <- submitted.answers %>% distinct(userID)
sources  <- submitted.answers %>% distinct(userID, source)

submitted.polygons <- loadData("polygons") %>% 
  semi_join(user.ids) %>%
  glimpse()

submitted.values <- loadData("values") %>%
  semi_join(user.ids) %>%
  mutate(value = str_replace_all(.$value, c("[^[:alnum:]]" = " ", 
                                            "The current level of protection and management guarantee conservation" = 
                                              "The current level of protection and management of marine areas in the South Coast is sufficient to guarantee conservation of marine ecosystems"))) %>% 
  left_join(sources) %>%
  glimpse() 

# # # # data from googledrive
# submitted.answers <- read_sheet("1Rn5sCqNFWJIIecQ3vpfdh3znVP6M2fcM0__FLmtSI54", sheet = 1) %>%
#   filter(!is.na(time)) %>%
#   separate(., time, into = c("date", "time"), sep = "-") %>%
#   mutate(year = str_sub(.$date, start = 1, end = 4)) %>%
#   mutate(month = str_sub(.$date, start = 5, end = 6)) %>%
#   mutate(day = str_sub(.$date, start = 7, end = 8)) %>%
#   mutate(hour = as.numeric(str_sub(.$time, start = 1, end = 2))) %>%
#   mutate(hour = if_else(timezone %in%c("Etc.UTC"), hour + 8, hour)) %>%
#   mutate(min = str_sub(.$time, start = 3, end = 4)) %>%
#   mutate(sec = str_sub(.$time, start = 5, end = 6)) %>%
#   mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
#   dplyr::rename(activity_or_value = activity.or.value) %>%
#   left_join(days)
# 
# submitted.polygons <- read_sheet("1Rn5sCqNFWJIIecQ3vpfdh3znVP6M2fcM0__FLmtSI54", sheet = 2) %>%
#   rename(number_of_times_clicked = number.of.times.clicked)

list.of.users <- submitted.answers %>%
  distinct(userID, name, email, phone, residence, postcode, gender, age, frequency, visited, date, timezone, time, year, month, day, source) %>%
  dplyr::mutate(row.num = 1:nrow(.)) %>%
  dplyr::select(userID, row.num)

# Create a metadata dataframe with all user info ---
metadata <- submitted.answers %>% 
  distinct(name, email, phone, residence, postcode, gender, age, frequency, visited, date, timezone, year, month, day, source, .keep_all = TRUE)

# double ups = SZMWOYIGLU850532135V and FGGUUUHUKO612539604J

aus <- metadata %>%
  filter(residence %in% c("Australia"))

wa <- aus %>%
  filter(postcode > 5999) %>%
  filter(postcode < 7000)

local <- wa %>%
  filter(postcode %in% c(6020,
                         6023,
                         6025,
                         6027,
                         6028,
                         6029,
                         6030,
                         6031,
                         6032,
                         6033,
                         6034,
                         6035,
                         6036,
                         6037,
                         6038))

socialmedia <- metadata %>%
  filter(source == "socialmedia")

email <- metadata %>%
  filter(source == "email")

website <- metadata %>%
  filter(source == "website")

launch <- metadata %>%
  filter(source == "launch")

test <- metadata %>%
  filter(source == "test")

# Create a dataframe for all activities and values selected ----
selected.all <- submitted.answers %>%
  distinct(name, email, phone, residence, postcode, gender, age, frequency, visited, date, timezone, year, month, day, source, activity_or_value, category, subcategory, activity, description, days, Summer, Autumn, Winter, Spring, .keep_all = TRUE) %>%
  dplyr::select(-c(name, email, phone, residence, postcode, gender, age, frequency, visited, date, time, timezone, year, month, day, hour, min, sec)) %>%
  glimpse()

# Split into activities, values and pressures ----
selected.activities <- selected.all %>%
  dplyr::filter(activity_or_value %in% c("activity")) %>%
  left_join(activity.list) %>%
  glimpse()

selected.values <- selected.all %>%
  dplyr::filter(activity_or_value %in% c("values")) %>%
  left_join(values.list) %>%
  filter(category %in% c("local_knowledge")) %>%
  glimpse()

selected.pressures <- selected.all %>%
  dplyr::filter(activity_or_value %in% c("pressures")) %>%
  left_join(pressures.list) %>%
  filter(category %in% c("pressures_and_threats")) %>%
  glimpse()

# split selected polygons into activities and values 
selected.polygons.activities <- left_join(submitted.polygons, selected.activities) %>%
  dplyr::select(-c(time, timezone, number_of_times_clicked)) %>%
  dplyr::filter(activity_or_value %in% c("activity")) %>%
  filter(!is.na(nice.cat)) %>%
  filter(mean > 0) %>%
  glimpse()

total.polygons.clicked.per.activity.user <- selected.polygons.activities %>%
  distinct(userID, mean, id, nice.cat, nice.sub, nice.act) %>%
  dplyr::group_by(userID, mean, nice.cat, nice.sub, nice.act) %>%
  dplyr::summarise(total.cells.clicked = n()) %>%
  mutate(weighted.score = mean / total.cells.clicked)

selected.polygons.activities <- left_join(selected.polygons.activities, total.polygons.clicked.per.activity.user)

selected.polygons.values <- left_join(submitted.polygons, selected.values) %>%
  dplyr::select(-c(time, timezone, number_of_times_clicked)) %>%
  dplyr::filter(activity_or_value %in% c("values")) %>%
  filter(!is.na(nice.cat))

selected.polygons.pressures <- left_join(submitted.polygons, selected.pressures) %>%
  dplyr::select(-c(time, timezone, number_of_times_clicked)) %>%
  dplyr::filter(activity_or_value %in% c("pressures")) %>%
  filter(!is.na(nice.cat))

# Format data for downloading ----
names(metadata)
dl.user.metadata <- metadata %>%
  dplyr::select(userID, name, email, phone, residence, postcode, gender, age, frequency, visited, source, date, time, timezone) %>%
  ga.clean.names()

dl.all.polygons <- bind_rows(selected.polygons.activities, selected.polygons.values) %>%
  dplyr::mutate(activity.or.knowledge = str_replace_all(.$activity_or_value, c("values" = "knowledge"))) %>%
  dplyr::select(userID, activity.or.knowledge, Category, Sub.category, Activity, description, days, mean, Summer, Autumn, Winter, Spring, id) %>%
  ga.clean.names()

dl.matrix <- submitted.values %>%
  ga.clean.names()%>%
  dplyr::select(userid, value, response)

## Themes ----
Theme1 <-    theme_bw()+
  theme( # use theme_get() to see available options
    panel.grid = element_blank(), 
    panel.border = element_blank(), 
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    legend.title = element_blank(),
    #legend.position = "top",
    text=element_text(size=12),
    strip.text.y = element_text(size = 12,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.y=element_text(size=12),
    axis.text.x=element_text(size=12),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=12, face="bold.italic"))

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    axis.text.x = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    legend.title = element_blank()
  )

## Leaflet spinner ----
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# Read in postcodes ----
# postcodes <- readOGR(dsn="spatial/POA_2021_AUST_GDA2020.shp", layer="POA_2021_AUST_GDA2020")
# save(postcodes, file = "spatial/postcodes.rda")
# 
# postcodes.single <- readOGR(dsn="spatial/postcodes_single_polygons.shp", layer="postcodes_single_polygons")
# save(postcodes.single, file = "spatial/postcodes.single.rda")

# Read in spatial files ----
load("spatial/grid.1km.rda")
load("spatial/postcodes.rda")
load("spatial/postcodes.single.rda")

# Grids for spatial questions ----
SpP <- SpatialPolygons(grid.1km@polygons)

SpP = SpatialPolygonsDataFrame(
  SpP,
  data = data.frame(ID = as.character(c(1:(
    length(SpP@polygons)
  ))),
  display = c(1:(
    length(SpP@polygons)
  ))),
  match.ID = FALSE
)

# FUNCTIONS ----
# Function to change the colour of polygons on leaflet map
change_color <- function(map, id_to_remove, data, colour, new_group){
  leafletProxy(map) %>%
    removeShape(id_to_remove) %>% # remove previous occurrence
    addPolygons(
      data = data,
      layerId = data$ID,
      group = new_group, # change group
      fillColor = colour,
      fillOpacity  = 0.5,
      weight = 1,
      color = "red",
      options = pathOptions(pane = "polygons"))
}

# Functions to create mouseover lat and lon for leaflet maps ----
clipboardDependency = function() {
  list(
    htmltools::htmlDependency(
      name = "clipboard",
      version = "0.0.1",
      src = system.file("htmlwidgets/lib/clipboard", package = "leafem"),
      script = "setClipboardText.js"
    )
  )
}

addmouselatlon <- function(map,
                                epsg = NULL,
                                proj4string = NULL,
                                native.crs = FALSE) {
  
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, c("leaflet", "leaflet_proxy", "mapdeck")))
  
  if (native.crs) { 
    txt_detailed <- paste0("
                           ' x: ' + (e.latlng.lng).toFixed(5) +
                           ' | y: ' + (e.latlng.lat).toFixed(5) +
                           ' | epsg: ", epsg, " ' +
                           ' | proj4: ", proj4string, " ' +
                           ' | zoom: ' + map.getZoom() + ' '")
  } else {
    txt_detailed <- paste0("
                            'Latitude: ' + ((e.latlng.lat).toFixed(5)) + 
                            ' | Longitude: ' + (e.latlng.lng).toFixed(5) +
                           ' (Decimal Degrees)'")
  }
  
  
  txt_basic <- paste0("
                      'Latitude: ' + [0|(e.latlng.lat)] + 
                      '° ' + 
                      [0|((e.latlng.lat)<0?(e.latlng.lat)=-(e.latlng.lat):(e.latlng.lat))%1*60] +
                      ' ' + 
                      ((e.latlng.lat)*60%1*60).toFixed(3) +
                      ' S | ' +
                      
                      'Longitude: ' + [0|(e.latlng.lng)] + 
                      '° ' + 
                      [0|((e.latlng.lng)<0?(e.latlng.lng)=-(e.latlng.lng):(e.latlng.lng))%1*60] +
                      ' ' + 
                      ((e.latlng.lng)*60%1*60).toFixed(3) +
                      ' E (Degrees Minutes Seconds)'
                      
")
  
  map$dependencies = c(
    map$dependencies,
    clipboardDependency()
  )
  
  map <- htmlwidgets::onRender(
    map,
    paste0(
      "
      function(el, x, data) {
      // get the leaflet map
      var map = this; //HTMLWidgets.find('#' + el.id);
      // we need a new div element because we have to handle
      // the mouseover output separately
      // debugger;
      function addElement () {
      // generate new div Element
      var newDiv = $(document.createElement('div'));
      // append at end of leaflet htmlwidget container
      $(el).append(newDiv);
      //provide ID and style
      newDiv.addClass('lnlt');
      newDiv.css({
      'position': 'relative',
      'bottomleft':  '0px',
      'background-color': 'rgba(255, 255, 255, 0.7)',
      'box-shadow': '0 0 2px #bbb',
      'background-clip': 'padding-box',
      'margin': '0',
      'padding-left': '5px',
      'color': '#333',
      'font': '12px/1.5 \"Helvetica Neue\", Arial, Helvetica, sans-serif',
      'z-index': '700',
      });
      return newDiv;
      }
      // check for already existing lnlt class to not duplicate
      var lnlt = $(el).find('.lnlt');
      if(!lnlt.length) {
      lnlt = addElement();
      // grab the special div we generated in the beginning
      // and put the mousmove output there
      map.on('mousemove', function (e) {
      if (e.originalEvent.ctrlKey) {
      if (document.querySelector('.lnlt') === null) lnlt = addElement();
      lnlt.text(", txt_detailed, ");
      } else {
      if (document.querySelector('.lnlt') === null) lnlt = addElement();
      lnlt.text(", txt_basic, ");
      }
      });
      // remove the lnlt div when mouse leaves map
      map.on('mouseout', function (e) {
      var strip = document.querySelector('.lnlt');
      if( strip !==null) strip.remove();
      });
      };
      //$(el).keypress(67, function(e) {
      map.on('preclick', function(e) {
      if (e.originalEvent.ctrlKey) {
      if (document.querySelector('.lnlt') === null) lnlt = addElement();
      lnlt.text(", txt_basic, ");
      var txt = document.querySelector('.lnlt').textContent;
      console.log(txt);
      //txt.innerText.focus();
      //txt.select();
      setClipboardText('\"' + txt + '\"');
      }
      });
      }
      "
    )
  )
  map
}

# Dropdown function -----
create_dropdown <- function(input_name, choices, label) {
  if (!is.null(input[[input_name]]) && input[[input_name]] %in% choices) {
    selected <- input[[input_name]]
  } else {
    selected <- choices[1]
  }
  
  selectInput(
    inputId = input_name,
    label = label,
    choices = choices,
    selected = selected
  )
}

getPalette = colorRampPalette(brewer.pal(9, "Blues"))
