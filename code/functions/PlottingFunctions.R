# Function: PlottingFunctions.R
# By: Sabine Loos
# Latest Update: 02.02.2021

# DESCRIPTION: These functions change the elements of a theme for plots in ggplot

# FUNCTIONS: There are multiple functions in this file including:
## plotTheme - general plotting theme
## plotThemeMin - for very minimal plots
## plotThemeCoeff - plotting theme for a coefficient plot
## plotThemeMap - plotting theme for maps
## plot_raster_nepal - make maps of 11 districts outside Kathmandu using a raster dataset
## plot_points_nepal - make maps of 11 districts outside Kathmandu using a points dataset
## saveplot - wrapper function for ggsave, with pre-specified inputs

# DEPENDENCIES:
## mapping functions call sp2gg


# loading required packages, functions and data ---------------------------
require(ggplot2)
if(!exists("sp2gg")){
  source("code/functions/sp2gg.R") #contains functions to convert spatial to ggplot dataframe
}
# see if district data exists
if(!exists("dist11_gg")) {
  # load boundary polygon for the 11 districts outside of Nepal
  dist11_shp <- readRDS("data/in/Nepal_11dist_boundaries.rds")
  if(class(dist11_shp) == "SpatialPolygons"){
    dist11_shp <- as(dist11_shp, "SpatialPolygonsDataFrame")
  }
  dist11_gg <- sp2gg(dist11_shp) #self-written functions
}
# fonts (change if on windows)
suppressMessages(require(extrafont))
suppressMessages(font_import(pattern = "Roboto", prompt = F))
suppressMessages(loadfonts(device = "win"))

# color palettes
warmpal <-colorRampPalette(c("#FDF1EC","#F4C2B4", "#EA927B", "#A85355","#66142E", "#1E0B11"))
coolpal <- colorRampPalette(c("#F4F6CC","#B9D5B2","#568F8B","#1B485E","#122740"))
dark <- "#66142E"
light <- "#F4C2B4"

# set plot theme
theme_set(theme_bw())
# Plotting themes ---------------------------------------------------------
plotTheme <- function(base_size = 12) {
  theme(
    text = element_text(size=(base_size-2), color = "black", family = "Open Sans"),
    plot.title = element_text(size = (base_size),colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=(base_size-2)),
    axis.title = element_text(size=(base_size-2)),
    axis.text = element_text(size=(base_size-2)),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.key.width = unit(8, "pt"),
    legend.key.height = unit(8, "pt"),
    plot.margin=grid::unit(c(1,1,1,1), "mm"),
    legend.margin = margin(c(3,1,1,1)),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(size=(base_size-2),colour = "black", face = "italic"))
}

plotThemeMin <- function(base_size = 12) {
  theme(
    text = element_text(size=(base_size-2), color = "black", family = "Open Sans"),
    plot.title = element_text(size = (base_size),colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=(base_size-2)),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    plot.margin=grid::unit(c(1,1,1,1), "mm"),
    legend.position = "none")
}

plotThemeCoeff <- function(base_size = 12) {
  theme(
    text = element_text(size =(base_size-2), color = "black", family = "Roboto"),
    plot.title = element_text(size = (base_size),colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    # axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=(base_size-2)),
    axis.title = element_text(size=(base_size)),
    axis.text = element_text(size=(base_size-2)),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

plotThemeMap <- function(base_size = 12) {
  theme(
    text = element_text( color = "black", family = "Open Sans"),
    plot.title = element_text(size = (base_size+4),colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.background = element_rect(fill = "transparent", color = "white"),
    strip.text = element_text(size=base_size),
    axis.title = element_text(size=(base_size-2)),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.8,0.9),
    legend.direction = "horizontal",
    legend.text = element_text(size = (base_size-2),colour = "black", face = "italic"#,
                               # margin = margin(r = 1, unit = "pt")
    ),
    legend.title = element_text(size=(base_size-2), vjust = 1),
    legend.key = element_blank(),
    legend.key.width = unit(12, "pt"),
    legend.key.height = unit(8, "pt"))
  # plot.margin=grid::unit(c(0,0,0,0), "mm"))
  # legend.margin = margin(c(1,1,1,1)))
}

# Mapping Nepal ----------------------------------------------
plot_raster_nepal <- function(raster, draw_kathmandu = T, 
                              draw_districts = T,
                              draw_field = F,
                              draw_scalebar = T,
                              dist_gg = dist11_gg,
                              field_df = field_df,
                              legend_title = "",
                              scale_legend = F,
                              leg_labs = NA,
                              legend_vals = c(0,0.5,1),
                              legend_lims = round(range(as.vector(raster), na.rm = T),2),
                              base_size = 10,
                              line_size = 0.25,
                              col_pal =  coolpal(1000),
                              field_col = "black",
                              ...) {
  require(rasterVis);
  # latitude for kathmandu
  kat_pt <- data.frame(long = 85.3240, lat = 27.7172, name = "Kathmandu")
  
  # base plot (just raster)
  p <- gplot(raster) + geom_tile(aes(fill = value))
  
  # adding scale bar
  if(draw_scalebar){
    require(ggsn)
    p <- p + ggsn::scalebar(dist11_gg,location = "bottomleft",dist = 25, transform = T,
                            dist_unit = "km", st.size=2,st.dist = 0.03, st.color = "black",
                            box.color = "snow3",box.fill = c("snow3", "white"),height=0.02, border.size = 0.5, model = 'WGS84')
  }
  
  
  # adding districts
  if(draw_districts){
    # plot
    p <- p + geom_polygon(data = dist11_gg, aes(long, lat, group=group), 
                          colour = "snow3", fill = "transparent", size = line_size)
  }
  
  # adding point for kathmandu
  if (draw_kathmandu) {
    p <- p + geom_point(data = kat_pt, aes(long, y = lat), shape = 18, color = "tomato3")+
      geom_text(data = kat_pt, aes(long, y = lat, label = name), hjust = 0, nudge_x = 0.04, size = 2,family = "Open Sans")
  }
  
  # adding points for field
  if(draw_field){
    p <- p+ geom_point(data = field_df, aes(coords.x1, coords.x2),shape = 19, size = 0.75, color = field_col)
  }
  # final touches
  if(scale_legend){
    if(is.na(leg_labs)){
      leg_labs <- legend_lims
    }
    p <- p +
      scale_fill_gradientn(legend_title,
                           na.value = NA,
                           colours = col_pal, breaks = legend_lims,
                           guide=guide_colourbar(title.position="top", ticks = T),
                           values = legend_vals, limits = c(legend_lims[1], legend_lims[length(legend_lims)]),
                           labels = leg_labs)+
      plotThemeMap(base_size = base_size) + coord_equal()
  }else{
    p <- p +
      scale_fill_gradientn(legend_title,na.value = NA, 
                           colours = col_pal,
                           guide=guide_colourbar(title.position="top", ticks = T))+
      plotThemeMap(base_size = base_size) + coord_equal()
  }
  
  return(p)
}



plot_points_nepal <- function(field_df, draw_kathmandu = T, 
                              draw_districts = T,
                              draw_field = F,
                              dist_gg = dist11_gg,
                              legend_title = "",
                              scale_legend = F,
                              base_size = 10,
                              line_size = 0.5,
                              legend_lims = round(range(as.vector(raster), na.rm = T),2),
                              ...) {
  require(ggsn)
  # latitude for kathmandu
  kat_pt <- data.frame(long = 85.3240, lat = 27.7172, name = "Kathmandu")
  
  # base plot (just points)
  p <- ggplot(field_df) + geom_point(data = field_df, aes_string("LON", "LAT",...))+
    ggsn::scalebar(dist11_gg,location = "bottomleft",dist = 25, transform = T,
                   dist_unit = "km", st.size=2, height=0.01,  model = 'WGS84')
  
  # adding districts
  if(draw_districts){
    p <- p + geom_polygon(data = dist11_gg, aes(long, lat, group=group), 
                          colour = "snow3", fill = "transparent", size = line_size)
  }
  
  # adding point for kathmandu
  if (draw_kathmandu) {
    p <- p + geom_point(data = kat_pt, aes(long, y = lat), shape = 18, color = "tomato3")+
      geom_text(data = kat_pt, aes(long, y = lat, label = name), hjust = 0, nudge_x = 0.04, size = 3,family = "Open Sans")
  }
  
  p <- p +  plotThemeMap(base_size = base_size) + coord_equal()
  
  return(p)
}
# Saving plots ------------------------------------------------------------
saveplot <- function(plot = last_plot(), width = 6, height = 4, 
                     units = "in", dpi = 300, 
                     file_locn = "results/", 
                     file_name = "plot",...){
  ggsave(paste0(file_locn, file_name, ".png"), plot = plot, 
         width = width, height = height, units = units, dpi = dpi, ...=...)
}

