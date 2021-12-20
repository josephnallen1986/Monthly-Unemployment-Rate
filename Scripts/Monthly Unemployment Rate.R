library(scales)       # Additional graphics functions
library(RColorBrewer) # Color ramps for graphs and maps
library(gridExtra)    # Functions for arranging multiple plots on a page
library(RSocrata)     # Download or Upload 'Socrata' Data Sets
library(Cairo)        # Create high-quality vector (PDF, PostScript and SVG) and bitmap output
library(tidyverse)    # Collection of R packages designed for data science
library(ggrepel)      # Provides text and label geoms for 'ggplot2' that help to avoid overlapping text labels. 
library(lubridate)    # Makes it easier to work with dates and times.

lf <- read.socrata(
  "https://data.edd.ca.gov/resource/e6gw-gvii.json?$where=year >= 2019 AND (area_type = 'State' OR area_type = 'County') AND seasonally_adjusted_y_n = 'N'") %>%
  mutate(
    date = as.Date(date),
    labor_force = as.numeric(labor_force),
    employment = as.numeric(employment),
    unemployment = as.numeric(unemployment),
    unemployment_rate = as.numeric(unemployment_rate)) %>%
  select(area_name, date, labor_force, employment, unemployment, unemployment_rate)

current_month <- paste(
  months(as.Date(max(lf$date)))," ",
  year(max(lf$date)),sep="")

d <- paste(getwd(),"/Output/",format(as.Date(max(lf$date)), "%y-%m")," ",month.abb[month(as.Date(max(lf$date)))],sep="")
dir.create(d, showWarnings = FALSE)

county_list <- unique(lf$area_name)

county_list <- county_list[! county_list %in% c("California")]

for (active_county in county_list) {
  
  laus <- filter(lf, area_name == active_county | area_name == "California")
  
  dat_labels <- laus %>% 
    group_by(area_name) %>% 
    filter(
      unemployment_rate == max(unemployment_rate) |
        unemployment_rate == min(unemployment_rate) |
        date == max(date)) %>%
    mutate(
      label = paste(
        substr(year(ymd(date)),3,4),"-",
        month(ymd(date), label = TRUE, abbr = TRUE)," - ",
        unemployment_rate * 100, "%",sep=""),
      param =
        case_when(
          date == max(date) ~ "Current",
          unemployment_rate == min(unemployment_rate) ~ "Min",
          unemployment_rate == max(unemployment_rate) ~ "Max",
        )) %>%
    select(area_name, date, unemployment_rate, label, param)
  
  labor_force <- laus %>%
    ggplot(aes(x=date, y=unemployment_rate)) +
      geom_line(size = 1.25, alpha = 0.8, aes(x = date,
                                              y = unemployment_rate,
                                              colour = area_name,
                                              group = area_name)) +
      geom_label_repel(
        data = dat_labels,
        fill = "white",
        alpha = 0.95,
        colour = case_when(
          dat_labels$param == "Min" ~ "blue",
          dat_labels$param == "Max" ~ "red",
          dat_labels$param == "Current" ~ "black"),
        box.padding = 1, point.padding = 0.5, max.overlaps = Inf,
        aes(x = date, y = unemployment_rate, label = label, group = area_name)) +
      geom_point(data= dat_labels,
                 aes(x = date, y = unemployment_rate, size = param),
                 colour = case_when(
                   dat_labels$param == "Min" ~ "blue",
                   dat_labels$param == "Max" ~ "red",
                   dat_labels$param == "Current" ~ "black")) +
    scale_size_manual("Peaks and Lows", values=rep(3,3),
                      guide=guide_legend(override.aes = list(colour=c("black", "red", "blue")))) +
    scale_colour_manual(values = c("#cf7f00", "#00597c"),
                        guide = guide_legend(override.aes = list(
                          linetype = c("solid", "solid")))) +
    labs(
      title = active_county,
      subtitle = "Unemployment Rate Three-Year Trend") +
    theme(text = element_text(colour = "#000000", size=14),
          title = element_text(color = "#00587C"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = NA),
          plot.background = element_rect(fill = "#FFFFFF"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = "top",
          plot.title = element_text(
            hjust = 0.5,
            color = "#00587C", 
            size = 18,
            face="bold"),
          plot.subtitle = element_text(
            hjust = 0.5,
            color = "#00587C", 
            size = 16,
            face="bold"),
          axis.title=element_text(size=16,face="bold")) +
    theme(
      strip.text.x = element_text(
        size = 14, colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(
      date_breaks  ="3 month",
      date_labels="%y %b") +
    scale_y_continuous(
      breaks = seq(0, max(laus$unemployment_rate), by = 0.01),
      labels = scales::percent)
  
  file_name <- paste(d,"/",current_month," ",active_county,".png",sep="") 
  
  ggsave(labor_force, filename = file_name, dpi = 300, type = 'cairo',
         width = 14, height = 8.5, units = 'in', bg="#ffffff")
  
  labor_force
  
  print(file_name)
  
}
