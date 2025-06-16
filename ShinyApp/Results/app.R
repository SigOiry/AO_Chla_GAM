# library(brms)
library(tidyverse)
library(patchwork)
# library(Utilities.Package)

# setwd("ShinyApp/Results")
df_IceCover_Manual <- read.csv("df_IceCover_RAW.csv")
df_IceCover_Relatif <- read.csv("df_IceCover_Relatif.csv")
df_SST_Manual <- read.csv("df_SST_RAW.csv")
df_SST_Relatif <- read.csv("df_SST_Relatif.csv")
df_WindSpeed_Manual <- read.csv("df_WindSpeed_RAW.csv")
df_WindSpeed_Relatif <- read.csv("df_WindSpeed_Relatif.csv")



colpal <- c("Baffin Bay" = "#326CBE",
            "Barents Sea" = "#C2E5B4",
            "Basin" = "#1F408D",
            "Beaufort Sea"= "#808064",
            "Canadian Archipelago"= "#199A98",
            "Chukchi Sea"= "#538CB2",
            "East Siberian Sea"= "#38B423",
            "Kara Sea" = "#98BF39",
            "Laptev Sea" = "#A7C6EB",
            "Nordic Sea" = "#4D3374")

##### WindSpeed #####



p1 <- df_WindSpeed_Manual %>% 
    ggplot(aes(x = WindSpeed, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Region, fill = Region)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Region)+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of WindSpeed on Chla in raw scales", y = "Chla (mg/m³)", x = "WindSpeed") +
    theme_minimal()

p2 <- df_WindSpeed_Relatif %>% 
    ggplot(aes(x = WindSpeed, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Region, fill = Region)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Region)+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of WindSpeed on Chla in relative scales", y = "Chla (mg/m³)", x = "WindSpeed") +
    theme_minimal()


p3 <- df_SST_Manual %>% 
    ggplot(aes(x = SST, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Region, fill = Region)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Region)+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of SST on Chla in raw scales", y = "Chla (mg/m³)", x = "SST") +
    theme_minimal()

p4 <- df_SST_Relatif %>% 
    ggplot(aes(x = SST, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Region, fill = Region)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Region, scale = "free")+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of SST on Chla in relative scales", y = "Chla (mg/m³)", x = "SST") +
    theme_minimal()

p5 <- df_IceCover_Manual %>% 
    ggplot(aes(x = IceCover, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Region, fill = Region)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Region)+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of IceCover on Chla in raw scales", y = "Chla (mg/m³)", x = "IceCover") +
    theme_minimal()


p6 <- df_IceCover_Relatif %>% 
    ggplot(aes(x = IceCover, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = Region, fill = Region)) +
    geom_line() +
    scale_color_manual(values = colpal)+
    scale_fill_manual(values = colpal)+
    facet_wrap(~Region)+
    geom_ribbon(alpha = 0.2, color = NA) +
    labs(title = "Marginal effect of IceCover on Chla in relative scales", y = "Chla (mg/m³)", x = "IceCover") +
    theme_minimal()


plot_WindSpeed_RAW <- p1+
    plot_layout(guides = "collect") &
    theme(legend.position = "top")

plot_WindSpeed_Relative <- p2+
    plot_layout(guides = "collect") &
    theme(legend.position = "top")

plot_SST_RAW <- p3+
    plot_layout(guides = "collect") &
    theme(legend.position = "top")

plot_SST_Relative <- p4+
    plot_layout(guides = "collect") &
    theme(legend.position = "top")

plot_IceCover_Raw <- p5+
    plot_layout(guides = "collect") &
    theme(legend.position = "top")

plot_IceCover_Relatives <- p6+
    plot_layout(guides = "collect") &
    theme(legend.position = "top")





# app.R ---------------------------------------------------------------
# Load packages -------------------------------------------------------
library(shiny)
library(ggplot2)
library(bslib)    # needed only if you create/modify the ggplots here

# --------------------------------------------------------------------
# 1. Put your eight ggplot objects in the workspace.
#    Name (or assign) them however you like; below we assume:
#      var1_raw, var1_rel, var2_raw, var2_rel, var3_raw, var3_rel, var4_raw, var4_rel
#    ── OR ── replace the 'plots' list below with whatever names you use.

# Example placeholders (delete these lines once your real plots exist)
# var1_raw <- ggplot() + geom_blank() + labs(title = "Var 1 · Raw")
# var1_rel <- ggplot() + geom_blank() + labs(title = "Var 1 · Relative")
# ... repeat for the other six ...

# Nest them in a list ⇒ plots[["Var 1"]][["Raw"]], etc.
# plots <- list(
#   WindSpeed = list(Raw = plot_WindSpeed_RAW, Relative = plot_WindSpeed_Relative),
#   SST = list(Raw = plot_SST_RAW, Relative = plot_SST_Relative),
#   IceCover = list(Raw = plot_IceCover_Raw, Relative = plot_IceCover_Relatives)
# )

plots <- list(
    Raw = list(
        WindSpeed = plot_WindSpeed_RAW,
        SST       = plot_SST_RAW,
        IceCover  = plot_IceCover_Raw
    ),
    Relative = list(
        WindSpeed = plot_WindSpeed_Relative,
        SST       = plot_SST_Relative,
        IceCover  = plot_IceCover_Relatives
    )
)

# --------------------------------------------------------------------
# 2. UI ---------------------------------------------------------------
# ── 2.  USER INTERFACE  ───────────────────────────────────────────────
ui <- fillPage(                           # full viewport
    tags$head(
        tags$style(HTML("
      body, html { margin: 0; }           /* remove default margins     */
      #plot-box, #plot-box > .shiny-plot-output {
        height: 100vh; width: 100vw;      /* plot occupies full screen  */
      }
      #controls {
        position: absolute;
        top: 1rem; right: 1rem;           /* corner placement           */
        width: 230px;
        background: rgba(255,255,255,.92);
        border-radius: .5rem;
        box-shadow: 0 0.5rem 1rem rgba(0,0,0,.15);
        padding: .8rem 1rem;
        z-index: 1000;
      }
      #controls .radio { margin-bottom: .4rem; }
    "))
    ),
    
    div(id = "plot-box",
        plotOutput("displayPlot", height = "100%", width = "100%"),
        
        # floating panel
        div(id = "controls",
            radioButtons("scale", "Scale",
                         choices  = c("Raw", "Relative"),
                         selected = "Raw"),
            radioButtons("variable", "Variable",
                         choices  = names(plots$Raw),
                         selected = names(plots$Raw)[1])
        )
    )
)

# ── 3.  SERVER  ───────────────────────────────────────────────────────
server <- function(input, output, session) {
    
    chosenPlot <- reactive( plots[[input$scale]][[input$variable]] )
    
    output$displayPlot <- renderPlot(res = 120,{
        chosenPlot()
    })
}

# ---------------------------------------------------------------------
shinyApp(ui, server)
# rsconnect::deployApp(appDir = "ShinyApp/Results")
