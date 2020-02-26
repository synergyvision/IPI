#APOYO MAPA INTERACTIVO
#MAPA DE VENEZUELA SENCILLO
geojson <- download_map_data("countries/ve/ve-all")

data <- get_data_from_map(geojson) 
data$value <- round(runif(26,1,200))

mapdata <- geojson

              highchart(type = "map") %>%
              hc_exporting(
                enabled = TRUE,
                buttons = tychobratools::hc_btn_options()
              ) %>%
              hc_add_series(
                mapData = mapdata, 
                data = list_parse(data), 
                joinBy = c("hc-a2"),
                allAreas = FALSE,
                dataLabels = list(enabled = TRUE, format = '{point.value:,.0f}'),
                name = "Número de Sucursales",
                tooltip = list(
                  valueDecimals = 0, 
                  valuePrefix = "$"
                )
              ) %>% 
              hc_plotOptions(
                series = list(
                  allowPointSelect = TRUE,
                  states = list(
                    select = list(
                      color = "green"
                    )
                   )
                  # point = list(
                  #   events = list(
                  #     unselect = state_unselect,
                  #     select = state_select
                  #   )
                  # )
                )        
              ) 
              # %>%
              # hc_colorAxis(auxpar = NULL) %>%
              # hc_title(text = "Distribución de incidencias"),state_unselect))









