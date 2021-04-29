library(readxl)
library(purrr)
library(tibble)
library(dplyr)
library(stringr)
library(randomcoloR)
library(fs)

### Helper functions for the server side of the app.

### Conventions:
###    rFileinfo:  The reactive fileinfo structure returned by the file browser

tag.noscen <- '->No scenarios selected<-'     # placeholder when no scenario selected

#### State variables
last.region.filter <- NULL

#' Load default data into UI
#'
#' Returns the data from the default data file
#'
#' @param regionsSettings the region to region group mappings
#'
#' @export
loadDefault <- function(regionSettings)
{
  filenames <- list.files("./data")
  data <- list()

  for (filename in filenames) {
    data[[scenarioName(filename)]] <- loadProject2(file.path('./data', filename), regionSettings)
  }

  data
}

#' @export
scenarioDescriptions <- list(
  Paris_Forever = "Paris Nationally Determined Contribution (NDC) targets are met by all countries by 2030 and retained thereafter.",
  Paris_2C = "Paris Nationally Determined Contribution (NDC) targets are met by all countries by 2030, after which there is an emissions cap based on a global emissions trajectory designed to ensure that the 2100 global surface mean temperature does not exceed 2°C above pre-industrial levels with a 50% probability.",
  Accelerated_Actions = "More near-term actions are taken relative to Paris 2°C, and global emissions are consistent with ensuring that the 2100 global surface mean temperature does not exceed 1.5°C above pre-industrial levels with a 50% probability. Note: Climate results are shown for a slightly different 1.5°C scenario (Paris 1.5°C) that uses a global emissions price."
)

#' Load the default project file into the settings
#'
#' Returns the settings from the default project file
#' @export
loadDefaultProjectSettings <- function()
{
  loadProjectSettings('./data/Paris_Forever.xls')
}

#' Load the default region colors
#'
#' Returns the region colors from the default project file
#' @export
loadDefaultRegionSettings <- function()
{
  loadRegionSettings('./data/Paris_Forever.xls')
}

#' Load the default sector colors
#'
#' Returns the sector colors from the default project file
#' @export
loadDefaultSectorColors <- function()
{
  loadSectorColors('./data/Paris_Forever.xls')
}

#' Load the default percentile colors
#'
#' Returns the percentile colors from the default project file
#' @export
loadDefaultPercentileColors <- function()
{
  loadPercentileColors('./data/Paris_Forever.xls')
}



#' Load the default sector colors
#'
#' Returns the sector colors from the default project file
#' @export
loadDefaultGroupColors <- function()
{
  loadGroupColors('./data/Paris_Forever.xls')
}

#' Load a file into the UI
#'
#' Returns the data from the project file, if valid
#' @param projFile Path to the project file
#' @param regionSettings mapping from region to region group
#' @export
loadProject2 <- function(projFile, regionSettings)
{
    if (is.character(projFile)) {
        if (file.exists(projFile)) {
            if (file.access(projFile, mode = 6) != 0) {
                stop("File ", projFile, " exists but lacks either read or write permission.")
            }

            prjdata_econ <- readFromExcel(projFile, "data_econ", regionSettings)
            prjdata_climate <- readFromExcel(projFile, "data_clim", regionSettings)
            prjdata <- c(prjdata_econ, prjdata_climate)
        }
        else {
            prjdata <- list()
        }
         attr(prjdata, "file") <- projFile
         attr(prjdata, "scenario_name") <- scenarioName(projFile)
    }
    else {
        stop("loadProject2: invalid object passed as proj argument; proj must be a filename.")
    }
    prjdata
}

scenarioName <- function(file_path) {
  file_path %>% path_file() %>% path_ext_remove()
}

#' Load a file into the settings
#'
#' Returns the settings from the project file
#' @param proj Path to the project file
#' @export
loadProjectSettings <- function(file) {
    read_excel(file,
              sheet = "query",
              cell_cols("A:D"),
              col_names = c("query", "order", "type", "dataType")) %>%
      mutate(order = as.integer(order)) %>%
      mutate(query = as.factor(query)) %>%
      mutate(type = as.factor(type)) %>%
      mutate(dataType = as.factor(dataType)) %>%
      arrange(query) %>%
      distinct(query, .keep_all = TRUE)
}

loadRegionSettings <- function(file) {
  read_excel(file,
             sheet = "rgroup",
             cell_cols("A:C"),
             col_names = c("EPPA Region", "Regional Group", "color")) %>%
    mutate(`EPPA Region` = str_replace_all(`EPPA Region`, "_", " ")) %>%
    mutate(`Regional Group` = str_replace_all(`Regional Group`, "_", " ")) %>%
    mutate(`Regional Group` = as.factor(`Regional Group`))
}

loadSectorColors <- function(file) {
  read_excel(file,
             sheet = "colormap",
             cell_cols("A:B"),
             col_names = c("Source", "color")) %>%
    mutate(Source = as.factor(Source))
}

loadPercentileColors <- function(file) {
  read_excel(file,
             sheet = "percentile",
             cell_cols("A:B"),
             col_names = c("Percentile", "color")) %>%
    mutate(Percentile = str_replace_all(Percentile, "_", " ")) %>%
    mutate(Percentile = as.factor(Percentile))
}

loadGroupColors <- function(file) {
  read_excel(file,
             sheet = "groupcolormap",
             cell_cols("A:B"),
             col_names = c("group", "color")) %>%
    mutate(group = as.factor(group))
}

readFromExcel <- function(file, sheet, regionSettings) {
    scenario_name <- scenarioName(file)
    data <- read_excel(file,
                       sheet = sheet,
                       col_types = c("guess", "text", "text", "text", "guess", "guess", "guess", "text"),
                       col_names = c("variable", "Source", "order", "Percentile", "Units", "year", "EPPA Region", "value")) %>%
        add_column(scenario = scenario_name)

    # replace GAMS "Eps" output with 0.
    # See https://www.gams.com/latest/docs/gamside/special_values.htm
    data[data$value == "Eps", "value"] <- "0"

    # Convert numeric columns to numeric
    # I'm not able to read them as numeric without getting tons of warnings
    data$value <- as.numeric(data$value)
    data$year <- as.numeric(data$year)
    data$order <- as.numeric(data$order)

    # GAMS cannot output region names with spaces in them, but we want them to be human-readable
    data <- data %>% mutate(
      `EPPA Region` = str_replace_all(`EPPA Region`, "_", " "),
      Percentile = str_replace_all(Percentile, "_", " ")
    )

    # Add region groups from the region -> region group mappings
    regionSettings <- regionSettings %>% select(c("EPPA Region", "Regional Group"))
    data <- data %>% left_join(regionSettings)

    # split single table into list of tables, named by variable
    # See https://stackoverflow.com/questions/57107721/how-to-name-the-list-of-the-group-split-output-in-dplyr
    data <- mutate(data, variable = factor(variable, levels = unique(variable)))
    data %>%
        group_split(variable, keep = FALSE) %>%
        setNames(unique(data$variable))
}

#' Get the scenarios in the project for display
#'
#' Returns a place holder string if no project has been loaded yet.
#' @param rFileinfo Reactive fileinfo object returned by file browser in the UI.
#' @param concat Separator string to use when concatenating scenario names.
#' @importFrom magrittr "%>%"
#' @export
getProjectScenarios <- function(rFileinfo, concat=NULL)
{
    pd <- rFileinfo()$project.data
    if(is.null(pd)) {
        '->none<-'
    } else {
        scenarios <- rev(rgcam::listScenarios(rFileinfo()$project.data) %>% paste(collapse=concat))
        names(scenarios) <- getScenarioNames(scenarios)
        scenarios
    }
}

#' Get the queries for a project and scenario(s) for display
#'
#' @param rFileinfo Reactive fileinfo object returned by file browser in the UI.
#' @param scenarios List of scenarios.
#' @param concat Separator string for concatenating query names.
#' @importFrom magrittr "%>%"
#' @export
getScenarioQueries <- function(rFileinfo, scenarios, concat=NULL)
{
    prj <- rFileinfo()$project.data
    dataType <- rFileinfo()$project.dataType
    settings <- rFileinfo()$project.settings
    if(is.null(prj)) {
        if(is.null(concat))
            ''                          # probably not intended for display
        else
            '->none<-'                  # probably intended for display
    }
    else if(length(scenarios) == 0 || all(scenarios=='')) {
        if(is.null(concat))
            ''                          # probably not intended for display
        else
            tag.noscen                  # probably intended for display
    }
    else {
        queries <- tryCatch(
            lapply(scenarios, . %>% rgcam::listQueries(prj, .)) %>%
            Reduce(intersect,.) %>%
            sort %>%
            paste(collapse=concat),
            ## errors in the pipeline above are caused by selecting a new data
            ## set that doesn't contain the current scenario.  The problem will
            ## clear up once the scenario selector is repopulated.
            error = function(e) {
                if(is.null(concat)) '' else tag.noscen
            })
        tibble(query = queries) %>%
        left_join(settings) %>%
        arrange(order, query) %>%
        filter(dataType == !!dataType) %>%
        pull(query)
    }
}

getQueryOrder <- function(queries, settings) {
  settings[[query]][[0]]$order
}

#' Indicate whether the UI is in an obviously invalid state.
#'
#' Invalid states frequently occur as transients when a new project is being
#' loaded and the UI elements are being updated.
#'
#' @param prj Project data structure
#' @param scenario Scenario name
#' @param query Query name
#' @return Boolean indicating whether the UI state appears to be valid.
#' @export
uiStateValid <- function(prj, scenario, query)
{
    valid.values <- !(is.null(prj) || scenario == '' || query == '' ||
                          query==tag.noscen)
    if(valid.values) {
        prjscens <- listScenarios(prj)
        valid.scen <- all(scenario %in% prjscens)
    }
    else {
        valid.scen <- FALSE
    }

    ## This if block is the return value
    if(valid.scen) {
        scenqueries <- listQueries(prj, scenario)
        all(query %in% scenqueries)
    }
    else {
        FALSE
    }
}


#' Get the years for which a query is defined
#'
#' @param prj Project data structure
#' @param scenario Name of the scenario
#' @param query Name of the query
#' @export
getQueryYears <- function(prj, scenario, query)
{
    if(!uiStateValid(prj, scenario, query)) {
        c(2005, 2100)
    }
    else {
        range(getQuery(prj, query, scenario)["year"])
    }
}


### Helpers for making plots

#' Plot a default panel
#'
#' Mainly intended for use when no data has been loaded.
#'
#' @param label.text Text to display in the middle of the panel
#' @importFrom ggplot2 ggplot geom_label theme_minimal aes aes_
#' @export
default.plot <- function(label.text='No data selected')
{
    ggplot(mapping=aes(x=0,y=0)) + geom_label(aes_(label=label.text), size=10) +
        theme_minimal()
}

### Data wrangling

#' Extract and format data for a plot
#'
#' @param prjdata Project data structure
#' @param query Name of the query to plot
#' @param pltscen Name of the scenario to plot
#' @param diffscenDifference scenario, if any
#' @param key Aggregation variable.  (e.g., 'EPPA Region' or 'Source')
#' @param percentileOrder if Aggregation variable is percentile, the ordering for those percentiles
#' @param filtervar If not NULL, filter on this variable before aggregating
#' @param filterset:  Set of values to include in the filter operation.  Ignored
#'   if filtervar is NULL.
#' @keywords internal
getPlotData <- function(prjdata, query, pltscen, diffscen, key, percentileOrder, filtervar=NULL,
                        filterset=NULL)
{
    tp <- getQuery(prjdata, query, pltscen) # 'table plot'
    if(!is.null(diffscen)) {
        dp <- getQuery(prjdata, query, diffscen) # 'difference plot'
    }
    else {
        dp <- NULL
    }

    if(!is.null(dp)) {
        ## We're doing a difference plot, so subtract the difference scenario.
        ## Join the data sets first so that we can be sure that we have matched
        ## the rows and columns correctly
        varnames <- names(tp)
        mergenames <- varnames[!varnames %in% c('scenario', 'order', 'Units', 'value')]

        joint.data <- merge(tp, dp, by=mergenames, all=TRUE)
        if(anyNA(joint.data))
            joint.data[is.na(joint.data)] <- 0 # zero out missing values

        value <- joint.data$value.x - joint.data$value.y

        mergenames <- sapply(mergenames, as.name) # Don't eval hyphenated col names

        # Construct the new data frame.  We use the scenario name from the left
        # (dp) data frame.
        tp <- dplyr::rename(joint.data, scenario=scenario.x, Units=Units.x) %>%
           dplyr::select_(.dots=c('scenario', mergenames, 'Units')) %>% cbind(value)
    }

    ## If filtering is in effect, do it now
    if(!is.null(filtervar) &&
       !is.null(filterset) &&
       length(filterset) > 0 &&
       filtervar %in% names(tp)
       ) {

        tp <- dplyr::filter_(tp, lazyeval::interp(~y %in% x, y = as.name(filtervar), x = filterset))
    }

    ## Select the key and year columns, then sum all values with the same
    ## key.  Force the sum to have the name 'value'.
    if(!is.null(key) &&
       toString(key) %in% (tp %>% names %>% setdiff(c('year', 'Units')))
       ) {
      # Do not enforce any special ordering unless we're breaking down by sector and have
      # numbers in the order column, or we're breaking down by percentile
      if (key == "Source" && !any(is.na(tp$order))) {
        ordered_subcategories <- unique(arrange(tp, desc(order))[[key]])
        tp <- tp %>%
          mutate(!!key := factor(!!key, levels = ordered_subcategories, ordered = TRUE)) %>%
          group_by(!!key, year, Units) %>%
          summarise(value = sum(value), order = first(order))
      } else if (key == "Percentile") {
        tp <- tp %>%
          mutate(Percentile = as.factor(Percentile)) %>%
          mutate(Percentile = factor( # order the percentiles correctly
            Percentile,
            levels = c("95th percentile", "75th percentile", "Median", "25th percentile", "5th percentile")
          )) %>%
          group_by(!!! syms(key), year, Units) %>%
          summarise(value = sum(value))
      } else {
        tp <- tp %>%
          group_by(!!! syms(key), year, Units) %>%
          summarise(value = sum(value))
      }
    }
    else {
      tp <- tp %>%
        group_by(year, Units) %>%
        summarise(value = sum(value))
    }
    ## Occasionally you get a region with "0.0" for the unit string because
    ## most of its entries were zero. Fix these so that the column all has the
    ## same unit.
    tp$Units <- summarize.unit(tp$Units)
    tp <- tp %>% filter(value != 0)
    tp
}

#' Summarize the unit column of a GCAM data frame by taking the most common
#' entry.
#'
#' In theory the unit should have a single, common value, but in practice GCAM
#' isn't always great about getting its unit strings consistent.
#' @param unitcol Character vector of unit names.
#' @keywords internal
summarize.unit <- function(unitcol)
{
    unitcol[which.max(table(unitcol))]
}

getRegionColorPalette <- function(regionColors)
{
  color_palette <- regionColors$color
  names(color_palette) <- regionColors[["EPPA Region"]]
  color_palette
}

getGroupColorPalette <- function(groupColors)
{
  color_palette <- groupColors$color
  names(color_palette) <- groupColors$group
  color_palette
}

getSectorColorPalette <- function(sectorColors)
{
  color_palette <- sectorColors$color
  names(color_palette) <- sectorColors$Source
  color_palette
}

getPercentileColorPalette <- function(percentileColors)
{
  color_palette <- percentileColors$color
  names(color_palette) <- percentileColors$Percentile
  color_palette
}


#' Get scenario names
#' @param scenarios a list of scenarios
#' @importFrom stringr str_replace
getScenarioNames <- function(scenarios)
{
  scenario_names <- list()
  for (i in 1:length(scenarios)) {
    scenario_names[[i]] <- scenarios[[i]] %>% str_replace("_", " ")
  }
  scenario_names
}


#' Plot values over time as a bar chart
#' @param prjdata A project data structure
#' @param plot_type The type to plot: stacked or line
#' @param query  Name of the query to plot
#' @param scen  Name of the scenario to plot
#' @param diffscen  Name of the difference scenario, or NULL if none
#' @param subcatvar  Variable to use for subcategories in the plot
#' @param filter  If TRUE, then filter to regions in the rgns argument
#' @param rgns  Regions to filter to, if filter is TRUE.
#' @param regionColors Region colors to use, if plotting by region
#' @param sectorColors Sector colors to use, if plotting by sector
#' @param groupColors Group colors to use, if plotting by group
#' @param percentileColors Percentile colors to use, if plotting by percentile
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes_string geom_bar geom_line theme_minimal ylab scale_fill_manual scale_color_manual scale_x_continuous labs
#' @export
plotTime <- function(prjdata, plot_type, query, scen, diffscen, subcatvar, filter, rgns, regionSettings, sectorColors, groupColors, percentileColors)
{
    if(is.null(prjdata)) {
      list(plot = default.plot())
    }
    else {
        if(filter)
            filtervar <- "EPPA Region"
        else
            filtervar <- NULL

        if(subcatvar=='none')
            subcatvar <- NULL
        else
            subcatvar <- as.name(subcatvar)

        if (plot_type == "line")
            subcatvar <- as.name("EPPA Region")

        if (plot_type == "percentile")
            subcatvar <- as.name("Percentile")

        pltdata <- getPlotData(prjdata, query, scen, diffscen, subcatvar, percentileColors$Percentile,
                               filtervar, rgns)

        if(is.null(pltdata)) return(list(plot = default.plot()))

        plt <- ggplot(pltdata, aes_string('year','value', fill=subcatvar, color=subcatvar)) +
          theme_minimal(base_size = 16) +
          ylab(pltdata$Units) +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 9)) +
          labs(title = query)

        if (is.null(plot_type) || plot_type == "stacked" || is.null(subcatvar)) {
          plt <- plt + geom_bar(stat='identity')
        } else {
          plt <- plt + geom_line(size = 1)
        }

        if(!is.null(subcatvar)) {

            if (subcatvar == "Regional Group") {
              color_palette <- getGroupColorPalette(groupColors)
            } else if (subcatvar == "EPPA Region") {
              color_palette <- getRegionColorPalette(regionSettings)
            } else if (subcatvar == "Percentile") {
              color_palette <- getPercentileColorPalette(percentileColors)
            } else {
              color_palette <- getSectorColorPalette(sectorColors)
            }

            plt <- plt +
              scale_fill_manual(values = color_palette) +
              scale_color_manual(values = color_palette)
        }
        return(list(plot = plt, plotdata = pltdata))
    }
}

#' A group of checkboxes divided by sub-labels
#' @param variable The name of the input group
#' @param choicesByLabel The choices indexed by label
#' @param selected  Name of the query to plot
#' @export
checkboxMultiGroupInput <- function(variable, choicesByLabel = NULL, selected = NULL)
{
  div(id = variable, class = "form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
      choicesByLabel %>% pmap(function(`Regional Group`, `EPPA Region`) {
        checkboxGroupInput("tvRgns", `Regional Group`, choices = `EPPA Region`, selected = selected)
      })
  )
}
