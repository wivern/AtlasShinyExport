x <- jsonlite::read_json("data/synpuf10k_by_event.json")


treemapData <- x$treemapData

x$treemapData <- NULL



