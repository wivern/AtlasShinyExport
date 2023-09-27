(input <- '{"idOfSomeSort":3104735880413999909}')

(output <- jsonlite::fromJSON(input, simplifyVector = F, digits = 9) |> 
    jsonlite::toJSON(auto_unbox = T, digits = NA))

input == output