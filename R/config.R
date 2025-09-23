cfg <- local({
  sv <- yaml::read_yaml("config/services.yml")
  vr <- yaml::read_yaml("config/variables.yml")
  th <- yaml::read_yaml("config/thresholds.yml")
  pa <- yaml::read_yaml("config/paths.yml")
  list(services = sv, vars = vr, thresh = th, paths = pa)
})

# sanity check: warn on unmapped services
check_services <- function(df, map) {
  raw <- sort(unique(df$service))
  mapped <- unname(map[raw])
  unknown <- raw[is.na(mapped)]
  if (length(unknown)) warning("Unmapped services: ", paste(unknown, collapse=", "))
  invisible(NULL)
}
