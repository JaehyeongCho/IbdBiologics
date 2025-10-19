execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cohortDatabaseSchema = cdmDatabaseSchema,
                    cohortTable = "cohort",
                    cohortInclusionTable = paste0(cohortTable, "_inclusion"),
                    cohortInclusionResultTable = paste0(cohortTable, "_inclusion_result"),
                    cohortInclusionStatsTable = paste0(cohortTable, "_inclusion_stats"),
                    cohortSummaryStatsTable = paste0(cohortTable, "_summary_stats"),
                    cohortCensorStatsTable = paste0(cohortTable, "_censor_stats"),
                    oracleTempSchema = NULL,
                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                    outputFolder,
                    databaseId = "Unknown",
                    databaseName = "Unknown",
                    databaseDescription = "Unknown",
                    createCohorts = TRUE,
                    runAnalyses = TRUE,
                    packageResults = TRUE) {
  outputFolder <- normalizePath(outputFolder, mustWork = FALSE)
  if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)

  if (!is.null(oracleTempSchema) && oracleTempSchema != "") {
    warning("The 'oracleTempSchema' argument is deprecated. Use 'tempEmulationSchema' instead.")
    tempEmulationSchema <- oracleTempSchema
  }
  if (connectionDetails$dbms %in% c("oracle", "bigquery", "impala", "spark") && is.null(tempEmulationSchema)) {
    stop(sprintf("DBMS '%s' requires 'tempEmulationSchema' to be set.", connectionDetails$dbms))
  }
  if (!is.null(getOption("andromedaTempFolder")) && !file.exists(getOption("andromedaTempFolder"))) {
    warning("andromedaTempFolder '", getOption("andromedaTempFolder"), "' not found. Attempting to create folder")
    dir.create(getOption("andromedaTempFolder"), recursive = TRUE)
  }

  if (createCohorts) {
    message("Creating exposure and outcome cohorts")
    createCohorts(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTableNames = list(cohortTable = cohortTable,
                                          cohortInclusionTable = cohortInclusionTable,
                                          cohortInclusionResultTable = cohortInclusionResultTable,
                                          cohortInclusionStatsTable = cohortInclusionStatsTable,
                                          cohortSummaryStatsTable = cohortSummaryStatsTable,
                                          cohortCensorStatsTable = cohortCensorStatsTable),
                  tempEmulationSchema = tempEmulationSchema,
                  outputFolder = outputFolder)
  }

  if (runAnalyses) {
    message("Running analyses")
    runCohortMethod(connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable,
                    tempEmulationSchema = tempEmulationSchema,
                    outputFolder = outputFolder,
                    maxCores = maxCores)
  }

  if (packageResults) {
    message("Packaging results")
    exportResults(outputFolder = outputFolder,
                  databaseId = databaseId,
                  databaseName = databaseName,
                  databaseDescription = databaseDescription,
                  connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  minCellCount = minCellCount,
                  maxCores = maxCores)
  }

  invisible(NULL)
}
