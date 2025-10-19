library(FeatureExtraction)
library(dplyr)

cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(packageName = "IbdBiologics",
                                                               settingsFileName = "Cohorts.csv",
                                                               cohortFileNameValue = "cohortId")

cohortDefinitionSet$cohortId

for (j in cohortDefinitionSet$cohortId){
  IbdBiologicsCovariateSettings <- createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAge = TRUE,
    useDemographicsAgeGroup = FALSE,
    useDemographicsRace = FALSE,
    useDemographicsEthnicity = FALSE,
    useDemographicsIndexYear = TRUE,
    useDemographicsIndexMonth = FALSE,
    useDemographicsPriorObservationTime = FALSE,
    useDemographicsPostObservationTime = FALSE,
    useDemographicsTimeInCohort = FALSE,
    useDemographicsIndexYearMonth = FALSE,
    useConditionOccurrenceAnyTimePrior = FALSE,
    useConditionOccurrenceLongTerm = TRUE,
    useConditionOccurrenceMediumTerm = FALSE,
    useConditionOccurrenceShortTerm = FALSE,
    useConditionOccurrencePrimaryInpatientAnyTimePrior = FALSE,
    useConditionOccurrencePrimaryInpatientLongTerm = FALSE,
    useConditionOccurrencePrimaryInpatientMediumTerm = FALSE,
    useConditionOccurrencePrimaryInpatientShortTerm = FALSE,
    useConditionEraAnyTimePrior = FALSE,
    useConditionEraLongTerm = FALSE,
    useConditionEraMediumTerm = FALSE,
    useConditionEraShortTerm = FALSE,
    useConditionEraOverlapping = FALSE,
    useConditionEraStartLongTerm = FALSE,
    useConditionEraStartMediumTerm = FALSE,
    useConditionEraStartShortTerm = FALSE,
    useConditionGroupEraAnyTimePrior = FALSE,
    useConditionGroupEraLongTerm = FALSE,
    useConditionGroupEraMediumTerm = FALSE,
    useConditionGroupEraShortTerm = FALSE,
    useConditionGroupEraOverlapping = FALSE,
    useConditionGroupEraStartLongTerm = FALSE,
    useConditionGroupEraStartMediumTerm = FALSE,
    useConditionGroupEraStartShortTerm = FALSE,
    useDrugExposureAnyTimePrior = FALSE,
    useDrugExposureLongTerm = TRUE,
    useDrugExposureMediumTerm = FALSE,
    useDrugExposureShortTerm = FALSE,
    useDrugEraAnyTimePrior = FALSE,
    useDrugEraLongTerm = FALSE,
    useDrugEraMediumTerm = FALSE,
    useDrugEraShortTerm = FALSE,
    useDrugEraOverlapping = FALSE,
    useDrugEraStartLongTerm = FALSE,
    useDrugEraStartMediumTerm = FALSE,
    useDrugEraStartShortTerm = FALSE,
    useDrugGroupEraAnyTimePrior = FALSE,
    useDrugGroupEraLongTerm = TRUE,
    useDrugGroupEraMediumTerm = FALSE,
    useDrugGroupEraShortTerm = FALSE,
    useDrugGroupEraOverlapping = FALSE,
    useDrugGroupEraStartLongTerm = FALSE,
    useDrugGroupEraStartMediumTerm = FALSE,
    useDrugGroupEraStartShortTerm = FALSE,
    useProcedureOccurrenceAnyTimePrior = FALSE,
    useProcedureOccurrenceLongTerm = FALSE,
    useProcedureOccurrenceMediumTerm = FALSE,
    useProcedureOccurrenceShortTerm = FALSE,
    useDeviceExposureAnyTimePrior = FALSE,
    useDeviceExposureLongTerm = FALSE,
    useDeviceExposureMediumTerm = FALSE,
    useDeviceExposureShortTerm = FALSE,
    useMeasurementAnyTimePrior = FALSE,
    useMeasurementLongTerm = FALSE,
    useMeasurementMediumTerm = FALSE,
    useMeasurementShortTerm = FALSE,
    useMeasurementValueAnyTimePrior = TRUE,
    useMeasurementValueLongTerm = FALSE,
    useMeasurementValueMediumTerm = FALSE,
    useMeasurementValueShortTerm = FALSE,
    useMeasurementRangeGroupAnyTimePrior = FALSE,
    useMeasurementRangeGroupLongTerm = FALSE,
    useMeasurementRangeGroupMediumTerm = FALSE,
    useMeasurementRangeGroupShortTerm = FALSE,
    useObservationAnyTimePrior = FALSE,
    useObservationLongTerm = FALSE,
    useObservationMediumTerm = FALSE,
    useObservationShortTerm = FALSE,
    useCharlsonIndex = TRUE,
    useDcsi = FALSE,
    useChads2 = FALSE,
    useChads2Vasc = FALSE,
    useHfrs = FALSE,
    useDistinctConditionCountLongTerm = FALSE,
    useDistinctConditionCountMediumTerm = FALSE,
    useDistinctConditionCountShortTerm = FALSE,
    useDistinctIngredientCountLongTerm = FALSE,
    useDistinctIngredientCountMediumTerm = FALSE,
    useDistinctIngredientCountShortTerm = FALSE,
    useDistinctProcedureCountLongTerm = FALSE,
    useDistinctProcedureCountMediumTerm = FALSE,
    useDistinctProcedureCountShortTerm = FALSE,
    useDistinctMeasurementCountLongTerm = FALSE,
    useDistinctMeasurementCountMediumTerm = FALSE,
    useDistinctMeasurementCountShortTerm = FALSE,
    useDistinctObservationCountLongTerm = FALSE,
    useDistinctObservationCountMediumTerm = FALSE,
    useDistinctObservationCountShortTerm = FALSE,
    useVisitCountLongTerm = FALSE,
    useVisitCountMediumTerm = FALSE,
    useVisitCountShortTerm = FALSE,
    useVisitConceptCountLongTerm = FALSE,
    useVisitConceptCountMediumTerm = FALSE,
    useVisitConceptCountShortTerm = FALSE,
    longTermStartDays = -365,
    mediumTermStartDays = -180,
    shortTermStartDays = -30,
    endDays = 0,
    includedCovariateConceptIds = c(),
    addDescendantsToInclude = FALSE,
    excludedCovariateConceptIds = c(),
    addDescendantsToExclude = FALSE,
    includedCovariateIds = c()
  )

  covariates <- getDbCovariateData(connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                   cohortTable = cohortTable,
                                   cohortId = c(j),
                                   covariateSettings = IbdBiologicsCovariateSettings)

  covariates$covariates <- covariates$covariates %>% left_join(covariates$covariateRef)

  statisticsPooled <- covariates$covariates %>%
    group_by(.data$covariateId) %>%
    summarise(conceptId = .data$conceptId,
              sum = sum(as.numeric(.data$covariateValue), na.rm = TRUE),
              mean = mean(as.numeric(.data$covariateValue), na.rm = TRUE),
              sumSqr = sum(as.numeric(.data$covariateValue)^2, na.rm = TRUE),
              median = median(as.numeric(.data$covariateValue), na.rm = TRUE),
              n = n(),
              min = min(as.numeric(.data$covariateValue), na.rm = TRUE),
              max = max(as.numeric(.data$covariateValue), na.rm = TRUE)
    ) %>%
    mutate(sd = sqrt((.data$sumSqr - (.data$sum^2 / .data$n)) / .data$n))

  df <- data.frame(label = c("Age",  "Gender: female", "Weight", "C-reactive protein", "Albumin", "Hemoglobin", "Platelets", "Fecal calprotectin",
                             "Aminosalicylates", "Corticosteroids"),
                   analysisId = c(2, 1, 705, 705, 705, 705, 705, 705, 302, 302),
                   covariateIds = c(1002, 8532001, 3025315705, 4208414705, 3024561705, 3000963705, 3007461705, 3048689705, 21600662302, 21602728302),
                   conceptIds = c(NA, 8532, 3025315, 3020460, 3024561, 3000963, 3007461, 3048689, 21600662, 21602728))
  df$mean <- NA
  df$sd <- NA
  df$n <- NA

  statisticsPooled <- data.frame(statisticsPooled)

  for (i in 1){
    covariateId <- as.numeric(df$covariateIds[i])
    t <- statisticsPooled[statisticsPooled$covariateId == covariateId,]
    df$mean[i] <- round(t$mean, 2)
    df$sd[i] <- t$sd
    df$n[i] <- t$n
  }

  for (i in c(3, 4, 5, 6, 7, 8)){
    conceptId <- as.numeric(df$conceptIds[i])
    t <- statisticsPooled[statisticsPooled$conceptId == conceptId,]
    df$mean[i] <- round(t$mean, 2)
    df$sd[i] <- t$sd
    df$n[i] <- t$n
  }

  for (i in c(2, 9, 10)){
    conceptId <- as.numeric(df$conceptIds[i])
    t <- statisticsPooled[statisticsPooled$conceptId == conceptId,]
    df$mean[i] <- paste0(t$n, " (", round(t$n/df$n[1], 4)*100, "%", ")")
    df$sd[i] <- t$sd
    df$n[i] <- t$n
  }

  print(df)
  write.csv(df, paste0(file.path(outputFolder, j), ".csv"))
}
