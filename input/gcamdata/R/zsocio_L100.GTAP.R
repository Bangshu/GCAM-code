# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# GCAM-Pacific 47-region modification: when distributed prebuilt GTAP objects
# still use 32 regions, expand their structural shares to 47 regions through an
# explicit parent map. This is a geographic fallback, not new GTAP evidence.

#' module_socio_L100.GTAP
#'
#' preprocess GTAP data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.GTAP_capital_stock}, \code{L100.GTAPCostShare_AgLU_reg_comm}.
#' @details Select national accounts data from Penn World Table for all countries.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate mutate_at select rename
#' @author xz 2025
#'
module_socio_L100.GTAP <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      FILE = "socioeconomics/GTAP/GTAP_region_mapping",
      FILE = "socioeconomics/GTAP/GTAP_sector_mapping",
      FILE = "socioeconomics/GTAP/GCAM_GTAP_sector_mapping_AgLU",
      FILE = "socioeconomics/GTAP/GTAP_region_proxy_47regions",
      OPTIONAL_FILE = "socioeconomics/GTAP/GTAPv10_basedata_VKB_SAVE_VDEP",
      OPTIONAL_FILE = "socioeconomics/GTAP/GTAPv10_baseview_SF01_VFA")

  MODULE_OUTPUTS <-
    c("L100.GTAP_capital_stock",
      "L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest",
      "L100.GTAPCostShare_AgLU_reg_comm")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # silence package checks
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- Units <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- source_region <- target_region <- source_GCAM_region_ID <-
      target_GCAM_region_ID <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ROM and ROU are source aliases for one Romania entity. Collapse the map
    # before any ISO-based coverage check so Romania is represented once.
    iso_GCAM_regID <- iso_GCAM_regID %>%
      mutate(iso = if_else(tolower(iso) == "rou", "rom", tolower(iso))) %>%
      distinct()

    # 1. Capital stock and cost shares ----
    if(!is.null(GTAPv10_baseview_SF01_VFA) && !is.null(GTAPv10_basedata_VKB_SAVE_VDEP)) {

      # GTAP data will have VKB (the value of the capital stock at the beginning of the period)
      # total annual investment and VDEP (the depreciation of the capital stock in terms of $)
      # We can use this information to back out VKE (the value of capital stock at the end of the period)
      # as simply: VKB + investment - VDEP

      ## Calculate ending capital stock VKE----
      GTAPv10_basedata_VKB_SAVE_VDEP %>%
        spread(variable, value) %>%
        left_join_error_no_match(
          GTAPv10_baseview_SF01_VFA %>%
            filter(output == "CGDS") %>% # investment
            group_by(region, year) %>%
            summarize(investment = sum(value)),
          by = c("region", "year") ) %>%
        mutate(VKE = VKB + investment - VDEP) ->
        L100.GTAP_VKB_VKE

      # convert total values to shares which is what we will use ultimately since we are
      # mixing a number of data sources
      GTAPv10_baseview_SF01_VFA %>%
        filter(input == "Capital") %>%
        left_join_error_no_match(
          L100.GTAP_VKB_VKE %>% select(region, year, VKB, VKE, VDEP), by = c("region", "year") ) %>%
        # Share out VKB and VKE by annual capital cost
        # This assumes a uniform depreciation rate & rate of return
        dplyr::group_by_at(vars(-value, -output)) %>%
        mutate(VKB = value / sum(value) * VKB,
               VKE = value / sum(value) * VKE,
               VDEP = value / sum(value) * VDEP) %>%
        ungroup() %>%
        select(-input) %>% rename(CapitalCost= value) ->
        L100.GTAP_capital_stock_0

      # Join mappings to get capital stock by sectors ----

      L100.GTAP_capital_stock_0 %>%
        rename(region_GTAP = region) %>%
        left_join_error_no_match(
          GTAP_region_mapping %>% select(region_GTAP = GTAPv10_region, region_GCAM = GCAM_region),
          by = "region_GTAP") %>%
        left_join_error_no_match(
          GTAP_sector_mapping %>%
            select(output = GTAPv10, GCAM_sector), by = "output") %>%
        group_by(region_GTAP, region_GCAM, GCAM_sector, year) %>%
        summarize_at(vars(CapitalCost, VKE, VKB, VDEP), .funs = sum) %>%
        ungroup ->
        L100.GTAP_capital_stock

      unique_regions <- unique(L100.GTAP_capital_stock$region_GTAP)
      missing_regions <- (unique((iso_GCAM_regID %>% dplyr::filter(GCAM_region_ID > 32))$iso))[
        !(unique((iso_GCAM_regID %>% dplyr::filter(GCAM_region_ID > 32))$iso)) %in% unique_regions]

      if(length(missing_regions) > 0){
        warning("WARNING: missing GTAP regions for capital stock; check the GTAP_region_mapping.csv mapping")
      }

      # regional aggregate to GCAM & drop GTAP region
      L100.GTAP_capital_stock %>%
        group_by(region = region_GCAM, GCAM_sector, year) %>%
        summarize(across(where(is.numeric), sum), .groups = "drop") ->
        L100.GTAP_capital_stock

      L100.GTAP_capital_stock %>%
        add_title("GTAP capital stock data", overwrite = TRUE) %>%
        add_units("value share") %>%
        add_comments("Processed capital stock data for separate GCAM macro capital stock based on GTAP") %>%
        add_precursors("common/iso_GCAM_regID",
                       "socioeconomics/GTAP/GTAP_region_mapping",
                       "socioeconomics/GTAP/GTAP_sector_mapping",
                       "socioeconomics/GTAP/GTAPv10_basedata_VKB_SAVE_VDEP",
                       "socioeconomics/GTAP/GTAPv10_baseview_SF01_VFA") ->
        L100.GTAP_capital_stock

      verify_identical_prebuilt(L100.GTAP_capital_stock)

    } else {
      # If missing source GTAP data, prebuilt data is read here
      L100.GTAP_capital_stock <- extract_prebuilt_data("L100.GTAP_capital_stock")
    }
    ## *L100.GTAP_capital_stock ----

    # The raw GTAP matrices are optional and are not distributed with this repository. When those source files are
    # absent, the module falls back to prebuilt 32-region GTAP structures. Expand those structural shares to the
    # current 47-region GCAM-Pacific mapping using an explicit parent-region proxy table. This does not create new
    # country-level GTAP observations; it only assigns each split region the cost-share/capital-stock structure of
    # its documented 32-region parent.
    if(length(unique(L100.GTAP_capital_stock$region)) < nrow(GCAM_region_names)) {
      GTAP_region_proxy_47regions %>%
        select(target_region, source_region) %>%
        left_join(L100.GTAP_capital_stock,
                  by = c("source_region" = "region"),
                  relationship = "many-to-many") %>%
        mutate(region = target_region) %>%
        select(region, GCAM_sector, year, CapitalCost, VKE, VKB, VDEP) ->
        L100.GTAP_capital_stock

      assert_that(
        length(unique(L100.GTAP_capital_stock$region)) == nrow(GCAM_region_names),
        msg = "GTAP capital stock proxy expansion did not produce all GCAM-Pacific regions"
      )
      assert_that(
        !any(is.na(select(L100.GTAP_capital_stock, CapitalCost, VKE, VKB, VDEP))),
        msg = "GTAP capital stock proxy expansion produced missing values"
      )

      L100.GTAP_capital_stock %>%
        add_title("GTAP capital stock data", overwrite = TRUE) %>%
        add_units("value share") %>%
        add_comments("Processed capital stock data for separate GCAM macro capital stock based on GTAP") %>%
        add_comments("Expanded from 32-region GTAP structures to 47 GCAM-Pacific regions using GTAP_region_proxy_47regions") %>%
        add_precursors("common/iso_GCAM_regID",
                       "common/GCAM_region_names",
                       "socioeconomics/GTAP/GTAP_region_proxy_47regions",
                       "socioeconomics/GTAP/GTAP_region_mapping",
                       "socioeconomics/GTAP/GTAP_sector_mapping",
                       "socioeconomics/GTAP/GTAPv10_basedata_VKB_SAVE_VDEP",
                       "socioeconomics/GTAP/GTAPv10_baseview_SF01_VFA") ->
        L100.GTAP_capital_stock
    }


    # 2. Ag cost shares ----
    if(!is.null(GTAPv10_baseview_SF01_VFA)) {

      # ensure GTAP Ag sectoral mapping is okay so sectors needed are included in the data
      assert_that(
        GCAM_GTAP_sector_mapping_AgLU %>% distinct(GTAP_sectors) %>% pull %>%
          setdiff(GTAPv10_baseview_SF01_VFA %>% distinct(output) %>% pull) %>%
          length() == 0, msg = "Mapping inconsistency"
      )

      GTAP_AgLU_sector <-
        GCAM_GTAP_sector_mapping_AgLU %>%
        distinct(output = GTAP_sectors)

      GTAPv10_baseview_SF01_VFA %>%
        # Keep relevant AgLU sectors only
        right_join(GTAP_AgLU_sector, by = "output") %>%
        # aggregate input sectors for simplicity (GTAP has more inputs)
        left_join_error_no_match(
          GTAP_sector_mapping %>%
            select(input = GTAPv10, input1 = GCAM_sector),
          by = "input") %>%
        group_by(region, year, sector = output, input = input1) %>%
        summarize(value = sum(value), .groups = "drop") ->
        L100.GTAPCostShare_MilUSD_AgLU

      # Aggregate to GCAM regions
      L100.GTAPCostShare_MilUSD_AgLU %>%
        rename(region_GTAP = region) %>%
        left_join_error_no_match(
          GTAP_region_mapping %>%
            select(region_GTAP = GTAPv10_region, region_GCAM = GCAM_region),
          by = "region_GTAP") %>%
        left_join_error_no_match(
          GCAM_region_names %>% select(region_GCAM = region, GCAM_region_ID),
          by = "region_GCAM") %>%
        group_by(GCAM_region_ID, year, sector, input) %>%
        summarize(value = sum(value), .groups = "drop") ->
        L100.GTAPCostShare_MilUSD_AgLU_R

      # Map to GCAM sectors and Calculate shares
      GCAM_GTAP_sector_mapping_AgLU %>%
        select(GCAM_commodity, sector = GTAP_sectors, Primary) %>%
        # the mapping is one-way and row number will change (not using LJENM)
        # only cost shares are useful!
        left_join(
          L100.GTAPCostShare_MilUSD_AgLU_R, by = "sector",
          relationship = "many-to-many") %>%
        # for sectors with processing (e.g., two gtap sectors were mapped to beef),
        # separating primary and processed regarding factor inputs
        mutate(input = if_else(Primary == F,
                               paste0(input, "_Proc"), input) ) %>%
        group_by(GCAM_region_ID, GCAM_commodity, input, year) %>%
        summarize(value = sum(value), .groups = "drop") %>%
        group_by(GCAM_region_ID, year, GCAM_commodity) %>%
        mutate(value = value / sum(value) * 100) %>% ungroup() ->
        L100.GTAPCostShare_AgLU_reg_comm

      L100.GTAPCostShare_AgLU_reg_comm %>%
        add_title("GCAM AgLU sector cost share", overwrite = TRUE) %>%
        add_units("share") %>%
        add_legacy_name("L100.GTAPCostShare_AgLU_reg_comm") %>%
        add_comments("Note that for livestock sectors, primary and secondary are aggregated while labor and capital are distinguished") %>%
        add_precursors("common/GCAM_region_names",
                       "socioeconomics/GTAP/GTAP_region_mapping",
                       "socioeconomics/GTAP/GTAP_sector_mapping",
                       "socioeconomics/GTAP/GCAM_GTAP_sector_mapping_AgLU",
                       "socioeconomics/GTAP/GTAPv10_baseview_SF01_VFA") ->
        L100.GTAPCostShare_AgLU_reg_comm

      verify_identical_prebuilt(L100.GTAPCostShare_AgLU_reg_comm)


      # Calculate labor and capital share across all primary Ag sectors ----
      GCAM_GTAP_sector_mapping_AgLU %>%
        filter(GCAM_commodity %in% c("OtherMeat_Fish", "Forest")) %>%
        distinct(sector = GTAP_sectors, GCAM_commodity) ->
        GTAP_Mapping_Fish_For

      L100.GTAPCostShare_MilUSD_AgLU_R %>%
        # keep only primary sectors
        inner_join(
          GCAM_GTAP_sector_mapping_AgLU %>%
            filter(Primary == T) %>%
            distinct(sector = GTAP_sectors),
          by = "sector") %>%
        filter(input %in% c("Labor", "Capital")) %>%
        group_by(GCAM_region_ID, year, input) %>%
        mutate(share = value / sum(value) * 100) %>% ungroup %>%
        # keep only fish and forest sectors
        inner_join(GTAP_Mapping_Fish_For, by = "sector") ->
        L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest
      # e.g., 3% fish labor share means 3% of primary Ag labor in a region is in the fishing sector

      L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest %>%
        add_title("GTAP labor and captial share in primary agriculture for fish and forestry sectors", overwrite = TRUE) %>%
        add_units("share") %>%
        add_legacy_name("L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest") %>%
        add_comments("The factor shares derived here will complement ILO data to derive the full data set in physical units") %>%
        add_precursors("common/GCAM_region_names",
                       "socioeconomics/GTAP/GTAP_region_mapping",
                       "socioeconomics/GTAP/GTAP_sector_mapping",
                       "socioeconomics/GTAP/GCAM_GTAP_sector_mapping_AgLU",
                       "socioeconomics/GTAP/GTAPv10_baseview_SF01_VFA") ->
        L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest

      verify_identical_prebuilt(L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest)


    } else {
      # If missing source GTAP data, prebuilt data is read here
      L100.GTAPCostShare_AgLU_reg_comm <- extract_prebuilt_data("L100.GTAPCostShare_AgLU_reg_comm")
      L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest <-
        extract_prebuilt_data("L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest")

    }

    if(length(unique(L100.GTAPCostShare_AgLU_reg_comm$GCAM_region_ID)) < nrow(GCAM_region_names)) {
      GTAP_region_proxy_47regions %>%
        select(target_GCAM_region_ID, source_GCAM_region_ID) %>%
        left_join(L100.GTAPCostShare_AgLU_reg_comm,
                  by = c("source_GCAM_region_ID" = "GCAM_region_ID"),
                  relationship = "many-to-many") %>%
        mutate(GCAM_region_ID = target_GCAM_region_ID) %>%
        select(GCAM_region_ID, GCAM_commodity, input, year, value) ->
        L100.GTAPCostShare_AgLU_reg_comm

      assert_that(
        length(unique(L100.GTAPCostShare_AgLU_reg_comm$GCAM_region_ID)) == nrow(GCAM_region_names),
        msg = "GTAP AgLU cost-share proxy expansion did not produce all GCAM-Pacific regions"
      )
      assert_that(
        !any(is.na(L100.GTAPCostShare_AgLU_reg_comm$value)),
        msg = "GTAP AgLU cost-share proxy expansion produced missing values"
      )

      L100.GTAPCostShare_AgLU_reg_comm %>%
        add_title("GCAM AgLU sector cost share", overwrite = TRUE) %>%
        add_units("share") %>%
        add_legacy_name("L100.GTAPCostShare_AgLU_reg_comm") %>%
        add_comments("Note that for livestock sectors, primary and secondary are aggregated while labor and capital are distinguished") %>%
        add_comments("Expanded from 32-region GTAP structures to 47 GCAM-Pacific regions using GTAP_region_proxy_47regions") %>%
        add_precursors("common/GCAM_region_names",
                       "socioeconomics/GTAP/GTAP_region_proxy_47regions",
                       "socioeconomics/GTAP/GTAP_region_mapping",
                       "socioeconomics/GTAP/GTAP_sector_mapping",
                       "socioeconomics/GTAP/GCAM_GTAP_sector_mapping_AgLU",
                       "socioeconomics/GTAP/GTAPv10_baseview_SF01_VFA") ->
        L100.GTAPCostShare_AgLU_reg_comm
    }

    if(length(unique(L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest$GCAM_region_ID)) < nrow(GCAM_region_names)) {
      GTAP_region_proxy_47regions %>%
        select(target_GCAM_region_ID, source_GCAM_region_ID) %>%
        left_join(L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest,
                  by = c("source_GCAM_region_ID" = "GCAM_region_ID"),
                  relationship = "many-to-many") %>%
        mutate(GCAM_region_ID = target_GCAM_region_ID) %>%
        select(GCAM_region_ID, year, sector, input, value, share, GCAM_commodity) ->
        L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest

      assert_that(
        length(unique(L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest$GCAM_region_ID)) == nrow(GCAM_region_names),
        msg = "GTAP fish/forest labor-capital proxy expansion did not produce all GCAM-Pacific regions"
      )
      assert_that(
        !any(is.na(select(L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest, value, share))),
        msg = "GTAP fish/forest labor-capital proxy expansion produced missing values"
      )

      L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest %>%
        add_title("GTAP labor and captial share in primary agriculture for fish and forestry sectors", overwrite = TRUE) %>%
        add_units("share") %>%
        add_legacy_name("L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest") %>%
        add_comments("The factor shares derived here will complement ILO data to derive the full data set in physical units") %>%
        add_comments("Expanded from 32-region GTAP structures to 47 GCAM-Pacific regions using GTAP_region_proxy_47regions") %>%
        add_precursors("common/GCAM_region_names",
                       "socioeconomics/GTAP/GTAP_region_proxy_47regions",
                       "socioeconomics/GTAP/GTAP_region_mapping",
                       "socioeconomics/GTAP/GTAP_sector_mapping",
                       "socioeconomics/GTAP/GCAM_GTAP_sector_mapping_AgLU",
                       "socioeconomics/GTAP/GTAPv10_baseview_SF01_VFA") ->
        L100.GTAP_LaborCapitalCostShare_PrimaryAg_reg_Fish_Forest
    }

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
