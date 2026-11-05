# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L111.rsrc_fos_Prod
#'
#' Calculate historical fossil energy production and fossil resource supply curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.Prod_EJ_R_F_Yh}, \code{L111.RsrcCurves_EJ_R_Ffos}. \code{L210.DeleteRsrcTradBio}. The corresponding file in the
#' original data system was \code{LA111.rsrc_fos_Prod.R} (energy level1).
#' @details For historical fossil energy production, determine regional shares of production for each primary fuel,
#' interpolate unconventional oil production to all historical years, deduct unconventional oil from total oil,
#' and include it in calibrated production table.
#' For fossil resource supply curves, downscale GCAM3.0 supply curves to the country level (on the basis of
#' resource production) and aggregate by the new GCAM regions, using crude oil production shares as a proxy
#' for unconventional oil resources.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete replace_na
#' @author BBL August 2017
module_energy_L111.rsrc_fos_Prod <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "common/GCAM32_to_PAC",
             FILE = "energy/mappings/IEA_product_rsrc",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             FILE = "energy/rsrc_unconv_oil_prod_bbld",
             FILE = "energy/A11.fos_curves",
             "L100.IEA_en_bal_ctry_hist",
             "L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
             "L1012.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.Prod_EJ_R_F_Yh",
             "L111.RsrcCurves_EJ_R_Ffos",
             "L210.DeleteRsrcTradBio"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    sector <- fuel <- year <- value <- share <- iso <- GCAM_region_ID <- unconventionals <- value.x <-
      value.y <- FLOW <- PRODUCT <- resource <- region_GCAM3 <- CumulSum <- subresource <- grade <-
      available <- available_region_GCAM3 <- extractioncost <- technology <- GCAM32_region_ID <-
      production_EJ <- parent_production_EJ <- child_count <- parent_available <- child_available <-
      difference <- . <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <-get_data(all_data, "common/GCAM_region_names")
    GCAM32_to_PAC <- get_data(all_data, "common/GCAM32_to_PAC")
    IEA_product_rsrc <- get_data(all_data, "energy/mappings/IEA_product_rsrc")
    end_use_fuel_agg <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    rsrc_unconv_oil_prod_bbld <- get_data(all_data, "energy/rsrc_unconv_oil_prod_bbld")
    A11.fos_curves <- get_data(all_data, "energy/A11.fos_curves")
    L100.IEA_en_bal_ctry_hist <- get_data(all_data, "L100.IEA_en_bal_ctry_hist", strip_attributes = TRUE)
    L101.en_bal_EJ_ctry_Si_Fi_Yh_full <- get_data(all_data, "L101.en_bal_EJ_ctry_Si_Fi_Yh_full", strip_attributes = TRUE)
    L1012.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)

    # ------- HISTORICAL FOSSIL ENERGY PRODUCTION

    # (lines 38-56 in original file)
    # NOTE: Regional production is derived for each fuel as global TES times regional share of global production
    # Determine global total primary energy supply (TES) for each fuel
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == energy.TPES_FLOW, fuel %in% energy.RSRC_FUELS, year %in% HISTORICAL_YEARS) %>%
      group_by(sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L111.TES_EJ_F_Yh

    # Determine regional shares of production for each primary fuel
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "out_resources", fuel %in% energy.RSRC_FUELS, year %in% HISTORICAL_YEARS) ->
      L111.Prod_EJ_R_F_Yh_IEA

    L111.Prod_EJ_R_F_Yh_IEA %>%
      group_by(sector, fuel, year) %>%
      mutate(share = value / sum(value)) %>%
      select(-value) ->
      L111.Prod_share_R_F_Yh

    # Multiply through to calculate production by fuel
    L111.Prod_EJ_R_F_Yh_IEA %>%
      select(-value) %>%
      left_join_error_no_match(select(L111.TES_EJ_F_Yh, fuel, year, value), by = c("fuel", "year")) %>%
      left_join_error_no_match(L111.Prod_share_R_F_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = value * share) %>%
      select(-share) ->
      L111.Prod_EJ_R_F_Yh_IEA_adj

    # Determine unconventional oil production (58-67)
    rsrc_unconv_oil_prod_bbld %>%
      gather_years %>%
      # interpolate production to all historical years
      complete(iso, year = HISTORICAL_YEARS) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      arrange(iso, year) %>%
      group_by(iso) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      # summarise by GCAM region and convert to EJ/yr
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value) * CONV_MBLD_EJYR) %>%
      # make unconventional oil a technology within crude oil
      mutate(sector = "out_resources",
             fuel = "crude oil",
             technology = "unconventional oil") ->
      L111.Prod_EJ_ctry_unconvOil_Yh

    # Subtract the unconventional oil, append the unconventional oil to the table, and write it out
    # Deduct unconventional oil from total oil and include unconventional oil in calibrated production table
    # Start by making an 'unconventionals' flag to easily split data
    L111.Prod_EJ_R_F_Yh_IEA_adj %>%
      mutate(unconventionals = GCAM_region_ID %in% L111.Prod_EJ_ctry_unconvOil_Yh$GCAM_region_ID &
               fuel == "refined liquids") ->
      L111.Prod_EJ_R_F_Yh

    L111.Prod_EJ_R_F_Yh %>%
      filter(unconventionals) %>%
      left_join_error_no_match(select(L111.Prod_EJ_ctry_unconvOil_Yh, GCAM_region_ID, year, value),
                               by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value.x - value.y) %>%
      bind_rows(filter(L111.Prod_EJ_R_F_Yh, !unconventionals)) %>%
      select(-value.x, -value.y, -unconventionals) %>%
      # switch the names from final to primary
      mutate(fuel = if_else(fuel == "refined liquids", "crude oil", fuel),
             fuel = if_else(fuel == "gas", "natural gas", fuel),
             technology = fuel) %>%
      bind_rows(select(L111.Prod_EJ_ctry_unconvOil_Yh, GCAM_region_ID, sector, fuel, year, value, technology)) -> L111.Prod_EJ_R_F_Yh


    # -------
    # Some regions do not have traditional biomass in MODEL historical years
    # Delete those resources to avoid errors from the dependency_finder_log
    L210.DeleteRsrcTradBio<-L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(year %in% MODEL_BASE_YEARS,
             fuel == "biomass_tradbio") %>%
      left_join_error_no_match(end_use_fuel_agg %>% select(fuel,bld), by = "fuel") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(resource = bld) %>%
      select(-bld,-fuel) %>%
      group_by(region,resource)%>%
      summarise(value=sum(value)) %>%
      ungroup() %>%
      filter(value==0) %>%
      select(LEVEL2_DATA_NAMES[["DeleteRsrc"]])

    # -------

    # Produce outputs
    L111.Prod_EJ_R_F_Yh %>%
      add_title("Historical fossil energy production") %>%
      add_units("EJ") %>%
      add_comments("Determine regional shares of production for each primary fuel, ") %>%
      add_comments("interpolate unconventional oil production to all historical years, ") %>%
      add_comments("deduct unconventional oil from total oil and include it in calibrated production table.") %>%
      add_legacy_name("L111.Prod_EJ_R_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/rsrc_unconv_oil_prod_bbld") ->
      L111.Prod_EJ_R_F_Yh

    L210.DeleteRsrcTradBio %>%
      add_title("Delete sectors with no TradBio production in model base years") %>%
      add_units("unitless") %>%
      add_comments("Regions with no Primary Solid Fuels according to IEA balances") %>%
      add_legacy_name("L210.DeleteRsrcTradBio") %>%
      add_precursors("common/GCAM_region_names", "L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/mappings/enduse_fuel_aggregation") ->
      L210.DeleteRsrcTradBio

    # ------- RESOURCE PRICES

    # Fossil resource historical prices are currently assumed. No level1 processing is needed.


    # ------- FOSSIL RESOURCE SUPPLY CURVES

    # Using supply curves from GCAM 3.0 (same as MiniCAM) (83-93)
    # These need to be downscaled to the country level (on the basis of resource production) and then
    # aggregated by the new GCAM regions. This requires that all regions have the same price points

    # L100.IEA_en_bal_ctry_hist might be null (meaning the data system is running
    # without the proprietary IEA data files). If this is the case, we substitute
    # pre-built output datasets and exit.
    if(is.null(L100.IEA_en_bal_ctry_hist)) {
      # Proprietary IEA energy data are not available, so used saved outputs
      # Reallocate only the parents that have been split in the active PAC map.
      # This follows the original L111 production-share method while preserving
      # the legacy parent curve total by resource, subresource, and grade.
      L111.RsrcCurves_EJ_R_Ffos_prebuilt <- extract_prebuilt_data("L111.RsrcCurves_EJ_R_Ffos")
      L111.PAC_parent_map <- GCAM32_to_PAC %>%
        dplyr::select(GCAM_region_ID, GCAM32_region_ID) %>%
        dplyr::distinct()
      # A target can cross old parent boundaries (Australia_NZ includes cck/cxr).
      # Split only that target's production between its historical parents using
      # country production; never count its entire production in both parents.
      L111.PAC_country_prod <- L101.en_bal_EJ_ctry_Si_Fi_Yh_full %>%
        dplyr::filter(sector == "out_resources", fuel %in% energy.RSRC_FUELS,
                      year %in% HISTORICAL_YEARS) %>%
        dplyr::select(iso, fuel, value) %>%
        left_join_error_no_match(
          dplyr::select(GCAM32_to_PAC, iso, GCAM_region_ID, GCAM32_region_ID), by = "iso"
        ) %>%
        dplyr::mutate(fuel = dplyr::recode(fuel, "refined liquids" = "crude oil", "gas" = "natural gas")) %>%
        dplyr::group_by(GCAM_region_ID, GCAM32_region_ID, fuel) %>%
        dplyr::summarise(country_production_EJ = sum(value), .groups = "drop")
      L111.PAC_split_parents <- L111.PAC_parent_map %>%
        dplyr::count(GCAM32_region_ID, name = "child_count") %>%
        dplyr::filter(child_count > 1L) %>%
        dplyr::pull(GCAM32_region_ID)
      L111.PAC_prod_shares <- tidyr::crossing(
        L111.PAC_parent_map,
        fuel = c("coal", "crude oil", "natural gas")
      ) %>%
        dplyr::left_join(
          L111.Prod_EJ_R_F_Yh %>%
            dplyr::filter(fuel %in% c("coal", "crude oil", "natural gas")) %>%
            dplyr::group_by(GCAM_region_ID, fuel) %>%
            dplyr::summarise(production_EJ = sum(value), .groups = "drop"),
          by = c("GCAM_region_ID", "fuel")
        ) %>%
        dplyr::left_join(L111.PAC_country_prod,
                        by = c("GCAM_region_ID", "GCAM32_region_ID", "fuel")) %>%
        tidyr::replace_na(list(production_EJ = 0, country_production_EJ = 0)) %>%
        dplyr::group_by(GCAM_region_ID, fuel) %>%
        dplyr::mutate(
          country_fraction = if (dplyr::n() == 1L) 1 else if (sum(country_production_EJ) > 0)
            country_production_EJ / sum(country_production_EJ) else if (all(production_EJ == 0)) 0 else NA_real_,
          production_EJ = production_EJ * country_fraction
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(GCAM32_region_ID, fuel) %>%
        dplyr::mutate(
          parent_production_EJ = sum(production_EJ),
          share = dplyr::if_else(parent_production_EJ > 0, production_EJ / parent_production_EJ, NA_real_)
        ) %>%
        dplyr::ungroup()
      if (any(!is.finite((L111.PAC_prod_shares %>%
          dplyr::filter(GCAM32_region_ID %in% L111.PAC_split_parents))$share))) {
        stop("No historical production denominator for a Pacific resource-curve parent.")
      }
      L111.PAC_old_split_curve <- L111.RsrcCurves_EJ_R_Ffos_prebuilt %>%
        dplyr::filter(GCAM_region_ID %in% L111.PAC_split_parents)
      L111.PAC_split_curve <- L111.PAC_old_split_curve %>%
        dplyr::rename(GCAM32_region_ID = GCAM_region_ID) %>%
        # One parent curve must expand to one row for each Pacific child region.
        dplyr::left_join(
          dplyr::select(L111.PAC_prod_shares, GCAM_region_ID, GCAM32_region_ID, fuel, share),
          by = c("GCAM32_region_ID", "resource" = "fuel")
        )
      if (any(is.na(L111.PAC_split_curve$share))) {
        stop("No historical production share for a Pacific resource-curve child.")
      }
      L111.PAC_split_curve <- L111.PAC_split_curve %>%
        dplyr::mutate(available = available * share)
      L111.PAC_curve_check <- L111.PAC_old_split_curve %>%
        dplyr::group_by(GCAM_region_ID, resource, subresource, grade) %>%
        dplyr::summarise(parent_available = sum(available), .groups = "drop") %>%
        dplyr::rename(GCAM32_region_ID = GCAM_region_ID) %>%
        left_join_error_no_match(
          L111.PAC_split_curve %>%
            dplyr::group_by(GCAM32_region_ID, resource, subresource, grade) %>%
            dplyr::summarise(child_available = sum(available), .groups = "drop"),
          by = c("GCAM32_region_ID", "resource", "subresource", "grade")
        ) %>%
        dplyr::mutate(difference = child_available - parent_available)
      if (any(abs(L111.PAC_curve_check$difference) > 1e-10)) {
        stop("Pacific resource-curve allocation does not conserve the parent total.")
      }
      L111.RsrcCurves_EJ_R_Ffos <- dplyr::bind_rows(
        dplyr::filter(L111.RsrcCurves_EJ_R_Ffos_prebuilt, !GCAM_region_ID %in% L111.PAC_split_parents),
        dplyr::select(L111.PAC_split_curve, GCAM_region_ID, resource, subresource, grade, available, extractioncost)
      )
      if (any(L111.RsrcCurves_EJ_R_Ffos %>%
              dplyr::group_by(GCAM_region_ID, resource, subresource, grade) %>%
              dplyr::summarise(n_cost = dplyr::n_distinct(extractioncost), .groups = "drop") %>%
              dplyr::pull(n_cost) != 1L)) {
        stop("Cannot merge resource grades with different extraction costs.")
      }
      L111.RsrcCurves_EJ_R_Ffos <- L111.RsrcCurves_EJ_R_Ffos %>%
        dplyr::group_by(GCAM_region_ID, resource, subresource, grade) %>%
        dplyr::summarise(available = sum(available), extractioncost = dplyr::first(extractioncost), .groups = "drop") %>%
        dplyr::arrange(GCAM_region_ID, resource, subresource, grade) %>%
        same_attributes_as(L111.RsrcCurves_EJ_R_Ffos_prebuilt) %>%
        add_comments("Active PAC resource curves use historical production shares; cross-parent target shares use country production. Legacy resource totals are conserved; these are inherited resource assumptions.") %>%
        add_precursors("common/GCAM32_to_PAC", "L111.Prod_EJ_R_F_Yh", "L101.en_bal_EJ_ctry_Si_Fi_Yh_full")
      if (!setequal(L111.RsrcCurves_EJ_R_Ffos$GCAM_region_ID, GCAM_region_names$GCAM_region_ID)) {
        stop("Pacific resource curves do not cover exactly the active regions.")
      }
    } else {

      L100.IEA_en_bal_ctry_hist %>%
        filter(FLOW == "INDPROD", PRODUCT %in% IEA_product_rsrc$PRODUCT) %>%
        gather_years %>%
        # bring in resource information and summarise
        left_join_error_no_match(IEA_product_rsrc, by = "PRODUCT") %>%
        group_by(iso, resource) %>%
        summarise(CumulSum = sum(value)) %>%
        # calculate production shares of country within GCAM 3.0 region (95-102)
        left_join_error_no_match(select(iso_GCAM_regID, iso, region_GCAM3), by = "iso") %>%
        group_by(resource, region_GCAM3) %>%
        mutate(share = CumulSum / sum(CumulSum)) %>%   # share of iso within region
        ungroup %>%
        select(iso, resource, share) ->
        L111.Prod_share_ctry_F_Yh

      # Downscale available resources by GCAM 3.0 region to countries (104-116)
      tidyr::crossing(iso = L111.Prod_share_ctry_F_Yh$iso,
                      subresource = A11.fos_curves$subresource) %>%
        left_join_keep_first_only(select(A11.fos_curves, resource, subresource), by = "subresource") %>%
        repeat_add_columns(tibble(grade = unique(A11.fos_curves$grade))) %>%
        # remove non-existent grades
        filter(paste(subresource, grade) %in% paste(A11.fos_curves$subresource, A11.fos_curves$grade)) %>%
        # match in GCAM3 region, along with available resources
        left_join_error_no_match(select(iso_GCAM_regID, iso, region_GCAM3), by = "iso") %>%
        left_join_error_no_match(select(A11.fos_curves, region_GCAM3, subresource, grade, available_region_GCAM3 = available),
                                 by = c("region_GCAM3", "subresource", "grade")) %>%
        # match in the share of resource available by each country within GCAM 3.0 region
        # there are NAs here, so use left_join
        left_join(L111.Prod_share_ctry_F_Yh, by = c("iso", "resource")) ->
        L111.RsrcCurves_EJ_ctry_Ffos

      # Use crude oil production shares as a proxy for unconventional oil resources (123-130)
      L111.RsrcCurves_EJ_ctry_Ffos %>%
        filter(subresource == "crude oil") %>%
        select(iso, share) ->
        crude

      L111.RsrcCurves_EJ_ctry_Ffos %>%
        filter(subresource == "unconventional oil") %>%
        select(-share) %>%
        left_join_keep_first_only(crude, by = "iso") %>%
        bind_rows(filter(L111.RsrcCurves_EJ_ctry_Ffos, subresource != "unconventional oil")) %>%
        # set all other missing values to 0 (these are small countries)
        replace_na(list(share = 0)) %>%
        mutate(available = available_region_GCAM3 * share) ->
        L111.RsrcCurves_EJ_ctry_Ffos

      # Aggregate by GCAM regions (132-141)
      L111.RsrcCurves_EJ_ctry_Ffos %>%
        left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
        group_by(GCAM_region_ID, resource, subresource, grade) %>%
        summarise(available = sum(available)) %>%
        ungroup %>%
        left_join_keep_first_only(select(A11.fos_curves, resource, subresource, grade, extractioncost),
                                  by = c("resource", "subresource", "grade")) ->
        L111.RsrcCurves_EJ_R_Ffos

      L111.RsrcCurves_EJ_R_Ffos %>%
        add_title("Fossil resource supply curves", overwrite = TRUE) %>%
        add_units("available: EJ; extractioncost: 1975$/GJ") %>%
        add_comments("Downscale GCAM3.0 supply curves to the country level (on the basis of resource") %>%
        add_comments("production) and aggregate by the new GCAM regions.") %>%
        add_comments("Use crude oil production shares as a proxy for unconventional oil resources.") %>%
        add_legacy_name("L111.RsrcCurves_EJ_R_Ffos") %>%
        add_precursors("common/iso_GCAM_regID", "energy/A11.fos_curves",
                       "energy/mappings/IEA_product_rsrc", "L100.IEA_en_bal_ctry_hist",
                       "L1012.en_bal_EJ_R_Si_Fi_Yh") ->
        L111.RsrcCurves_EJ_R_Ffos
    }

    return_data(L111.Prod_EJ_R_F_Yh, L111.RsrcCurves_EJ_R_Ffos,L210.DeleteRsrcTradBio)
  } else {
    stop("Unknown command")
  }
}
