#### my streamlined helper function version ####
# F:  minor style issues:
#  - I prefer to have any variable inside a function to end with _var (hence I change the name in the helper below)
#  - I read somewhere that thisNotation was not good for reproducibility, but not sure why. 
 

# this is more like what I could call in Shiny
tax_revenue_helper <- function(numberTaxpayers_var = numberTaxpayers_mo,
                               tax_base_var = tax_base_mo,
                               brackets_var = brackets_po,
                               main_tax_var = main_tax_po){
  
  target_hhlds_mo <- sum(numberTaxpayers_var[which(main_tax_po>0)])
  
  tax_base_total_mo <- sum(tax_base_var[which(main_tax_po>0)])/1000
  tax_rev_init_mo <-   tax_base_total_mo * main_tax_var * 1000
  return(list( "target_hhlds_mo" = target_hhlds_mo,
               "tax_base_total_mo" = tax_base_total_mo,
               "tax_rev_init_mo" = tax_rev_init_mo))
}

#### tax_revenue_mo_f #### 

tax_revenue_mo_f <- function(
  cum_numberTaxpayers_dina_var = cum_numberTaxpayers_dina_in,
  cum_numberTaxpayers_scf_var = cum_numberTaxpayers_scf_in,
  num_billionares_var = num_billionares_in,
  cum_tax_base_dina_var = cum_tax_base_dina_in,
  cum_tax_base_scf_var = cum_tax_base_scf_in,
  brackets_var = brackets_po,
  starting_brack_var = starting_brack_po,
  main_tax_var = main_tax_po, 
  top_tax_base_var = top_tax_base_in){
  
  ## start with cum_numberTaxpayers
  ## calculate what you want for that outside of this function  

cum_numberTaxpayers = (cum_numberTaxpayers_dina_var + cum_numberTaxpayers_scf_var)/2

## can go from cumulative to per bin before passing to function
## num_billionares_var can be included in numberTaxpayers

numberTaxpayers_mo <- c(-1 * diff(cum_numberTaxpayers, lag = 1), num_billionares_var)

## start with cum_tax_base
## calculate what you want for that outside of this function

cum_tax_base = (cum_tax_base_dina_var + cum_tax_base_scf_var)/2

## can go from cumulative to per bin before passing to function
## you never define top_tax_base_in? (don't pass it in and don't create it in the function)
## F: great catch. It comes from the previous function (est_billionares_in_f()). 
## It should go into this function as a variable (added above)
tax_base_mo <- c(-1 * diff(cum_tax_base, lag = 1), top_tax_base_var) * 100


## don't need to return numberTaxpayers_mo and tax_base_mo since we input them
## we don't need starting_brack_var, we can recover it using brackets_var and main_tax_var
## F: not clear what you mean. Will review again tomorrow morning

helperResults = tax_revenue_helper(numberTaxpayers_mo = numberTaxpayers_mo,
                   tax_base_mo = tax_base_mo,
                   brackets_var = brackets_po,
                   main_tax_var = main_tax_po)


return(list("numberTaxpayers_mo" = numberTaxpayers_mo,
            "tax_base_mo" = tax_base_mo,
            "tax_base_dina_mo" = tax_base_dina_mo,
            "tax_base_scf_mo" = tax_base_scf_mo,
            "target_hhlds_mo" = helperResults$target_hhlds_mo,
            "tax_base_total_mo" = helperResults$tax_base_total_mo,
            "tax_rev_init_mo" = helperResults$tax_rev_init_mo))

}
invisible( list2env(tax_revenue_mo_f(),.GlobalEnv) )

#### total_rev_mo_f ####

## don't need top_tax_base_var anymore?
## so we don't need total_rev_mo_f()?

# use this instead
tax_revenue_mo_f()$tax_rev_init_mo

#### ten_years_mo_f ####

ten_years_mo_f <- function(inflation_var = inflation_so,
                           population_gr_var = population_gr_so,
                           real_growth_var = real_growth_so,
                           total_rev_var = total_rev_pe, 
                           top_tax_base_var = top_tax_base_in){
  discount_rate_mo <- inflation_var + population_gr_so + real_growth_so  
  ten_year_factor_mo <- sum( ( 1 + discount_rate_mo )^( 0:9 ) ) 
  ## is this what turns into 13?
  ## if so, I could adapt the Shiny app to fit better
  ## F: Yes
  
  ten_year_revenue_pe <- round(total_rev_pe) * ten_year_factor_mo     
  #ten_year_revenue_pe <- total_rev_pe * ten_year_factor_mo                      #PE
  ten_year_top_tax_pe <- top_tax_base_var * ten_year_factor_mo          
  #ten_year_top_tax_pe <- top_tax_base_var * ten_year_factor_mo                  #PE
  return( list("discount_rate_mo" = discount_rate_mo, "ten_year_factor_mo" = ten_year_factor_mo, 
               "ten_year_revenue_pe" = ten_year_revenue_pe, "ten_year_top_tax_pe" = ten_year_top_tax_pe) )
}

## I'll make your getGroup work in Shiny


#### matching tax revenue function ####

brackets_po <- c(0, 25, 50, 100, 250, 500, 1000) * 1e6
tax_rates_po <- c(  0,    0, 0.02,  0.02,  0.02,  0.02, 0.03) 

## Fernando's Tax Revenue 
getTaxRevenue <- function(wealth_var = wealth_aux, taxrates_var = tax_rates_po,
                          brackets_var = brackets_po) {
 # browser()
  ## expecting taxLevels in percentage
  # taxLevels <- taxLevels / 100
  if (length(brackets_var) != length(taxrates_var)){
    stop("Tax brackets and tax rates do not match")
  }
  # Compute max taxable wealth per bracket
  max_tax_per_brack <- c(diff(c(0, brackets_var)), 1e100)
  # Substract wealth minus tax bracket. If wealth above a given bracket (difference is larger than max taxable wealth), 
  # then assign max taxable wealth to that given bracket
  to_tax <- ifelse( wealth_var - c(0, brackets_var) > max_tax_per_brack, 
                    max_tax_per_brack, 
                    ( wealth_var - c(0,brackets_var) ) )   
  # If wealth if lower than a given bracket (difference between wealth and bracket is negative), then assign zero to that bracket  
  to_tax <- ifelse( to_tax<0, 0, to_tax )
  # Apply trax rates to each corresponding bracket and all together
  total_tax <- sum( to_tax * c(0, taxrates_var) )   
  return(total_tax)
}

## Sara's Tax Revenue 
getAverageTax <- function(wealth, taxLevels, brackets) {
  ## pass in brackets to make sure they update
  ## expecting taxLevels in percentage
  taxLevels <- taxLevels / 100
  first <- wealth - brackets[1] * 1e6
  second <- first - (brackets[2] * 1e6 - brackets[1] * 1e6)
  third <- second - (brackets[3] * 1e6 - brackets[2] * 1e6)
  fourth <- third - (brackets[4] * 1e6 - brackets[3] * 1e6)
  
  firstChunk <- ifelse(second >= 0, taxLevels[1] * (brackets[2] * 1e6 - brackets[1] * 1e6), taxLevels[1] * max(first, 0))
  secondChunk <- ifelse(third >= 0, taxLevels[2] * (brackets[3] * 1e6 - brackets[2] * 1e6), taxLevels[2] * max(second, 0))
  thirdChunk <- ifelse(fourth >= 0, taxLevels[3] * (brackets[4] * 1e6 - brackets[3] * 1e6), taxLevels[3] * max(third, 0))
  fourthChunk <- ifelse(fourth >= 0, fourth * taxLevels[4], 0)
browser()  
  toReturn <- firstChunk + secondChunk + thirdChunk + fourthChunk
  return(toReturn)
}


getTaxRevenue(1e8,tax_rates_po,brackets_po)
getAverageTax(1e8,tax_rates_po*100,brackets_po/1e6)

lapply(c(1e6,25e6,75e6,1100e6),getAverageTax,tax_rates_po*100,brackets_po/1e6)
lapply(c(1e6,25e6,75e6,1100e6),getTaxRevenue,tax_rates_po,brackets_po)

## 1100e6 doesn't match 
## F: The reasons is because getAverageTax reads 4 tax rates, so when we pass the rax rates from above (length 7) applies only up to 2% to the last bracket (instead of three). 

lapply(c(0, 25, 50, 100, 250, 500, 1000) * 1e6,getAverageTax,tax_rates_po*100,brackets_po/1e6)
lapply(c(0, 25, 50, 100, 250, 500, 1000) * 1e6,getTaxRevenue,tax_rates_po,brackets_po)
## these all match

lapply((10+c(0, 25, 50, 100, 250, 500, 1000)) * 1e6,getAverageTax,tax_rates_po*100,brackets_po/1e6)
lapply((10+c(0, 25, 50, 100, 250, 500, 1000)) * 1e6,getTaxRevenue,tax_rates_po,brackets_po)
## last one doesn't match
## Same as above. 

getTaxRevenue(1001e6, taxrates_var = tax_rates_po, brackets_var = brackets_po)

getAverageTax(1001e6, taxLevels = tax_rates_po*100, brackets = brackets_po/1e6)
