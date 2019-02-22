#### my streamlined helper function version ####

# this is more like what I could call in Shiny
tax_revenue_helper <- function(numberTaxpayers_mo = numberTaxpayers_mo,
                               tax_base_mo = tax_base_mo,
                               brackets_var = brackets_po,
                               main_tax_var = main_tax_po){
  
  target_hhlds_mo <- sum(numberTaxpayers_mo[which(main_tax_po>0)])
  
  tax_base_total_mo <- sum(tax_base_mo[which(main_tax_po>0)])/1000
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
  main_tax_var = main_tax_po){
  
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
tax_base_mo <- c(-1 * diff(cum_tax_base, lag = 1), top_tax_base_in) * 100


## don't need to return numberTaxpayers_mo and tax_base_mo since we input them
## we don't need starting_brack_var, we can recover it using brackets_var and main_tax_var

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
