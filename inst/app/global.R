prod_opt_list <- load_prodopts()
prodopts_file <- system.file("ExtData", "MODIStsp_ProdOpts.RData",
                             package = "MODIStsp")
mod_prod_cat <- as.data.frame(
  t(vapply(prod_opt_list, function(x){
    c(x[[1]]$cat01, x[[1]]$cat02)
  }
  , FUN.VALUE = character(2)))
)
names(mod_prod_cat) <- c("cat01", "cat02")
mod_prod_cat$cat    <- apply(mod_prod_cat, 1, paste, collapse = " - ")
mod_prod_list <- names(prod_opt_list)

modis_grid <- get(load(system.file("ExtData/MODIS_Tiles.RData",
                       package = "MODIStsp")))


