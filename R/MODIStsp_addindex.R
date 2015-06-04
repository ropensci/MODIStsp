 #' MODIStsp_addindex
 #' @description Function used to add a user-defined Spectral Index to the default list of computable spectral indexes
 #'
 #' @details The function asks the user to provide the info related to the new desired Spectral Index using a GUI interface,
 #' checks for correctness of provided information (e.g., correct bandnames, computable formula, etc...). If the index is legit,
 #' it modifies the MODIStsp_Previous.RData file so to allow computation of the additional index within MODIStsp.
 #' To remove all custom-added spectral indexes, simply delete the MODIStsp_Previous.RData file within the /Extdata subfolder of the
 #' folder in which the package was installed
 #'
 #' @param Previous_File string path to the MODIStsp_Previous.RData file
 #'
 #' @return NULL - the MODIStsp_Previous.RData file is modified so to allow computation of the additional index
 #'
 #' @author Lorenzo Busetto, phD (2015)
 #' email: busetto.l@@irea.cnr.it
 #'
 #' license GPL >3


 # xml_file = file.choose()

  MODIStsp_addindex = function() {

		MODIStsp_dir = system.file(package = "MODIStsp")
		previous_dir = file.path(MODIStsp_dir,'Previous')
		previous_file = file.path(previous_dir, 'MODIStsp_Previous.RData')
		if(file.exists(Previous_File)){load(Previous_File)} else {stop}
		xml_file = file.path(MODIStsp_dir, '/ExtData/MODIStsp_ProdOpts.xml')

  	# Here goes the GUI !!!!

  	main_win <- gbasicdialog(title = "Insert the new Spectral Index information and formula", parent=NULL, do.buttons=F,
  			visible = T, spacing = 10)
  	main_group <- ggroup(container = main_win, horizontal = FALSE, expand = T)

  	indexbandname_group = ggroup(container = main_group, horizontal = T, expand = T)
  	indexbandname_label = glabel (text = '<span weight = "bold">	Spectral Index Acronym (e.g., NDVI) </span>', markup = T, container=indexbandname_group)
  	sel_indexbandname <- gedit(label = "Please Insert a valid Proj4 string				", container=indexbandname_group, size = 800, horizontal = T)

  	indexbandfullname_group = ggroup(container = main_group, horizontal = T, expand = T)
  	indexbandfullname_label = glabel (text = '<span weight = "bold">	Spectral Index Full Name (e.g., Simple Ratio (b1_NIR/b2_Red) )</span>', markup = T, container=indexbandfullname_group)
  	sel_indexbandfullname <- gedit(container=indexbandfullname_group, size = 800, horizontal = T)

  	indexformula_group = ggroup(container = main_group, horizontal = T, expand = T)
  	indexformula_label = glabel (text = '<span weight = "bold">	Spectral Index Formula (e.g., (b1_NIR/b2_Red) ) </span>', markup = T, container=indexformula_group)
  	sel_indexformula <- gedit(container=indexformula_group, size = 800, horizontal = T)

  	but_group <- ggroup(container = main_group, horizontal = TRUE)

  	start_but <- gbutton(text = 'Add Index', container = but_group, handler = function (h,...) {# If "Start" pressed, retrieve selected values and save in previous file

  				new_indexbandname = svalue(sel_indexbandname)
  				new_indexfullname = svalue(sel_indexbandfullname)
  				new_indexformula = svalue(sel_indexformula)
  				new_indexnodata_out = '32767'
  				print(new_indexbandname)
  #				browser()
  # Look for valid band names in index formula
  				refbands_names = c('b1_Red','b2_NIR','b3_Blue','b4_Green','b5_SWIR','b6_SWIR', 'b7_SWIR')
  				req_bands = c(str_detect(new_indexformula,'b1_Red'),
  						str_detect(new_indexformula,'b2_NIR'),
  						str_detect(new_indexformula,'b3_Blue'),
  						str_detect(new_indexformula,'b4_Green'),
  						str_detect(new_indexformula,'b5_SWIR'),
  						str_detect(new_indexformula,'b6_SWIR'),
  						str_detect(new_indexformula,'b7_SWIR'))

  # verify if string is computable
#browser()
  				if (req_bands[1] == T) {b1_Red = 5}
  				if (req_bands[2] == T) {b2_NIR = 6}
  				if (req_bands[3] == T) {b3_Blue = 7}
  				if (req_bands[4] == T) {b4_Green = 8}
  				if (req_bands[5] == T) {b5_SWIR = 9}
  				if (req_bands[6] == T) {b6_SWIR = 15}
  				if (req_bands[7] == T) {b7_SWIR = 25}

  				catch_err = 0
  				if (max(req_bands == 1)) {
  					try_parse = tryCatch (eval(parse(text = new_indexformula)))
  					if (class(try_parse) =='try-error') {catch_err = 1}
  				} else {catch_err = 1}#End If on max(req_bands == 1)

  # If formula is good, parse the XML file and add the new index entry to the products
  # for which the formula is computable (i.e., they have the required bands)

  				if (catch_err == 0) {

  					xmlfile=xmlParse(xml_file)  # initialize xml parsing

  					xmltop = xmlRoot(xmlfile) #gives content of root
  					class(xmltop)
  					top_name = xmlName(xmltop) #give name of node

  					n_products = xmlSize(xmltop) #how many product available ? = elements in root
  					prodnames = NULL
  					for (prod in 1:n_products) {  # cycle on available products
							print(prod)
  						nbands = xmlSize(xmltop[[prod]][["bands"]])  # number of original layers
  						bandnames = NULL ; 	band_fullname = NULL ;	datatype = NULL ; 	nodata_in = NULL ; 	nodata_out = NULL
  						for (band in 1:nbands) { # get bandnames of available products
  							bandnames = c(bandnames,xmlToList(xmltop[[prod]][['bands']][[band]][["bandname"]]))
  						} #End Cycle on band

  						# check if bands required for index computation are avilable for the product
  						check = 0
#  						browser()
  						for (reqband in refbands_names[which(req_bands == T)]){
								bandexists = grep(reqband, bandnames)
  							if (length(bandexists) != 0 ) {check = check + 1}
  						} #End Cycle on reqband

  						# if all required bands are available in product, add the new index to the indexes list for the product in the previous_opts file.
  						# in this way, at next execution, the new index should be available. Moreover, loading and use of old RData options files
  						# won't be broken if an index is added later than their creation.
#							if (check != 0) {
							n_req_bands = sum(req_bands)
  						if (n_req_bands == check ) {
  							prod_opt_list[[prod]]$indexes_bandnames = c(prod_opt_list[[prod]]$indexes_bandnames,new_indexbandname)
  							prod_opt_list[[prod]]$indexes_fullnames = c(prod_opt_list[[prod]]$indexes_fullnames,new_indexfullname)
  							prod_opt_list[[prod]]$indexes_formulas = c(prod_opt_list[[prod]]$indexes_formulas,new_indexformula)
  							prod_opt_list[[prod]]$indexes_nodata_out = c(prod_opt_list[[prod]]$indexes_nodata_out,32767)
#  						}
							}

  					}  #End Cycle on products

  # Save the products list and the chars of the products in previous file
  				save(prod_opt_list, mod_prod_list, file= previous_file)
  				gmessage ('The new Spectral Index was correctly added')
  				dispose(main_win)
  				} else { gmessage ('The formula of the new index is not computable. Please check it (Valid band names are: b1_Red, b2_NIR, br_Blue, b4_Greeen, b5_SWIR, b6_SWIR and B7_SWIR')}
  			})

  	quit_but <- gbutton(text = 'Quit', container = but_group, handler = function(h,...){ # If "Quit", set "Quit to T and exit
  				dispose(main_win)
  			})


  	visible(main_win, set = T)





  # xml_file = 'D:/Documents/Source_Code/R/LB_MOD_DWL/Extdata/MODIStsp_ProdOpts_Prova.xml'

  }

