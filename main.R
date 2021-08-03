# install.packages("googlesheets4")
# install.packages("Rcpp")
# install.packages("git2r")
# devtools::install_github("tidyverse/googlesheets4")
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(aria)
library(googlesheets4)
library(remotes)
library(cmdstanr)
library(rprojroot)

# what is the difference between the three methods below???
#install.packages('/home/keane/git/GSOC/posteriordb/rpackage', repos = NULL, type="source")
#remotes::install_local('/home/keane/git/GSOC/posteriordb/rpackage')
#posteriordb::trace(model_code, edit=TRUE)
devtools::install_local('/home/keane/git/GSOC/posteriordb/rpackage')
library(posteriordb)


#### getting data from google spreadsheet
# parameters
DATA_PATH = '~/git/GSOC/GSOC/data'
MODEL_PATH = '~/git/GSOC/GSOC/model'

# database
setwd("~/git/GSOC/posteriordb") # why is this necessary?
pdbl = pdb_local()
data_names = posteriordb::data_names(pdbl)
model_names = posteriordb::model_names(pdbl)
model_names
# gather data from the spreadsheet
gs4_deauth()
df = read_sheet("https://docs.google.com/spreadsheets/d/1J5hgKqiAnM-eW9U6tkxqmST9D7OMKHH0r01sayx7BBo/edit#gid=0",
				skip = 1)
names = names(df)
names[4] = "name"
df = setNames(df, names)
df = df[df$`Existing in DB (yes/no)` == "ready",]
# process function for each matched entry
process = function(entry, posterior_sampling = FALSE, overwrite = FALSE){
	DATA_NAME = entry$`Data Name`
	MODEL_NAME = entry$`Model Name`
	POSTERIOR_NAME = entry$`name`

	# check if there are already entries
	if (DATA_NAME %in% data_names){
		warning('Data name already exists')
		if (overwrite){
			warning('Data name will be overwritten')
		}
		else{
			stop('Stop because data name already exists')
		}
	}

	if (MODEL_NAME %in% model_names){
		warning('Model name already exists')
		if (overwrite){
			warning('Model name will be overwritten')
		}
		else{
			stop('Stop because model name already exists')
		}
	}


	# data
	x <- list(name = DATA_NAME,
			  keywords = c("benchmarking_1.0"),
			  title = entry$`Data Title`,
			  description = entry$`Data Description`,
			  urls = entry$`Data urls`,
			  references = entry$`Data References`,
			  added_date = Sys.Date(),
			  added_by = "Keane Nguyen")

	di <- pdb_data_info(x)
	file_path <- paste(DATA_PATH,"/", DATA_NAME, "/data.R", sep = "")
	source(file_path, chdir = T)
	stan_data <- pdb_data(eval(parse(text = "data")), info = di)
	write_pdb(stan_data, pdbl, overwrite = overwrite)

	# model
	x <- list(name = entry$`Model Name`,
			  keywords = entry$`Model Keywords`,
			  title = entry$`Model Title`,
			  description = entry$`Model Description`,
			  urls = entry$`Model urls`,
			  framework = "stan",
			  references = entry$`Model References`,
			  added_by = "Keane Nguyen",
			  added_date = Sys.Date())
	mi <- pdb_model_info(x)

	file_path <- paste(MODEL_PATH, "/", MODEL_NAME, "/model.stan", sep = "")
	smc <- readLines(file_path)
	sm <- rstan::stan_model(model_code = smc)
	mc <- model_code(sm, info = mi)
	write_pdb(mc, pdbl, overwrite = overwrite)

	if (posterior_sampling) {
		x <- list(pdb_model_code = mc,
				  pdb_data = data,
				  keywords = "posterior_keywords",
				  urls = "posterior_urls",
				  references = "posterior_references",
				  dimensions = list("dimensions" = 2, "dim" = 3),
				  reference_posterior_name = NULL,
				  added_date = Sys.Date(),
				  added_by = "Keane Nguyen")
		po <- pdb_posterior(x, pdbl)
		check_pdb_posterior(po)
		write_pdb(po, pdbl)
	}
}

# process entry
for (i in 1:nrow(df)){
	entry = df[i,]
	print(entry$`Model Name`)
	print(entry$`Data Name`)
	try({
		process(entry, overwrite = F)
	})
}




