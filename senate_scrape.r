#scrape wiki for US congress info

#### get 50 state names
###############################################

# read 'simple.wikipedia.org' page with 50 states
states_wiki <- readLines("http://simple.wikipedia.org/wiki/List_of_U.S._states")

# exctract "<li><b>AL</b>... " opening - note 'AL' for Alabama
# take 50 states and exclude places like American Samoa, Guam & Puerto Rico
state_shortlist <- grep("(?<=^<li><b>)[A-Z]{2}(?=</b>)", states_wiki, perl = TRUE, value = TRUE)[1:50]

# use regexpr() to get state (+abbrev.) character placement in each line
shortlist_regexpr <- regexpr("(?<=href=\\\"/wiki/).+?(?=\\\" title)", state_shortlist, perl = TRUE) 
shortlist_abbrev_regexpr <- regexpr("(?<=^<li><b>)[A-Z]{2}(?=</b>)", state_shortlist, perl = TRUE)

# create vectors of start & end for each string (+ abbrev.)
state_start_vector <- sapply(shortlist_regexpr, "[")
state_end_vector <- state_start_vector + attr(shortlist_regexpr, "match.length") - 1 ## -1 to include starting character

state_start_vector_abbrev <- sapply(shortlist_abbrev_regexpr, "[")
state_end_vector_abbrev <- state_start_vector_abbrev + attr(shortlist_abbrev_regexpr, "match.length") - 1 ## -1 to include starting character

# create data frame of 50 state names (+abbrev.)
state_names <- substr(state_shortlist, state_start_vector, state_end_vector)
state_abbrev <- substr(state_shortlist, state_start_vector_abbrev, state_end_vector_abbrev)

states_data_frame <- data.frame(state_names, state_abbrev, stringsAsFactors = FALSE)

# clear objects
remove(states_wiki, state_shortlist, shortlist_regexpr, shortlist_abbrev_regexpr, state_start_vector, state_end_vector, 
	state_start_vector_abbrev, state_end_vector_abbrev, state_names, state_abbrev)
gc()

# check working env. is clean 
if(length(ls()) != 1) {cat("\nMore than one object in top-level environment\n")}

### Senators by State
################################################

# create empty data.frame
us_senators_data_frame <- data.frame(stringsAsFactors = FALSE)
bug_list <- vector()

# get senator info from 'List_of_United_States_Senators_from_XXXXX'
for(i in 1:length(states_data_frame[, 1])) {

	state_wiki_page_i <- readLines(paste("http://en.wikipedia.org/wiki/List_of_United_States_Senators_from_", states_data_frame[i, 1], sep = ""))

	# get senator names in wiki URL format using regexpr() and vectors of start / end character place 
	sen_names_regexpr_i <- regexpr("(?<=/></a> <a href=\\\"/wiki/).+?(?=\\\" title=)", state_wiki_page_i, perl = TRUE)

	# condition checks whether regex above matches to *something*
	# tests whether length of the number of unique starting places for the above regex values is more than 1 (i.e., more than simply '-1' for no match)
	if(length(unique(sapply(sen_names_regexpr_i, "["))) > 1) {

		# create vectors of start & end for each string
		start_vector <- sapply(sen_names_regexpr_i, "[")
		end_vector <- start_vector + attr(sen_names_regexpr_i, "match.length") - 1 ## -1 to include starting character

		names_vector <- sort(unique(substr(state_wiki_page_i, start_vector, end_vector)))[-1] ## [-1] to get rid of NA

		# create data frame based on names_vector
		state_i_sen_data <-  data.frame(names_vector, rep(paste(states_data_frame[i, 1]), length(names_vector)), rep(paste(states_data_frame[i, 2]), length(names_vector)))
		names(state_i_sen_data) <- c("senator", "state_name", "state_abbrev")

	} else {

		state_i_sen_data <- data.frame() ## means that the rbind() function below will add nothing and not bug out
		bug_list[length(bug_list) + 1] <- i 

	}

	# add temp data.frame to us_senators_data_frame macro data.frame
	us_senators_data_frame <- rbind(us_senators_data_frame, state_i_sen_data)

	print(paste("finished iteration", i, ":", states_data_frame[i, 1]))

}

# fix append extra states in bug_list
#bugs due to regex in regexpr() function in above loop (this is changed for in the code below)
for(i in 1:length(bug_list)) {

	# get wiki URL
	state_wiki_page_bug_i <- readLines(paste("http://en.wikipedia.org/wiki/List_of_United_States_Senators_from_", states_data_frame[bug_list, 1][i], sep = ""))

	# get senator names using char placement and regexpr()
	sen_names_regexpr_bug_i <- regexpr("(?<=<b><a href=\\\"/wiki/).+?(?=\\\" title=)", state_wiki_page_bug_i, perl = TRUE)

	# create vectors of start & end for each string
	start_vector <- sapply(sen_names_regexpr_bug_i, "[")
	end_vector <- start_vector + attr(sen_names_regexpr_bug_i, "match.length") - 1 ## -1 to include starting character
	
	names_vector <- sort(unique(substr(state_wiki_page_bug_i, start_vector, end_vector)))[-1] ## [-1] to get rid of NA

	# create temp data.frame() 
	state_bug_i_sen_data <- data.frame(names_vector, 
										rep(paste(states_data_frame[bug_list[i], 1]), length(names_vector)), 
										rep(paste(states_data_frame[bug_list[i], 2]), length(names_vector)))
	names(state_bug_i_sen_data) <- c("senator", "state_name", "state_abbrev")

	# rbind() with marco 'us_senators_data_frame' database
	us_senators_data_frame <- rbind(us_senators_data_frame, state_bug_i_sen_data)

}

# clear objects
remove(states_data_frame, bug_list, state_wiki_page_i, sen_names_regexpr_i, start_vector, end_vector, names_vector, state_i_sen_data, 
	state_wiki_page_bug_i, sen_names_regexpr_bug_i, state_bug_i_sen_data, i)
gc()

# check working env. is clean 
if(length(ls()) != 1) {cat("\nMore than one object in top-level environment\n")}
nrow(us_senators_data_frame) ## should be 1852! 

# ~~~~~~~~~~ CUSTOM SAVE FUNCTION ~~~~~~~~~~~~~~~~~

# automatically writes the date and time into the save name
#Note: remember ".csv" at the end of filePathAndName
customWriteCSV <- function(object, filePathAndName) {

	#unique date / time stamp to keep saves seperate
	date_time_stamp <- paste(format(Sys.time(), "%d-%m-%y"), format(Sys.time(), "%H-%M"), sep = "_") ## writes out as DD-MM-YY_HH-mm

	write.csv(object, paste(filePathAndName, date_time_stamp, sep = ""), row.names = FALSE)

}

# save data as .csv
customWriteCSV(us_senators_data_frame, "C:/Users/Jamie/Desktop/Dropbox/Jamie/Test Scripts/us_senators_data_basic.csv")


### Scrape ALL senator data off wiki
################################################

# create big dataset of each senators wikipade comprised of lines of HTML code
senator_wiki_data_ALL <- list()

for(i in 1:nrow(us_senators_data_frame)) {

	#which senator from database & webpage from wikipedia	
	senator_wiki_data_ALL[[i]] <- readLines(paste("http://en.wikipedia.org/wiki/", as.character(us_senators_data_frame[i, 1]), sep = ""))

}

# save as .R file
save(senator_wiki_data_ALL, "C:/Users/Jamie/Desktop/Dropbox/Jamie/Test Scripts/senator_wiki_data_ALL.R")


# ~~~~~~~~~~~~~~~ FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~

# function to grab multiple lines between regular expressions
grepMultiLine <- function(startline, regex, object, perlVal = FALSE) {

	#'startline' is typically determined after already using grep() once
	grep_line <- grep(regex, object[startline:length(object)], perl = perlVal)
	grep_line <- min(grep_line[which(grep_line > 1)]) ## to ensure that the regex isn't taken from startline

	return(object[startline : (startline + grep_line - 1)])

}

# function which searches for a regex in a chunk of text between two other regexes
#(makes use of above 'grepMultiLine' function)
regexBetweenRegex <- function(regex1, regex2, regex3, data, perl1 = FALSE, perl2 = FALSE, perl3 = FALSE) {

	#define a starting line in a text based on regex1
	startline <- grep(regex1, data, perl = perl1)
	if(length(startline) == 0) {print("error with startline from regex1"); return(NA)}

	#pull all lines between regex1 and regex2
	raw_info <- grepMultiLine(startline, regex2, data, perlVal = perl2)
	if(length(raw_info) == 0) {print("error with raw_info from regex2"); return(NA)}

	#find regex3 in the lines between regex1 and regex2
	info_regexpr <- regexpr(regex3, raw_info, perl = perl3)
	if(length(info_regexpr) == 0) {print("error with info_regexpr from regex3"); return(NA)}

	#extract regex3
	start_vector <- sapply(info_regexpr, "[")
	end_vector <- start_vector + attr(info_regexpr, "match.length") - 1
	result <- sort(unique(substr(raw_info, start_vector, end_vector)))[-1] ## '[-1]' takes care of missing entry after sort(unique()). This is a cheap hax. FIX!

	return(result)

}


## ~~~~~~~~~~~~~~~~~~~ SCRAPING ~~~~~~~~~~~~~~~~~~~

# load data if data is not present in top-level environment
if(!"senator_wiki_data_ALL" %in% ls()) {
	
	load("C:/Users/Jamie/Desktop/Dropbox/Jamie/Test Scripts/senator_wiki_data_ALL.R")

}

# OVERALL STRATEGY:
#create a for loop for each variable (i.e., birth date, in office, birth place, etc.)
#use list of data rather than scrape from wiki each time - increases speed and reduces connection issues.

# PERSONAL DATA : BIRTH & DEATH DATES
# create variable
us_senators_data_frame$birth_death_date_raw <- NA

# for loop over each senator to grab opening line and key date section and store in dataframe
for(i in 1:nrow(senator_wiki_data_ALL)) {

	#BUG LIST
	# i <- 84 (phonetic alphabet HTML script impedes standard scrape)

	#which senator from pre-existing wikipedia database	
	senator_i_wiki <- senator_wiki_data_ALL[[i]]

	#messy scrape of intro line
	intro_line <- grep("^<p><b>.*(born|â€“|-)", senator_i_wiki, value = TRUE)

	#take 'â€“' (unicode encoding) between parentheses and extract content (i.e., dates!)
	intro_line_regexpr <- regexpr("\\(.*?(born|â€“|-).+?\\)", intro_line)

	#use substr() to extract key info
	dates_raw <- substr(intro_line, intro_line_regexpr, intro_line_regexpr + attr(intro_line_regexpr, "match.length") - 1)

	#in case of multiple entries from regexpr() line
	dates_raw <- dates_raw[which(nchar(dates_raw) > 0)]

	#update variable with info extracted from substr() or NA depending on length of dates_raw
	if(length(dates_raw) != 0) {

		us_senators_data_frame$birth_death_date_raw[i] <- dates_raw[1] ##the [1] takes care of length() > 1

	} else if(length(dates_raw) == 0 & length(intro_line) != 0) {

		#this is still better than no data.
		us_senators_data_frame$birth_death_date_raw[i] <- intro_line[1]

	} else {

		us_senators_data_frame$birth_death_date_raw[i] <- NA

	}

	print(i)

}

# PERSONAL DATA : ASSUMED / IN OFFICE

# loop over each senator and scrape wiki info related to government offices held
for(i in 1:nrow(us_senators_data_frame)) {

	#which senator from pre-existing wikipedia database	
	senator_i_wiki <- senator_wiki_data_ALL[[i]]

	# scrape info from right-hand side (RHS) box on wiki page for offices
	# find startline based on HTML 'infobox vcard' table class
	RHS_table_start <- grep("infobox vcard", senator_i_wiki)

	# make sure 'RHS_table_start' has length() > 0
	if(length(RHS_table_start) == 0) { 

		#if no data, start next iteration
		next

	}

	# scrape all data from 'infobox vcard' line to end of HTML '</table>''
	RHS_table <- grepMultiLine(RHS_table_start, "</table>", senator_i_wiki) ## trying to parse info on RHS

	# parsing text for key offices based on key HTML/CSS features of 'offices' as on wikipedia
	office_shortlist <- RHS_table[grepl("text-align:\\s*center", RHS_table) & ##get centred text (necessary condition of office)
								 !grepl("(O|o)ffice", RHS_table) & ##make sure it's NOT actually the word 'office'
							   	  grepl("href=", RHS_table) & ##make sure it's 'href=' (i.e., linked to something else)
							      grepl("background-color:\\s*lavender", RHS_table)] ##this sure it's an actual RHS table section title

	# make sure length() of 'office_shortlist' > 0
	if(length(office_shortlist) == 0) { 

		#if no data, start next iteration
		next

	}

	#regexpr() office and substr() replace with 'office_shortlist' with clean value
	office_shortlist_regexpr <- regexpr("(?<=href=\\\"/wiki/).+?(?=\\\")", office_shortlist, perl = TRUE) ## using 'href' instead of 'title' in case wiki URLs needed
	office_vector <- substr(office_shortlist, office_shortlist_regexpr, office_shortlist_regexpr + attr(office_shortlist_regexpr, "match.length") - 1)
						   
	# start looking for dates from lines identified in 'office_vector'
	dates_start_search <- which(RHS_table %in% office_shortlist)

	# empty dates vector
	office_dates_raw <- vector()

	# for loop over each of 'office_vector' to look for closest date immediately below each office
	for(j in 1:length(office_vector)) {

		#while loop setup
		counter <- 1
		dates_temp <- vector() 

		#while loop until dates_temp is filled
		while(length(dates_temp) == 0) {

			#check for 4 consecutive numbers (i.e., 1992)
			if(grepl("[0-9]{4}", RHS_table[dates_start_search[j] + counter])) {

				#update temp variable
				dates_temp <- RHS_table[dates_start_search[j] + counter]

			}

			counter <- counter + 1

		}

		#update 'office_dates_raw[j]' with temp values
		office_dates_raw[j] <- dates_temp

	}

	# create new office & (corresponding) date variables as needed
	if(length(grep("office_", names(us_senators_data_frame))) < length(office_vector)) {

		#how many vars need creating
		difference <- length(office_vector) - length(grep("office_", names(us_senators_data_frame)))

		#add new variables
		for(d in 1:difference){

			#once for office name
			us_senators_data_frame[, ncol(us_senators_data_frame) + 1] <- NA
			names(us_senators_data_frame)[ncol(us_senators_data_frame)] <- paste("office_", length(grep("office_", names(us_senators_data_frame))) + 1, sep = "")

			#once for office dates
			us_senators_data_frame[, ncol(us_senators_data_frame) + 1] <- NA
			names(us_senators_data_frame)[ncol(us_senators_data_frame)] <- paste("dates_", length(grep("dates_", names(us_senators_data_frame))) + 1, sep = "")

		}

	}

	# fill variables with scraped values in 'office_vector' & 'office_dates_daw'
	for(o in 1:length(office_vector)) {

		col_num_office <- which(names(us_senators_data_frame) == paste("office_", o, sep = ""))
		col_num_dates <- which(names(us_senators_data_frame) == paste("dates_", o, sep = ""))

		us_senators_data_frame[i, col_num_office] <- office_vector[o]
		us_senators_data_frame[i, col_num_dates] <- office_dates_raw[o]

	}

	print(i)

}

# remove temp objects
rm(senator_i_wiki, RHS_table_start, RHS_table, office_shortlist, office_shortlist_regexpr, office_vector,
	dates_start_search, office_dates_raw, counter, dates_temp, difference, col_num_office, col_num_dates, i)
gc()








#the easy edit page is nice
#what about: http://en.wikipedia.org/w/index.php?title=Ed_Markey&action=edit

#and graphing the data
#http://en.wikipedia.org/w/index.php?title=Barack_Obama&action=history















## text data from sections
# section headings
senator_i_section_headings <- regexBetweenRegex("^<li class=\\\"toclevel-1 tocsection-1\\\"><a href",
												"</div>",
												"(?<=#).+?(?=\\\")",
												senator_i_wiki, perl3 = TRUE)

# add info for each of j sections in text format
for(j in 1:length(senator_i_section_headings)) {

	#startline for each section
	contents_j_startline <- grep(paste("^<h2><span class=\\\"mw-headline\\\" id=\\\"", section_headings[j],sep = ""), stuff)

	#raw info for each section
	contents_j_raw_info <- grepMultiLine(contents_j_startline, "^<h2><span class=\\\"mw-headline\\\"", stuff)

	#take lines with content and gsub() HTML script out
	contents_j_single_string <- paste(gsub("<.+?>", "", grep("^<p>", contents_j_raw_info, value = TRUE)), collapse = " ")

	#add

}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## older shitter stuff needing to updated




# example with Ed_Markey
stuff <- readLines("http://en.wikipedia.org/wiki/Ed_Markey")

#assumed office
gsub("<.+?>", "", stuff[grep("Assumed office</b></span><br />$", stuff) + 1]) # +1 to grab next line

#personal data
startline_personal <- grep("<th style=\\\"text-align:left;\\\">Born</th>", stuff)
personal_raw_info <- grepMultiLine(startline_personal, "</tr>", stuff)

#party affiliation

#spouse(s)

#resident

#alma_mater

#religion

#website
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             


#### contents
startline_contents <- grep("^<li class=\\\"toclevel-1 tocsection-1\\\"><a href", stuff)
contents_raw_info <- grepMultiLine(startline_contents, "</div>", stuff)

section_regexpr <- regexpr("(?<=#).+?(?=\\\")", contents_raw_info, perl = TRUE)

section_start_vector <- sapply(section_regexpr, "[")
section_end_vector <- section_start_vector + attr(section_regexpr, "match.length") - 1

section_headings <- sort(unique(substr(contents_raw_info, section_start_vector, section_end_vector)))[-1] ## -1 for ""

j <- 5
contents_j_startline <- grep(paste("^<h2><span class=\\\"mw-headline\\\" id=\\\"", section_headings[j],sep = ""), stuff)
contents_j_raw_info <- grepMultiLine(contents_j_startline, "^<h2><span class=\\\"mw-headline\\\"", stuff)

contents_j_single_string <- paste(gsub("<.+?>", "", grep("^<p>", contents_j_raw_info, value = TRUE)), collapse = " ")






###########################

