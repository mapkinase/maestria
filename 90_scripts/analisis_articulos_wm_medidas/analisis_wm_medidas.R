###### REVTOOLS #######

install.packages("revtools")
# MJ Westgate (2019) revtools: An R package to support article screening for evidence synthesis. Research Synthesis Methods http://doi.org/10.1002/jrsm.1374 [PDF]

library(revtools)

# revtools is designed to import bibliographic data; specifically the kinds of files that you can export from academic databases such as Web of Science or Scopus
# Alternatively, most bibliographic management software (such as Zotero, Mendeley) can export to a range of formats, including .ris

data <- read_bibliography("pubmed-ris/pubmed-ris.ris")
class(data) # = data.frame
# data_all <- read_bibliography(c("my_data.ris", "my_data.bib"))
# A common extension to this approach is to detect all of the file names in a given subdirectory, and import them all simultaneously:
# If the files are in the working directory:
#file_names <- list.files()

# Or if they are in a subdirectory:
#path <- "./raw_data/"
#file_names <- paste0(path, list.files(path = path))

# Then import to a data.frame
#data_all <- read_bibliography(file_names)

results <- screen_topics(data)







######### bibliometrix and biblioshiny
install.packages("bibliometrix")

library(bibliometrix)

bibliometrix::biblioshiny()


