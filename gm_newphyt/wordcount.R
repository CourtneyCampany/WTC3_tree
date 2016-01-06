%% uncomment this after compiling the ms, then compile again. This is a recursive dependency :(
  % Also, requires Perl.
  % #' <<>>=
  % #' shell("texcount manuscript.tex > wordcount.txt")
  % #' nrw <- get_wordcount("wordcount.txt")
  % #' @
  % #' 
  % #' Total number of words: \Sexpr{nrw$Total} \\
  % #' Abstract: \Sexpr{nrw$Abstract} \\
  % #' Introduction: \Sexpr{nrw$Introduction} \\
  % #' Methods and Materials: \Sexpr{nrw$Methods} \\
  % #' Results: \Sexpr{nrw$Results} \\
  % #' Discussion: \Sexpr{nrw$Discussion} \\
  % #' 
  % #' Number of figures: 6 (all in colour) \\
  % #' Number of tables: 2 \\
  % #' Supporting information : 1 Tables, 3 Figures and 1 Notes.
  
  get_wordcount <- function(fn){
    
    # gets first numeric string
    getcount <- function(x)as.numeric(str_extract(x, "[0-9]{1,4}"))
    
    # not robust; will find first matched argument
    count <- function(txt)getcount(r[grep(txt,r)[1]])
    
    if(file.exists(fn)){
      
      r <- readLines(fn)
      
      # should do this more cleverly, but annoyingly the subsections are not added to a total
      # for the section. So more work to make this general
      
      l <- list()
      l$Abstract <- count("Abstract")
      l$Introduction <- count("Introduction")
      l$Methods <- count("Terminology") + count("Data")  + 
        count("Data analysis")
      l$Results <- count("Results")
      l$Discussion <- count("Discussion")
      
      l$Total <- count("Words in text")
      
      return(l)
      
    } else {
      return(list(NA))
    }
  }
  