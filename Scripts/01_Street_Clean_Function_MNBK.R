###### Chang Xu ######

clean<-function(x){
  x<-gsub("\\<STREET\\>|\\<SRT\\>$|\\<SR\\>$\\<SRT\\>$|\\<STR\\>$|\\<SST\\>$|\\<SEET\\>$|\\<TREET\\>$|\\<SHEER\\>$|\\<SHEE\\>$|\\<STREE\\>$|\\<SREET\\>$|\\<REET\\>$|\\<STEE\\>$|\\<ST\\>$","ST",x)
  x<-gsub("\\<N\\>","N",x)
  x<-gsub("\\<S\\>","S",x)
  x<-gsub("\\<E\\>","E",x)
  x<-gsub("\\<W\\>","W",x)
  x<-gsub("\\<DRIVE\\>|\\<DR\\>|\\<DV\\>|\\<DE\\>$|\\<DRV\\>|\\<DRI\\>|\\<DRIV\\>|\\<DRIE\\>","DR",x) 
  x<-gsub("\\<CIRCLE\\>|\\<CIR\\>|\\<CRL\\>|\\<CIRC\\>|\\<CR\\>|\\<CL\\>|\\<CIRCL\\>|\\<CICLE\\>","CIR",x)
  x<-gsub("\\<AVENUE\\>|\\<AVE\\>|\\<AV\\>|\\<AVN\\>|\\<AVEN\\>|\\<AVENU\\>","AVE",x)
  x<-gsub("\\<COURT\\>|\\<CT\\>|\\<CRT\\>|\\<CTR\\>|\\<COUR\\>|\\<COT\\>|\\<CORT\\>","CT",x)
  x<-gsub("\\<BOULEVARD\\>|\\<BLVD\\>|\\<BVLD\\>|\\<BV\\>|\\<BLD\\>|\\<BD\\>|\\<BL\\>|\\<BLV\\>","BLVD",x)
  x<-gsub("\\<ROAD\\>|\\<RD\\>|\\<RAD\\>|\\<ROD\\>","RD",x)
  x<-gsub("\\<ALLEY\\>|\\<ALY\\>|\\<AL\\>|\\<ALLY\\>|\\<ALEY\\>|\\<ALLE\\>|\\<AY\\>","ALY",x)
  x<-gsub("\\<PLACE\\>|\\<PL\\>|\\<PLC\\>|\\<PLE\\>|\\<PC\\>|\\<PLAC\\>|\\<PLCE\\>|\\<PCE\\>","PL",x)
  x<-gsub("\\<PK\\>|\\<PRK\\>|\\<PRAK\\>|\\<PAK\\>","PARK",x)
  x<-gsub("\\<PARKWAY\\>|\\<PKWY\\>|\\<PARKW\\>|\\<PWY\\>|\\<PKW\\>|\\<PRKWY\\>|\\<PKWY\\>|\\<PKW\\>","PKWY",x)
  x<-gsub("\\<APPR\\>|\\<APR\\>|\\<APPROA\\>|\\<APRCH\\>|\\<APPRCH\\>","APPROACH",x)
  x<-gsub("\\<TERRACE\\>|\\<TER\\>|\\<TERR\\>|\\<TRC\\>|\\<TRCE\\>|\\<TR\\>","TER",x)
  x<-gsub("\\<PLAZA\\>|\\<PLZ\\>|\\<PLAZ\\>|\\<PZ\\>|\\<PLZA\\>","PLZ",x)
  x<-gsub("\\<LANE\\>|\\<LN\\>|\\<LNE\\>|\\<LAN\\>","LN",x)
  x<-gsub("\\<BRIDGE\\>|\\<BRG\\>|\\<BRGD\\>|\\<BGE\\>","BRG",x)
  x<-gsub("\\<HILL\\>|\\<HL\\>|\\<HLL\\>|\\<HIL\\>","HL",x)
  x<-gsub("\\<HEIGHTS\\>|\\<HTS\\>|\\<HT\\>|\\<HEIGHT\\>|\\<HEGHTS\\>|\\<HHT\\>|\\<HEIGT\\>","HTS",x) 
  x<-gsub("\\<SLP\\>|\\<SLEP\\>|\\<SLIIP\\>|\\<SLI\\>","SLIP",x)
  x<-gsub("\\<RON\\>|\\<RW\\>|\\<ROE\\>|\\<ROOW\\>","ROW",x)
  x<-gsub(".*\\((.*)\\).*", "\\1", x)
  x<-gsub("\\<ST\\>","",x) # new remove streets
  x<-gsub("\\d+\\ - *\\d*|\\d+\\ TO *\\d*|\\d+\\-\\d*","",x) #remove addresses
  
  
  ## dealing with numbered streets
  x<-gsub("(\\d)(ST|ND|RD|TH)\\b", "\\1", x)
  x<-str_remove(x, "(?<=[0-9])(ST|ND|RD|TH)")
  x<-gsub("\\<ONE HUNDRED\\>|\\<ONEHUNDRED\\>|\\<HUNDRED\\>|\\<HUDRED\\>|\\<HUNDED\\>","1",x) 
  x<-gsub("\\<TWO HUNDRED\\>|\\<TWOHUNDRED\\>","2",x)
  x<-gsub("-"," ",x)
  x <- gsub("\\\\", "", x) ##new
  x <- gsub("\\'", "", x) ## remove apostrophe
  #x<-gsub("\\<AND\\>"," ",x) keep AND
  # no "AMP" in the dataset
  x<-gsub("&","AND",x)
  x<-gsub("\\<1ST\\>|\\b1\\b","1",x)
  x<-gsub("\\<2ND\\>|\\b2\\b","2",x)
  x<-gsub("\\<3RD\\>|\\b3\\b","3",x)
  x<-gsub("\\<4TH\\>|\\b4\\b","4",x)
  x<-gsub("\\<5TH\\>|\\b5\\b","5",x)
  x<-gsub("\\<6TH\\>|\\b6\\b","6",x)
  x<-gsub("\\<7TH\\>|\\b7\\b","7",x)
  x<-gsub("\\<8TH\\>|\\b8\\b","8",x)
  x<-gsub("\\<9TH\\>|\\b9\\b","9",x)
  #new
  x<-gsub("\\<TENTH\\>|\\<10TH\\>|\\b10\\b","10",x)
  x<-gsub("\\<ELEVENTH\\>|\\<11TH\\>|\\b11\\b","11",x)
  x<-gsub("\\<TWELFTH\\>|\\<12TH\\>|\\b12\\b","12",x)
  x<-gsub("\\<THIRTEENTH\\>|\\<13TH\\>|\\b13\\b","13",x)
  x<-gsub("\\<FORTEENTH\\>|\\<14TH\\>|\\b14\\b","14",x)
  x<-gsub("\\<FIFTEENTH\\>|\\<15TH\\>|\\b15\\b","15",x)
  x<-gsub("\\<SIXTEENTH\\>|\\<16TH\\>|\\b16\\b","16",x)
  x<-gsub("\\<SEVENTEENTH\\>|\\<17TH\\>|\\b17\\b","17",x)
  x<-gsub("\\<EIGHTEENTH\\>|\\<18TH\\>|\\b18\\b","18",x)
  x<-gsub("\\<NINETEENTH\\>|\\<19TH\\>|\\b19\\b","19",x)
  
  
  x<-gsub("\\<TWENTY\\>|\\<TWENTI\\>|\\<TENTI\\>","2",x)
  x<-gsub("\\<THIRTY\\>|\\<THIRTHY\\>|\\<THIRTEY\\>|\\<TIRTY\\>|\\<TRITHY\\>","3",x)
  x<-gsub("\\<FORTY\\>|\\<FOURTY\\>|\\<FOURTHY\\>|\\<FRTY\\>","4",x)
  x<-gsub("\\<FIFTY\\>|\\<FIFTEY\\>|\\<FIFT\\>|\\<FITY\\>|\\<FIFTHY\\>","5",x)
  x<-gsub("\\<SIXTY\\>|\\<SXTY\\>|\\<SIXY\\>|\\<SXTY\\>|\\<SIXTHY\\>|\\<SIXTEY\\>","6",x)
  x<-gsub("\\<SEVENT\\>|\\<SEVENTY\\>|\\<SEVENTEY\\>|\\<SVENTY\\>|\\<SEVENTI\\>","7",x)
  x<-gsub("\\<EIGHTY\\>|\\<EIGHTEY\\>|\\<EIGHTE\\>","8",x)
  x<-gsub("\\<UNITY\\>|\\<NINTH\\>|\\<NINETY\\>|\\<NINETEY\\>|\\<NINETIETH\\>|\\<NINTY\\>","9",x)
  x<-gsub("\\<FRIST\\>|\\<FIST\\>|\\<FRST\\>|\\<FIRST\\>|\\<ONE\\>","1",x)
  x<-gsub("\\<SECOND\\>|\\<SECORD\\>|\\<SCOND\\>|\\<SECOND\\>|\\<TWO\\>","2",x)
  x<-gsub("\\<THRID\\>|\\<THIRD\\>|\\<TIRD\\>|\\<TRIHD\\>|\\<THREE\\>","3",x)
  x<-gsub("\\<FORTH\\>|\\<FOURTH\\>|\\<FROTH\\>|\\<FROUTH\\>|\\<FOUR\\>","4",x)
  x<-gsub("\\<FIFETH\\>|\\<FIFTH\\>|\\<FIFFTH\\>|\\<FIFTHE\\>|\\<FIVE\\>","5",x)
  x<-gsub("\\<SIXTH\\>|\\<SXTH\\>|\\<SITH\\>|\\<SIHXT\\>|\\<SIX\\>","6",x)
  x<-gsub("\\<SEVENTH\\>|\\<SVEN\\>|\\<SVENTH\\>|\\<SEVENH\\>|\\<SEVENT\\>|\\<SEVEN\\>","7",x)
  x<-gsub("\\<EIGHTH\\>|\\<EIGHTEH\\>|\\<EITH\\>|\\<EIGHT\\>","8",x)
  x<-gsub("\\<NINETH\\>|\\<NINTH\\>|\\<NINT\\>|\\<NINETH\\>|\\<NINE\\>|\\<NIN\\>","9",x)
  x<-gsub("\\<TWENTIETH\\>|\\<TWENTIEFTH\\>","20",x) 
  x<-gsub("\\<THIRTIETH\\>|\\<THIRTIEFTH\\>","30",x)
  x<-gsub("\\<FORTIETH\\>|\\<FOURTIETH\\>","40",x)
  x<-gsub("\\<FIFTIETH\\>","50",x)
  x<-gsub("\\<SIXTIETH\\>","60",x)
  x<-gsub("\\<SEVENTIETH\\>","70",x)
  x<-gsub("\\<EIGHTIETH\\>","80",x)
  x<-gsub("\\<NINETIETH\\>|\\<NINTIETH\\>","90",x)
  x<-gsub("(?<=\\d) (?=\\d)","",x,perl = T) 
  
  x<-gsub("^\\bST\\b","SAINT", x) 
  x<-gsub("\\bHOUSE\\b","", x)
  x<-gsub("\\bHOSTEL\\b","", x)
  x<-gsub("\\bHOTEL\\b","", x)
  x<-gsub("\\bLODGE\\b","", x)
  x<-gsub("\\bLODGING\\b","", x)
  x<-trimws(x, "both")
  
  
  ## overall spacing problem extra white space
  x <- gsub("((\\w+|\\d+)\\s\\s(\\w+|\\d+))", "\\2 \\3", x)
  
  ## 4 W to W 4
  x <- gsub("(\\w+|\\d+)\\s(\\bN\\b|\\bW\\b|\\bS\\b|\\bE\\b)", "\\3\\2 \\1", x)
  #x <- gsub("(\\w+|\\d+)\\s(N|W|S|E)", "\\3\\2 \\1", x) ##new
  
  ## W  4 to W 4
  #x <- gsub("(N|W|S|E)\\s\\s(\\w+|\\d+)", "\\1 \\2 \\3", x)
  
  
  ## AVE D
  x <- gsub("(\\b[A-Z])\\s(AVE)", "\\3\\2 \\1", x)
  
} 
