library(readr)
library(sliced)
library(rstudioapi)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

rm(list=ls()) # Clean previous environment variables

DATA_FRAME  <- read_csv("test_data.csv")
SAMPLE_SIZE <- 16    # Set as zero if you are not testing
VERBOSE     <- TRUE
N_BLOCKS    <- 3

if(SAMPLE_SIZE > 0) {
  sampleRangeBegin <- sample(1:(nrow(DATA_FRAME)-SAMPLE_SIZE), 1)
  sampleRangeEnd   <- sampleRangeBegin + SAMPLE_SIZE - 1
  DATA_FRAME <- DATA_FRAME[sampleRangeBegin:sampleRangeEnd,]
}

ROW_NAMES <- unlist(DATA_FRAME[,1]) # Get the first column as a vector
TRANSPOSED_DF <- as.data.frame(t(DATA_FRAME[,-1])) # Transpose without first column
colnames(TRANSPOSED_DF) <- ROW_NAMES # Set colnames

COR_MATRIX <- sliced.cor(TRANSPOSED_DF, N_BLOCKS, "pearson", VERBOSE)
colnames(COR_MATRIX) <- ROW_NAMES
rownames(COR_MATRIX) <- ROW_NAMES
COR_FILE <- "test_out.csv"
sliced.write(COR_MATRIX, COR_FILE, N_BLOCKS, VERBOSE)
