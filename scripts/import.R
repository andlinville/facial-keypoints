library(doMC)

rm(list=ls())
setwd('~/Desktop/Projects/facial-keypoints')
raw.dir <- paste0(getwd(), '/raw')
data.dir <- paste0(getwd(), '/data')

# Read in and save training data
d.train <- read.csv(paste0(raw.dir, '/training.csv'), stringsAsFactors=F)
str(d.train)
im.train <- d.train$Image
d.train$Image <- NULL
im.train <- foreach (im=im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

# Read in and save the test data
d.test <- read.csv(paste0(raw.dir, '/test.csv'), stringsAsFactors=F)
str(d.test)
im.test <- d.test$Image
d.test$Image <- NULL
im.test <- foreach (im=im.test, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

# Read in and save the sample submission file
s.submission <- read.csv(paste0(raw.dir, '/IdLookupTable.csv'), stringsAsFactors=F)

# Save test and train data and sample submission
save(d.train, file=paste0(data.dir, '/data_train.Rdata'))
save(im.train, file=paste0(data.dir, '/image_train.Rdata'))
save(d.test, file=paste0(data.dir, '/data_test.Rdata'))
save(im.test, file=paste0(data.dir, '/image_test.Rdata'))
save(s.submission, file=paste0(data.dir, '/sample_submission.Rdata'))
