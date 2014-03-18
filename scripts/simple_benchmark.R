# This is the first algorithm described in the kaggle tutorial
# I will use this mostly to build an initial submission dataset

library(reshape2)

rm(list=ls())
setwd('~/Desktop/Projects/facial-keypoints')
data.dir <- paste0(getwd(), '/data')
output.dir <- paste0(getwd(), '/output')

load(file=paste0(data.dir, '/data_train.Rdata'))
load(file=paste0(data.dir, '/data_test.Rdata'))
load(file=paste0(data.dir, '/image_train.Rdata'))
load(file=paste0(data.dir, '/image_test.Rdata'))
load(file=paste0(data.dir, '/sample_submission.Rdata'))

# Use mean location of each keypoint as prediction result
keypoints_matrix <- matrix(data=colMeans(d.train, na.rm=T), nrow=nrow(d.test), ncol=ncol(d.train), byrow=T)
colnames(keypoints_matrix) <- names(d.train)
predictions <- data.frame(ImageId=1:nrow(d.test), keypoints_matrix)
head(predictions)

# Reshape for appropriate submission format
submission <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
head(submission)

# Join to sample submission
sub.col.names <- names(s.submission)
s.submission$Location <- NULL
submission <- merge(s.submission, submission, all.x=T, sort=F)
submission <- submission[, c('RowId', 'Location')]
write.csv(submission, paste0(output.dir, '/submission_benchmark.csv'), quote=F, row.names=F)
