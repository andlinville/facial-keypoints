# This is the second algorithm described in the kaggle tutorial
library(doMC)
library(reshape2)

rm(list=ls())
setwd('~/Desktop/Projects/facial-keypoints')
data.dir <- paste0(getwd(), '/data')
output.dir <- paste0(getwd(), '/output')

# Load data
load(file=paste0(data.dir, '/data_train.Rdata'))
load(file=paste0(data.dir, '/data_test.Rdata'))
load(file=paste0(data.dir, '/image_train.Rdata'))
load(file=paste0(data.dir, '/image_test.Rdata'))
load(file=paste0(data.dir, '/sample_submission.Rdata'))

# Vector to hold names of all keypoints
keypoints <- c('left_eye_center', 'right_eye_center', 'left_eye_inner_corner', 'left_eye_outer_corner',
               'right_eye_inner_corner', 'right_eye_outer_corner', 'left_eyebrow_inner_end', 'left_eyebrow_outer_end',
               'right_eyebrow_inner_end', 'right_eyebrow_outer_end', 'nose_tip', 'mouth_left_corner',
               'mouth_right_corner', 'mouth_center_top_lip', 'mouth_center_bottom_lip')

# Get patch for an aribitrary keypoint based on an arbitrary function
# Test images will be tested for correlation with this patch
GetKeypointStat <- function (data, images, keypoint, patch_size=10, FUN=mean, ...) { 
  keypoint_x <- paste(keypoint, "x", sep="_")
  keypoint_y <- paste(keypoint, "y", sep="_")
  patches <- foreach (i = 1:nrow(data), .combine=rbind) %do% {
    im <- matrix(images[i, ], nrow=96, ncol=96)
    x <- data[i, keypoint_x]
    y <- data[i, keypoint_y]
    x1 <- x - patch_size
    y1 <- y - patch_size
    x2 <- x + patch_size
    y2 <- y + patch_size
    if (!is.na(x) && !is.na(y) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96)) {
      as.vector(im[x1:x2, y1:y2])
    } else {
      NULL
    }
  }
  
  stat_data <- apply(patches, 2, FUN=FUN, ...)
  stat_patch <- matrix(data=stat_data, nrow=2*patch_size+1, ncol=2*patch_size+1)
  return(stat_patch)
}

# Check function on arbitrary keypoints
mean_left_eye <- GetKeypointStat(d.train, im.train, "left_eye_center", FUN=mean)
image(1:21, 1:21, mean_left_eye[21:1, 21:1], col=gray((0:255)/255))

mean_nose_tip <- GetKeypointStat(d.train, im.train, "nose_tip", patch_size=15, FUN=mean)
image(1:31, 1:31, mean_nose_tip[31:1, 31:1], col=gray((0:255)/255))

# Given a trained patch (output from GetKeypointPatch) for an arbitrary keypoint,
# test a set of search patches for their correlation with the trained patch
KeypointPatchTest <- function(data, test_image, stat_patch, patch_size, keypoint, search_size=2) {
  keypoint_x <- paste(keypoint, "x", sep="_")
  keypoint_y <- paste(keypoint, "y", sep="_")
  mean_x <- mean(d.train[, keypoint_x], na.rm=T)
  mean_y <- mean(d.train[, keypoint_y], na.rm=T)
  x1 <- as.integer(mean_x)-search_size
  x2 <- as.integer(mean_x)+search_size
  y1 <- as.integer(mean_y)+search_size
  y2 <- as.integer(mean_y)-search_size
  params <- expand.grid(x=x1:x2, y=y1:y2)
  im <- matrix(data=test_image, nrow=96, ncol=96)
  r <- foreach (j=1:nrow(params), .combine=rbind) %dopar% {
    x <- params$x[j]
    y <- params$y[j]
    p <- im[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
    score <- cor(as.vector(p), as.vector(stat_patch))
    score <- ifelse(is.na(score), 0, score)
    data.frame(x, y, score)
  }
  match <- r[which.max(r$score), c('x', 'y')]
  return(match)
}

# Function to superimpose result of KeypointPatchTest onto image
ShowKeypointPatchTest <- function (image, result) {
  x <- result$x[1]
  y <- result$y[1]
  im <- matrix(data=rev(image), nrow=96, ncol=96)
  image(1:96, 1:96, im, col=gray((0:255)/255))
  points(96-x, 96-y, col='red')
}

# Test KeypointPatchTest
left_eye_center_1 <- KeypointPatchTest(d.train, im.train[25,], mean_left_eye, "left_eye_center", search_size=2)
ShowKeypointPatchTest(im.train[25,], left_eye_center_1)

# Loop through all keypoints to get patches
GetKeypointPatches <- function(d.train, d.test, keypoints, patch_size) {
  mean_patches <- foreach (i=keypoints, .combine=rbind) %dopar% {
    p <- GetKeypointStat(d.train, im.train, i, FUN=mean, patch_size=patch_size)
    p <- as.vector(p)
  }
  return(mean_patches)
}

# Use patches to predict keypoint locations in single row of test data
GetKeypointLocs <- function(im.test.i, keypoints, patches, patch_size, search_size) {
  im.result <- foreach (k=1:length(keypoints), .combine=rbind ) %dopar% {
    patch <- patches[k,]
    k.name <- keypoints[k]
    k.result <- KeypointPatchTest(d.train, im.test.i, patch, patch_size, k.name, search_size=search_size)
    data.frame(k.result)
  } 
  return(im.result)
}

# Use patches to predict kepoint locations in test data
PatchSearch <- function(d.test, im.test, keypoints, patches, patch_size, search_size) {
  locations <- NULL
  for (j in 1:nrow(im.test)) {
    id <- d.test[j, 1]
    im <- im.test[j,]
    im.result <- GetKeypointLocs(im, keypoints, patches, patch_size, search_size)
    im.result$Keypoints <- keypoints
    im.result$ImageId <- rep(id, length(keypoints))
    im.result <- melt(im.result, id.vars=c("ImageId", "Keypoints"), variable.name="Coord", value.name="Location")
    im.result$FeatureName <- paste(im.result$Keypoints, im.result$Coord, sep="_")
    im.result$Keypoints <- NULL
    im.result$Coord <- NULL
    locations <- rbind(locations, im.result)
  }
  return(locations)
}

mean_keypoints_p12 <- GetKeypointPatches(d.train, d.test, keypoints, 12)
mean_keypoints_p10 <- GetKeypointPatches(d.train, d.test, keypoints, 10)
mean_keypoints_p8 <- GetKeypointPatches(d.train, d.test, keypoints, 8)
s.submission$Location <- NULL

locations_p10_ss2 <- PatchSearch(d.test, im.test, keypoints, mean_keypoints_p10, 10, search_size=2)
submission <- merge(s.submission, locations_p10_ss2, all.x=T, sort=F)[,c('RowId', 'Location')]
write.csv(submission, paste0(output.dir, '/submission_patch_benchmark.csv'), quote=F, row.names=F)

locations_p8_ss4 <- PatchSearch(d.test, im.test, keypoints, mean_keypoints_p8, 8, search_size=4)
submission <- merge(s.submission, locations_p8_ss4, all.x=T, sort=F)[,c('RowId', 'Location')]
write.csv(submission, paste0(output.dir, '/submission_patch_benchmark_p8_ss4.csv'), quote=F, row.names=F)

locations_p12_ss1 <- PatchSearch(d.test, im.test, keypoints, mean_keypoints_p12, 12, search_size=1)
submission <- merge(s.submission, locations_p12_ss1, all.x=T, sort=F)[,c('RowId', 'Location')]
write.csv(submission, paste0(output.dir, '/submission_patch_benchmark_p12_ss1.csv'), quote=F, row.names=F)
