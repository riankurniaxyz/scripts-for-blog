rm(list=ls()); cat("\f");
SVD_list <- list();
distance_matrix_list <- list()
label_result <- vector()


load_labels_file = function(filename) {
  f <- file(filename, "rb")
  readBin(f, "integer", n=1, endian="big")
  nlabel <- readBin(f, "integer", n=1, endian="big")
  y <- readBin(f, "integer", n=nlabel, size=1, endian = "big")
  close(f)
  y
}

load_images_file = function(filename) {
  f <- file(filename, "rb")
  readBin(f, "integer", n=1, endian="big")
  nimage <- readBin(f, "integer", n=1, endian="big")
  nr <- readBin(f, "integer", n=1, endian="big")
  nc <- readBin(f, "integer", n=1, endian="big")
  x <- readBin(f, "integer", n=nimage*nr*nc, size=1, signed = FALSE, endian = "big")
  close(f)
  M <- matrix(x,nrow = nimage, ncol = nr*nc, byrow = TRUE)
  t(M)
}

show_digit = function(imagevector) {
  M <- matrix(as.matrix(imagevector), 28,28)[,28:1]
  image(M)
}

train_labels <- load_labels_file("train-labels.idx1-ubyte")
train_images <- load_images_file("train-images.idx3-ubyte")
test_labels <- load_labels_file("t10k-labels.idx1-ubyte")
test_images <- load_images_file("t10k-images.idx3-ubyte")

ptm <- proc.time()

n <- 10
for (i in 0:9) {
  digit_matrix <- train_images[,train_labels==i]
  SVD <- svd(digit_matrix); SVD_list[[i+1]] <- SVD
  U_truncated <- SVD$u[,1:n]
  distance_matrix_list[[i+1]] <- diag(dim(train_images)[1])-U_truncated %*% t(U_truncated)
}

for (j in 1:dim(test_images)[2]) {
  digit_vector <- test_images[,j]
  min_distance <- 0
  temp_label <- 0
  # compute distance for each digit category
  for (i in 0:9) {
    distance <- norm(distance_matrix_list[[i+1]] %*% digit_vector)
    if (i==0) {
      min_distance <- distance
    }
    else if (distance < min_distance) {
      min_distance <- distance; temp_label <- i
    }
  }
  label_result[j] <- temp_label
}

accuracy <- mean(label_result==test_labels)
elapsed_time <- proc.time() - ptm

print(paste("The accuracy is", accuracy))
print(paste("Done in", elapsed_time[1], "seconds"))
