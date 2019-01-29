library(camtrapRdeluxe)
context("Test imageRename")
wd_images_ID <- system.file("pictures/sample_images_tagged_abundance",
                            package = "camtrapRdeluxe")

if (Sys.which("exiftool") != "") {
  # Cam TRUE KeepEmpty FALSE
  tmp <- tempdir(check=TRUE)
  dir.create(file.path(tmp, "test1"))
  dir.create(file.path(tmp, "test2"))
  file.copy(wd_images_ID, file.path(tmp, "test1"), overwrite = TRUE,
            recursive = TRUE, copy.date = TRUE)
  file.sep <- .Platform$file.sep
  tmp <- gsub(pattern = "[\\]", replacement = file.sep, x = tmp)
  dirs_all <- list.dirs(file.path(tmp, "test1"), full.names = TRUE, recursive = TRUE)
  l<-strsplit(dirs_all, split = file.sep, fixed = TRUE)
  slength <- sapply(l, length)

  rn.table <- imageRename(file.path(tmp, "test1"), hasCameraFolders = TRUE,
                keepCameraSubfolders = TRUE, createEmptyDirectories = FALSE,
                stationIDposition = grep("Station02", l[[grep(pattern = "Station02", x = l)[1]]]),
                cameraIDposition = grep("C02a", l[[grep(pattern = "C02a", x = l)[1]]]),
                outDir = file.path(tmp, "test2"), copyImages = TRUE, writecsv = TRUE)

  lf_orig <- list.files(file.path(tmp, "test1"), pattern = ".JPG$", recursive = TRUE)
  lf <- list.files(file.path(tmp, "test2"), pattern = ".JPG$", recursive = TRUE)
  csv <- list.files(file.path(tmp, "test2"), pattern = ".csv$", recursive = TRUE)
  dirs_copied <- list.dirs(file.path(tmp, "test2"), full.names = TRUE, recursive = TRUE)
  test_that("imageRename: hasCameraFolders = TRUE, createEmptyDirectories = FALSE", {
  expect_equal(dim(rn.table), c(96, 10))
  expect_equal(dirname(lf_orig), dirname(lf))
  expect_length(grep(pattern = "Station01", rn.table$Station), 3)
  expect_length(grep(pattern = "C02b", rn.table$Camera), 5)
  expect_length(lf, 96)
  expect_length(csv, 1)
  expect_length(dirs_copied, 128)
  })

  unlink(tmp, recursive=TRUE)

  # Cam FALSE KeepEmpty FALSE
  tmp <- tempdir(check=TRUE)
  dir.create(file.path(tmp, "test1"))
  dir.create(file.path(tmp, "test2"))
  file.copy(wd_images_ID, file.path(tmp, "test1"), overwrite = TRUE,
            recursive = TRUE, copy.date = TRUE)
  file.sep <- .Platform$file.sep
  tmp <- gsub(pattern = "[\\]", replacement = file.sep, x = tmp)
  dirs_all <- list.dirs(file.path(tmp, "test1"), full.names = TRUE, recursive = TRUE)
  l<-strsplit(dirs_all, split = file.sep, fixed = TRUE)
  slength <- sapply(l, length)

  rn.table <- imageRename(file.path(tmp, "test1"), hasCameraFolders = TRUE,
                          keepCameraSubfolders = FALSE, createEmptyDirectories = FALSE,
                          stationIDposition = grep("Station02", l[[grep(pattern = "Station02", x = l)[1]]]),
                          cameraIDposition = grep("C02a", l[[grep(pattern = "C02a", x = l)[1]]]),
                          outDir = file.path(tmp, "test2"), copyImages = TRUE, writecsv = TRUE)

  lf_orig <- list.files(file.path(tmp, "test1"), pattern = ".JPG$", recursive = TRUE)
  lf <- list.files(file.path(tmp, "test2"), pattern = ".JPG$", recursive = TRUE)
  csv <- list.files(file.path(tmp, "test2"), pattern = ".csv$", recursive = TRUE)
  dirs_copied <- list.dirs(file.path(tmp, "test2"), full.names = TRUE, recursive = TRUE)
  test_that("imageRename: hasCameraFolders = TRUE, createEmptyDirectories = FALSE", {
    expect_equal(dim(rn.table), c(96, 10))
    expect_false(identical(dirname(lf_orig), dirname(lf)))
    expect_length(grep(pattern = "Station01", rn.table$Station), 3)
    expect_length(grep(pattern = "C02b", rn.table$Camera), 5)
    expect_length(lf, 96)
    expect_length(csv, 1)
    expect_length(dirs_copied, 106)
  })

  unlink(tmp, recursive=TRUE)

  # Cam TRUE KeepEmpty TRUE
  tmp <- tempdir(check=TRUE)
  dir.create(file.path(tmp, "test1"))
  dir.create(file.path(tmp, "test2"))
  file.copy(wd_images_ID, file.path(tmp, "test1"), overwrite = TRUE,
            recursive = TRUE, copy.date = TRUE)
  # Create empty station folder
  dir.create(file.path(tmp, "test1", "sample_images_tagged_abundance/KL/Station05"))

  file.sep <- .Platform$file.sep
  tmp <- gsub(pattern = "[\\]", replacement = file.sep, x = tmp)
  dirs_all <- list.dirs(file.path(tmp, "test1"), full.names = TRUE, recursive = TRUE)
  l<-strsplit(dirs_all, split = file.sep, fixed = TRUE)
  slength <- sapply(l, length)

  rn.table <- imageRename(file.path(tmp, "test1"), hasCameraFolders = TRUE,
                          keepCameraSubfolders = TRUE, createEmptyDirectories = TRUE,
                          stationIDposition = grep("Station02", l[[grep(pattern = "Station02", x = l)[1]]]),
                          cameraIDposition = grep("C02a", l[[grep(pattern = "C02a", x = l)[1]]]),
                          outDir = file.path(tmp, "test2"), copyImages = TRUE, writecsv = TRUE)

  lf_orig <- list.files(file.path(tmp, "test1"), pattern = ".JPG$", recursive = TRUE)
  lf <- list.files(file.path(tmp, "test2"), pattern = ".JPG$", recursive = TRUE)
  csv <- list.files(file.path(tmp, "test2"), pattern = ".csv$", recursive = TRUE)
  dirs_copied <- list.dirs(file.path(tmp, "test2"), full.names = TRUE, recursive = TRUE)

  test_that("imageRename: hasCameraFolders = TRUE, createEmptyDirectories = TRUE", {
  expect_equal(dim(rn.table), c(96, 10))
  expect_equal(dirname(lf_orig), dirname(lf))
  expect_length(grep(pattern = "Station01", rn.table$Station), 3)
  expect_length(grep(pattern = "C02b", rn.table$Camera), 5)
  expect_length(lf, 96)
  expect_length(csv, 1)
  expect_length(dirs_copied, 129)
})
  unlink(tmp, recursive=TRUE)

  # Cam FALSE KeepEmpty TRUE
  tmp <- tempdir(check=TRUE)
  dir.create(file.path(tmp, "test1"))
  dir.create(file.path(tmp, "test2"))

  file.copy(wd_images_ID, file.path(tmp, "test1"), overwrite = TRUE,
            recursive = TRUE, copy.date = TRUE)

  # Create empty station folder
  dir.create(file.path(tmp, "test1", "sample_images_tagged_abundance/KL/Station05"))

  file.sep <- .Platform$file.sep
  tmp <- gsub(pattern = "[\\]", replacement = file.sep, x = tmp)
  dirs_all <- list.dirs(file.path(tmp, "test1"), full.names = TRUE, recursive = TRUE)
  l<-strsplit(dirs_all, split = file.sep, fixed = TRUE)
  slength <- sapply(l, length)

  rn.table <- imageRename(file.path(tmp, "test1"), hasCameraFolders = TRUE,
                          keepCameraSubfolders = FALSE, createEmptyDirectories = TRUE,
                          stationIDposition = grep("Station02", l[[grep(pattern = "Station02", x = l)[1]]]),
                          cameraIDposition = grep("C02a", l[[grep(pattern = "C02a", x = l)[1]]]),
                          outDir = file.path(tmp, "test2"), copyImages = TRUE, writecsv = TRUE)

  lf_orig <- list.files(file.path(tmp, "test1"), pattern = ".JPG$", recursive = TRUE)
  lf <- list.files(file.path(tmp, "test2"), pattern = ".JPG$", recursive = TRUE)
  csv <- list.files(file.path(tmp, "test2"), pattern = ".csv$", recursive = TRUE)
  dirs_copied <- list.dirs(file.path(tmp, "test2"), full.names = TRUE, recursive = TRUE)

  test_that("imageRename: hasCameraFolders = TRUE, createEmptyDirectories = TRUE", {
    expect_equal(dim(rn.table), c(96, 10))
    expect_false(identical(dirname(lf_orig), dirname(lf)))
    expect_length(grep(pattern = "Station01", rn.table$Station), 3)
    expect_length(grep(pattern = "C02b", rn.table$Camera), 5)
    expect_length(lf, 96)
    expect_length(csv, 1)
    expect_length(dirs_copied, 107)
  })
  unlink(tmp, recursive=TRUE)

} else {
  warning("ExifTool is not available. Cannot test function")
}



