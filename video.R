create_frame <- function(folder_frames, folder_contours, folder_concentrations, date, frequency="hour", force=F){

    time_suffix_formats <- list(
      hour="%Y%m%d%H",
      day="%Y%m%d"
    )
    time_suffix <- format(date, time_suffix_formats[[frequency]])
    file_frame <- file.path(folder_frames, sprintf("frame_%s.jpg", time_suffix))
    file_map <- file.path(folder_contours, sprintf("contour_%s.jpg", time_suffix))
    file_plot <- file.path(folder_concentrations, sprintf("concentration_%s_%s.jpg", frequency, time_suffix))

    if(file.exists(file_frame) & !force){
      return(file_frame)
    }

    if(!file.exists(file_plot) | !file.exists(file_map)){
      return(NULL)
    }

    # Use image magick to put the two images together (map and plot)
    img_map <- image_read(file_map)
    img_plot <- image_read(file_plot)

    # Scale so that map and plot are the same width
    ratio <- image_info(img_plot)$width / image_info(img_map)$width
    new_height <- image_info(img_map)$height * ratio
    new_width <- image_info(img_map)$width * ratio
    img_map <- image_scale(img_map, glue("{new_width}x{new_height}"))

    img <- image_append(c(img_map, img_plot), stack=T)
    height <- image_info(img)$height
    width <- image_info(img)$width

    # If height or width is an odd number, resize
    # if(height %% 2 != 0){
    #   height <- height + 1
    # }
    # if(width %% 2 != 0){
    #   width <- width + 1
    # }
    #
    #
    # # Double the size to be sure it's divisible by 2
    # magick::image_write(image_resize(img, glue("{width}x{height}")), file_frame)
    magick::image_write(img, file_frame)
    gc()
}

create_frames <- function(folder_frames,
                          folder_contours,
                          folder_concentrations,
                          date_from, date_to=Sys.time(), frequency="hour", force=F){

    if(frequency == "hour"){
        dates <- seq.POSIXt(as.POSIXct(date_from), as.POSIXct(date_to), by="hour")
    } else if(frequency == "day"){
        dates <- seq.POSIXt(as.Date(date_from), as.Date(date_to), by="day")
    } else {
        stop("frequency must be 'hour' or 'day'")
    }

    pbapply::pblapply(dates, create_frame,
                      folder_frames=folder_frames,
                      folder_contours=folder_contours,
                      folder_concentrations=folder_concentrations,
                      frequency=frequency,
                      force=force)
}


create_videos <- function(folder_frames, folder_video, folder_contours, basename="video"){

    # Version with both contours and concentrations
    filepath_video <- file.path(folder_video, paste0(basename, '.mp4'))
    # av::av_encode_video(
    #   # sort files by name
    #   list.files(folder_frames, '*.jpg', full.names=T) %>% sort(),
    #   vfilter="pad=width=ceil(iw/2)*2:height=ceil(ih/2)*2",
    #   framerate = 18,
    #   output = filepath_video)

    # Use command instead
    args <- list(
      framerate=24,
      pattern_type="glob",
      vfilter="pad=width=ceil(iw/2)*2:height=ceil(ih/2)*2",
      pix_fmt="yuv420p",
      c_v="libx264"
    )

    system(glue("ffmpeg -y -framerate {args$framerate} -pattern_type {args$pattern_type} -i \"{folder_frames}/*.jpg\" -vf \"{args$vfilter}\" -pix_fmt {args$pix_fmt}  -c:v {args$c_v} {filepath_video}"))


    # Create a version with contours only
    filepath_video_contours <- file.path(folder_video, paste0(basename, '_contours.mp4'))
    system(glue("ffmpeg -y -framerate {args$framerate} -pattern_type {args$pattern_type} -i \"{folder_contours}/*.jpg\" -vf \"{args$vfilter}\" -pix_fmt {args$pix_fmt}  -c:v {args$c_v} {filepath_video_contours}"))

    filepath_video_contours_wlogo <- file.path(folder_video, paste0(basename, '_contours_withlogo.mp4'))
    system(glue("ffmpeg -y -i {filepath_video_contours} -i data/crea_logo_negative.png -filter_complex \"[1:v]scale=300:-1[logo];[0:v][logo]overlay=W-w-10:H-h-10\" {filepath_video_contours_wlogo}"))
}
