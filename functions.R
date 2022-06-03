get_tweet_blockquote <- function(screen_name, status_id, ..., null_on_error = TRUE, theme = "light") {
  oembed <- list(...)$oembed
  if (!is.null(oembed) && !is.na(oembed)) return(unlist(oembed))
  oembed_url <- glue::glue("https://publish.twitter.com/oembed?url=https://twitter.com/{screen_name}/status/{status_id}&omit_script=1&dnt=1&theme={theme}")
  bq <- possibly(httr::GET, list(status_code = 999))(URLencode(oembed_url))
  if (bq$status_code >= 400) {
    if (null_on_error) return(NULL)
    '<blockquote style="font-size: 90%">Sorry, unable to get tweet ¯\\_(ツ)_/¯</blockquote>'
  } else {
    httr::content(bq, "parsed")$html
  }
}

cache_profile_image <- function(profile_image_url, location = "www", default = "twitter-default-profile.jpg") {
  file_serve <- str_replace(profile_image_url, ".+/profile", "profile")
  file_local <- fs::path(location, file_serve)
  if (fs::file_exists(file_local)) {
    x <- list(result = file_serve)
  } else {
    fs::dir_create(fs::path_dir(file_local))
    x <- safely(download.file)(profile_image_url, file_local)
    # On fist download, the image won't be ready for the UI, so show default
    if (is.null(x$error)) x$result <- default
  }
  if (is.null(x$error)) x$result else default
}

masonify_tweets <- function(tweets, id = NULL, class = NULL) {
  stopifnot("status_id" %in% names(tweets))
  
  t_embed <-
    tweets %>%
    pmap(get_tweet_blockquote) %>%
    map(HTML) %>%
    map(tags$div, class = "tweet-item")
  
  tagList(
    tags$div(id = id,
             class = paste("masonry text-left", class),
             t_embed
    )
  )
}

twemoji <- function(runes, width = "20px") {
  runes <- tolower(runes)
  runes <- gsub(" ", "-", runes)
  runes <- sub("-fe0f$", "", runes) # seems to cause problems with twemoji :shrug:
  emojis <- glue::glue("https://cdnjs.cloudflare.com/ajax/libs/twemoji/11.2.0/2/svg/{runes}.svg")
  emojis <- glue::glue('<img src="{emojis}" width = "{width}">')
  paste(emojis)
}

progressGroup = function(text, value, min = 0, max = value, color = "aqua") {
  stopifnot(is.character(text))
  stopifnot(is.numeric(value))
  if (value < min || value > max)
    stop(sprintf("'value' should be in the range from %d to %d.", min, max), call. = FALSE)
  tags$div(
    class = "progress-group",
    tags$span(class = "progress-text", text),
    tags$span(class = "progress-number", sprintf("%d / %d", value, max)),
    progressBar(round(value / max * 100), color = color, size = "sm")
  )
}

progressBar_v = function(x, colors) {
  if (length(colors) > length(x)) {
    colors <- rep(colors, ceiling(length(x)/length(colors)))
  }
  x <- purrr::map2(x, colors[seq_along(x)], ~ progressBar(.x, color = .y))
  map_chr(x, paste)
}

progressBar = function(
    value = 0,
    label = FALSE,
    color = "aqua",
    size = NULL,
    striped = FALSE,
    active = FALSE,
    vertical = FALSE
) {
  stopifnot(is.numeric(value))
  if (value < 0 || value > 100)
    stop("'value' should be in the range from 0 to 100.", call. = FALSE)

  if (!is.null(size))
    size <- match.arg(size, c("sm", "xs", "xxs"))
  text_value <- paste0(value, "%")
  if (vertical)
    style <- htmltools::css(height = text_value, `min-height` = "2em")
  else
    style <- htmltools::css(width = text_value, `min-width` = "2em")
  tags$div(
    class = "progress",
    class = if (!is.null(size)) paste0("progress-", size),
    class = if (vertical) "vertical",
    class = if (active) "active",
    tags$div(
      class = "progress-bar",
      class = paste0("progress-bar-", color),
      class = if (striped) "progress-bar-striped",
      style = style,
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100,
      tags$span(class = if (!label) "sr-only", text_value)
    )
  )
}


