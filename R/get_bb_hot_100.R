#' Scrape Billboard Hot 100
#'
#' Scrape Billboard.com's Hot 100 songs for a given week.
#'
#' @param bb_date
#' @param .bow_session Optional bow session for more polite scraping
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun {
#' get_bb_hot_100(bb_date = as.Date('1958-08-04'))
#' }
#'

get_bb_hot_100 <- function(bb_date, .bow_session = NULL) {

    billboard_url <- paste0("https://www.billboard.com/charts/hot-100/", format(bb_date, "%Y-%m-%d"))

    if(rvest::is.session(bow_session)) {
        billboard_page <- polite::nod(bow_session, path = billboard_url) %>%
            polite::scrape()
    } else {
        billboard_page <- polite::bow(billboard_url) %>%
            polite::scrape()
    }

    rank <- billboard_page %>%
        rvest::html_node("ol.chart-list__elements") %>%
        rvest::html_nodes("span.chart-element__rank__number") %>%
        rvest::html_text() %>%
        stringr::str_remove_all("\\D") %>%
        readr::parse_integer()

    song <- billboard_page %>%
        rvest::html_node("ol.chart-list__elements") %>%
        rvest::html_nodes("span.chart-element__information__song") %>%
        rvest::html_text() %>%
        stringr::str_squish()

    artist <- billboard_page %>%
        rvest::html_node("ol.chart-list__elements") %>%
        rvest::html_nodes("span.chart-element__information__artist") %>%
        rvest::html_text() %>%
        stringr::str_squish()

    last_week <- billboard_page %>%
        rvest::html_node("ol.chart-list__elements") %>%
        rvest::html_nodes("span.chart-element__meta.text--last") %>%
        rvest::html_text() %>%
        stringr::str_remove_all("\\D") %>%
        readr::parse_integer()

    peak <- billboard_page %>%
        rvest::html_node("ol.chart-list__elements") %>%
        rvest::html_nodes("span.chart-element__meta.text--peak") %>%
        rvest::html_text() %>%
        stringr::str_remove_all("\\D") %>%
        readr::parse_integer()

    duration <- billboard_page %>%
        rvest::html_node("ol.chart-list__elements") %>%
        rvest::html_nodes("span.chart-element__meta.text--week") %>%
        rvest::html_text() %>%
        stringr::str_remove_all("\\D") %>%
        readr::parse_integer()

    tibble::tibble(rank = rank,
                   song = song,
                   artist = artist,
                   last_week_rank = last_week,
                   peak_rank = peak,
                   weeks_at_rank = duration)
}
