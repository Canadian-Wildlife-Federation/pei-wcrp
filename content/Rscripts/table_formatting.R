library("flextable")
library("magrittr")
library("officer")
library("dplyr")

std_border <- fp_border(color = "grey")

# Table formatting for csv's
format_flextable <- function(ft) {
  ft <- ft %>%
    colformat_num(big.mark = "", decimal.mark = ".", digits = 2) %>%
    bg(bg = "#008270", part = "header") %>%
    color(color = "white", part = "header") %>%
    set_caption() %>%
    align_text_col(align = "left", header = TRUE) %>%
    align_nottext_col(align = "left", header = TRUE) %>%
    vline(part = "all", border = std_border) %>%
    hline(part = "all", border = std_border) %>%
    autofit()
  return(ft)
}

# Table sorting for priority tables
create_sorted_flextable <- function(df) {

  column_name <- names(df)[8]

  sorted_df <- df %>%
    arrange(
      desc(!is.na(!!sym(column_name))),
      desc(!!sym(column_name))
    )

  # Step 2: Create a flextable
  ft <- flextable(sorted_df)

  # Return the flextable
  return(ft)
}