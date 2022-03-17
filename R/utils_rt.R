
rt_aggregate_percent <- function(numer, denom = "Total") {
  reactable::JS(
    glue::glue(
      "
    function(values, rows) {{
      var numer = 0;
      var denom = 0;
      rows.forEach(function(row) {{
        numer += row['{numer}'];
        denom += row['{denom}'];
      }});
      var prop = numer / denom * 100;
      return prop.toFixed(1);
    }}
    "
    )
  )
}

rt_footer_percent <- function(numer, denom = "Total") {
  reactable::JS(
    glue::glue(
      "
    function(colInfo) {{
      var numer = 0;
      var denom = 0;
      colInfo.data.forEach(function(row) {{
        numer += row['{numer}'];
        denom += row['{denom}'];
      }});
      var prop = numer / denom * 100;
      return prop.toFixed(1);
    }}
    "
    )
  )
}

rt_footer_attack_rate <- function(numer = "n", denom = "population", mult = 10000) {
  reactable::JS(
    glue::glue(
      "
    function(colInfo) {{
      var numer = 0;
      var denom = 0;
      colInfo.data.forEach(function(row) {{
        pop = row['{denom}'];
        if (pop !== 'NA') {{
          numer += row['{numer}'];
          denom += row['{denom}'];
        }}
      }});
      var prop = (numer / denom) * {mult};
      return prop.toFixed(1);
    }}
    "
    )
  )
}

rt_unique_vals <- function() {
  reactable::JS("
      function(values, rows) {
        return Array.from(new Set(values)).join(', ')
      }
    ")
}
