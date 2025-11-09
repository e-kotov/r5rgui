# --- Setup a real r5r network for testing new features ---
# This is needed to test automatic bounding box calculation.
# It will be skipped if r5r is not installed anyway.
if (requireNamespace("r5r", quietly = TRUE)) {
  data_path <- system.file("extdata/poa", package = "r5r")
  if (utils::packageVersion("r5r") >= "2.3.0") {
    r5r_net <- r5r::build_network(data_path)
  } else {
    r5r_net <- r5r::setup_r5(data_path)
  }
}
