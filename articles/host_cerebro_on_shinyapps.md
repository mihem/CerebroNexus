# Host Cerebro on shinyapps.io

## Overview

In this vignette, I explain how one can host Cerebro on
[shinyapps.io](https://www.shinyapps.io). For a more detailed tutorial
on how to host Shiny apps on shinyapps.io, take a look here:
<https://shiny.rstudio.com/articles/shinyapps.html>

[shinyapps.io](https://www.shinyapps.io) is a great place to host Shiny
apps and make them accessible for other users. You will need an account
on the website. The free account comes with a limit on (1) the number of
applications that can be hosted in parallel and (2) the number of active
hours, but it is enough to get some experience and figure out if this
service fits your purpose.

## Setup

To upload CerebroNexus to shinyapps.io, install `rsconnect` and
CerebroNexus.

``` r
install.packages(c("remotes", "rsconnect"))
remotes::install_github("mihem/CerebroNexus")

library(CerebroNexus)
```

Then, you need to provide `rsconnect` with your shinyapps.io account
info. On the shinyapps.io website, in the left navigation bar, go to
“Account” and then “Tokens”. A token should have already been generated
for you. Take the token and secret, your account name, and run the
command below.

``` r
rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
```

## Build the application

Use
[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
to build a deployable application directory. The example below uses the
data bundled with CerebroNexus; replace `crb` with your own `.crb` path
and choose a label that users will see in the application.

``` r
app_directory <- "~/test_cerebro_shinyapps"
crb <- system.file(
  "extdata/examples/example.crb",
  package = "CerebroNexus"
)

createShinyApp(
  cerebro_data = c("PBMC example" = crb),
  result_dir = app_directory,
  launch_browser = FALSE
)
```

The generated directory contains `app.R`, the Viewer runtime, and the
private data files needed by the deployed application. See
[`vignette("create_a_self_contained_shiny_app")`](https://mihem.github.io/CerebroNexus/articles/create_a_self_contained_shiny_app.md)
for deployment options and external expression-backend requirements.

## Deploy app

Upload the generated application directory with `rsconnect`:

``` r
rsconnect::deployApp(app_directory, appName = "Cerebro")
```

Uploading and preparing the app might take a few minutes, but once it
finishes successfully, Cerebro should open in your browser. Using the
shinyapps.io dashboard, you can manage the app, check usage metrics and
logs, etc. An important parameter is the “Instance size”, which controls
the available memory and can be controled in the “Settings” tab of the
dashboard. The larger the data set you want to upload to Cerebro, the
more memory will be necessary.

## See also

- [Launch Cerebro with pre-loaded data
  set](https://mihem.github.io/CerebroNexus/articles/launch_cerebro_with_pre-loaded_data_set.md)
- [Control access to Cerebro with a login
  page](https://mihem.github.io/CerebroNexus/articles/control_access_to_cerebro_with_a_login_page.md)

## Known issues

- Exporting plots to PDF through the `export to PDF` buttons effectively
  does not work at the moment. That is because it will trigger a file
  selection dialog on the server side, not allowing the user to specify
  a location on their local machine.
