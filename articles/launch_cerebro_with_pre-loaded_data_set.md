# Launch Cerebro with pre-loaded data set

## Overview

CerebroNexus can preload a `.crb` file and prevent users from uploading
their own data sets. This can be useful when you want to share a Cerebro
data set on a web server, for example alongside a publication, but don’t
want users to upload data to that server.

## How to do it

Provide the data path and closed mode to
[`launchCerebro()`](https://mihem.github.io/CerebroNexus/reference/launchCerebro.md).
Let’s say your data set is stored here:
`~/Cerebro/data_sets/pbmc_v3.crb`. Then, launch Cerebro as shown below:

``` r
launchCerebro(
  crb_file_to_load = "~/Cerebro/data_sets/pbmc_v3.crb",
  mode = "closed"
)
```

That’s it.

[![Load
data](launch_cerebro_with_pre-loaded_data_set_files/landing_page.png)](https://mihem.github.io/CerebroNexus/articles/launch_cerebro_with_pre-loaded_data_set_files/landing_page.png)

## Modify welcome page

If you would like to modify the message on the “Load data”/welcome page,
e.g. to link to a publication or give an introduction to the data set,
you can use the `welcome_message` parameter of
[`launchCerebro()`](https://mihem.github.io/CerebroNexus/reference/launchCerebro.md).
The provided string for that parameter can/should be written in HTML as
shown below:

``` r
custom_welcome_message <- '<h3 style="text-align: center; margin-top: 0px"><strong>This is a custom welcome message to Cerebro</strong></h3>
  <p style="margin-left: 10px;">This data set belongs to publication XY by Max Mustermann <em>et al.</em>.<br>
  It contains the transcriptomic profiles of PBMC samples taken from mice undergoing treatment F.<br>
  Please contact us at ... if you have any questions.</p>'

launchCerebro(
  crb_file_to_load = "~/Cerebro/data_sets/pbmc_v3.crb",
  mode = "closed",
  welcome_message = custom_welcome_message
)
```

[![Load
data](launch_cerebro_with_pre-loaded_data_set_files/custom_welcome_message.png)](https://mihem.github.io/CerebroNexus/articles/launch_cerebro_with_pre-loaded_data_set_files/custom_welcome_message.png)

## See also

- [Host Cerebro on
  shinyapps.io](https://mihem.github.io/CerebroNexus/articles/host_cerebro_on_shinyapps.md)
- [Control access to Cerebro with a login
  page](https://mihem.github.io/CerebroNexus/articles/control_access_to_cerebro_with_a_login_page.md)
