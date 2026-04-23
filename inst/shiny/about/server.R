##----------------------------------------------------------------------------##
## Tab: About.
##----------------------------------------------------------------------------##

##
output[["about"]] <- renderText({
  paste0(
    '<b>Version of cerebroApp</b><br>
    v1.5.4<br>
    <br>
    <b>Author</b><br>
    Roman Hillje<br>
    Michael Heming <br>
    Xuesong Wang <br>
    <br>
    <b>Links</b><br>
    <ul>
      <li><a href=https://github.com/romanhaa/Cerebro title="Discontinued Cerebro repository on GitHub (Roman Hillje)" target="_blank"><b>Discontinued Cerebro repository on GitHub (Roman Hillje)</b></a></li>
      <li><a href=https://github.com/mihem/cerebroApp title="Fork of Cerebro App with the most recent version (Michael Heming)" target="_blank"><b>Fork of Cerebro App with the most recent version (Michael Heming)</b></a></li>
    </ul>
    <br>
    <b>Citation</b><br>
    If you used Cerebro for your research, please cite the following publication:
    <br>
    Roman Hillje, Pier Giuseppe Pelicci, Lucilla Luzi, Cerebro: Interactive visualization of scRNA-seq data, Bioinformatics, btz877, <a href=https://doi.org/10.1093/bioinformatics/btz877 title="DOI" target="_blank">https://doi.org/10.1093/bioinformatics/btz877</a><br>
    <br>
    <b>License</b><br>
    Cerebro is distributed under the terms of the <a href=https://github.com/romanhaa/Cerebro/blob/master/LICENSE.md title="MIT license" target="_blank">MIT license.</a><br>
    <br>
    <b>Credit where credit is due</b><br>
    <ul>
      <li>Color palettes were built using colors from <a href="https://flatuicolors.com/" title="Flat UI Colors 2" target="_blank">https://flatuicolors.com/</a></li>
    </ul>
    <br>'
  )
})


##
output[["logo_Cerebro"]] <- renderImage({
  list(
    src = paste0(Cerebro.options$cerebro_root, '/extdata/logo_Cerebro.png'),
    contentType = 'image/png',
    width = 350,
    height = 405,
    alt = "Cerebro logo",
    align = "right"
  )},
  deleteFile = FALSE
)

##
output[["about_footer"]] <- renderText({
  paste0(
    '<br>
    <div class="text-center">
      <a target="_blank" href="https://www.twitter.com/fakechek1"><i class="fab fa-twitter" style="color: rgba(0,0,0,.44); font-size: 4rem; margin-left: 10px" aria-hidden="true"></i></a>
      <a target="_blank" href="https://github.com/romanhaa"><i class="fab fa-github" style="color: rgba(0,0,0,.44); font-size: 4rem; margin-left: 10px" aria-hidden="true"></i></a>
      <a target="_blank" href="https://gitlab.com/romanhaa"><i class="fab fa-gitlab" style="color: rgba(0,0,0,.44); font-size: 4rem; margin-left: 10px" aria-hidden="true"></i></a>
      <a target="_blank" href="https://hub.docker.com/u/romanhaa"><i class="fab fa-docker" style="color: rgba(0,0,0,.44); font-size: 4rem; margin-left: 10px" aria-hidden="true"></i></a>
      <a target="_blank" href="https://linkedin.com/in/roman.hillje"><i class="fab fa-linkedin" style="color: rgba(0,0,0,.44); font-size: 4rem; margin-left: 10px" aria-hidden="true"></i></a>
    </div>'
  )
})
