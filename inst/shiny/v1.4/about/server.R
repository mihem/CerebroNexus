##----------------------------------------------------------------------------##
## Tab: About.
##----------------------------------------------------------------------------##

##
output[["about"]] <- renderText({
  paste0(
    '<b>Version of cerebroAppLite</b><br>
    v1.7.2<br>
    <br>
    <b>Authors</b><br>
    Roman Hillje<br>
   Michael Heming<br>
   Xuesong Wang<br>
    <br>
    <b>Links</b><br>
    <ul>
      <li><a href=https://github.com/mihem/cerebroAppLite title="Lightweight contination of cerebroApp (Michael Heming)" target="_blank"><b> Lightweight continuation of cerebroApp. (Michael Heming)</b></a></li>
      <li><a href=https://github.com/romanhaa/Cerebro title="Discontinued Cerebro repository on GitHub (Roman Hillje)" target="_blank"><b>Discontinued Cerebro repository on GitHub (Roman Hillje)</b></a></li>
    </ul>
    <br>
    <b>Citation</b><br>
    If you used Cerebro for your research, please cite the following publication:
    <br>
    Roman Hillje, Pier Giuseppe Pelicci, Lucilla Luzi, Cerebro: Interactive visualization of scRNA-seq data, Bioinformatics, btz877, <a href=https://doi.org/10.1093/bioinformatics/btz877 title="DOI" target="_blank">https://doi.org/10.1093/bioinformatics/btz877</a><br>
    <br>
    <b>License</b><br>
    cerebroAppLite is distributed under the terms of the <a href=https://github.com/mihem/cerebroAppLite/blob/master/LICENSE.md title="MIT license" target="_blank">MIT license.</a><br>
    <br>
    <b>Credit where credit is due</b><br>
    <ul>
      <li>Color palettes were built using colors from <a href="https://flatuicolors.com/" title="Flat UI Colors 2" target="_blank">https://flatuicolors.com/</a></li>
    </ul>
    <br>'
  )
})
