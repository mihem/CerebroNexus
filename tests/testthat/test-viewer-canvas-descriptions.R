test_that("cloned panels own distinct accessible descriptions", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- withr::local_tempfile(fileext = ".js")
  writeLines(
    c(
      "const fs=require('fs'), assert=require('assert');",
      sprintf(
        "const source=fs.readFileSync(%s,'utf8');",
        encodeString(viewer_test_path("www", "cell_views.js"), quote = '"')
      ),
      "const nodes={}; const document={getElementById:id=>nodes[id]||null, createElement:()=>({})};",
      "const D={n:180}, sel=new Set(), spaceById={a:{label:'UMAP'},b:{label:'Clonal expansion'}};",
      "function panel(id,space){const parent={insertBefore(node){node.parentNode=this;nodes[node.id]=node;}};return {spaceId:space,canvas:{id,parentNode:parent,attrs:{'aria-describedby':'cv-a-description'},setAttribute(k,v){this.attrs[k]=v;},getAttribute(k){return this.attrs[k];}}};}",
      "eval(source.slice(source.indexOf('  function updateCanvasDescription('),source.indexOf('  function cssEscape(')));",
      "const first=panel('cv-cv-a','a'); const initial={parentNode:first.canvas.parentNode,textContent:'waiting'}; nodes['cv-description-a']=initial; prepareCanvasSemantics(first);",
      "assert.strictEqual(first.description,initial);",
      "const clone=panel('cv-cv-b','b'); const cloned={parentNode:clone.canvas.parentNode,textContent:initial.textContent}; nodes['cv-description-b']=cloned; prepareCanvasSemantics(clone);",
      "assert.strictEqual(clone.description,cloned);",
      "assert.notStrictEqual(first.description,clone.description);",
      "assert.strictEqual(first.description.textContent,'UMAP view with 180 points; 0 selected.');",
      "assert.strictEqual(clone.description.textContent,'Clonal expansion view with 180 points; 0 selected.');",
      "assert.strictEqual(clone.canvas.attrs['aria-describedby'],'cv-description-b');",
      "const same=clone.description;prepareCanvasSemantics(clone);assert.strictEqual(clone.description,same);"
    ),
    runner
  )
  result <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
})
