test_that("late choices-only updates preserve the latest gene selection", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- tempfile(fileext = ".js")
  withr::defer(unlink(runner))
  writeLines(
    c(
      "const assert = require('assert'), fs = require('fs');",
      "const pending = []; let installs = 0;",
      "const selectize = {items:['CD3D'], options:{CD3D:{value:'CD3D'}},",
      " getValue(){return this.items.slice();},",
      " addOption(option){this.options[option.value]=option;},",
      " setValue(value){this.items=value.filter(v=>this.options[v]);}};",
      "const element = {id:'expression_genes_input',selectize};",
      "const binding = {name:'shiny.selectInput', receiveMessage(el,data){",
      " if(data.url){el.selectize.items=[];el.selectize.options={};",
      " pending.push(()=>{el.selectize.addOption({value:'MS4A1'});",
      " if(data.value!==undefined)el.selectize.setValue(data.value);});",
      " }else if(data.value!==undefined)el.selectize.setValue(data.value);",
      " return 'original-result';}};",
      "global.window=global; global.Shiny={inputBindings:{getBindings:()=>[{binding}]}};",
      "global.document={addEventListener:(event,fn)=>{installs++;fn();}};",
      sprintf(
        "eval(fs.readFileSync(%s,'utf8'));",
        encodeString(
          viewer_test_path("www", "gene-selector.js"),
          quote = '"'
        )
      ),
      "assert.strictEqual(installs,1);",
      "assert.strictEqual(binding.receiveMessage(element,{url:'choices'}),'original-result');",
      "assert.deepStrictEqual(selectize.items,['CD3D']);",
      "selectize.addOption({value:'NKG7'});selectize.setValue(['NKG7']);",
      "pending.shift()(); assert.deepStrictEqual(selectize.items,['NKG7']);",
      "binding.receiveMessage(element,{url:'choices'});selectize.setValue([]);",
      "pending.shift()();assert.deepStrictEqual(selectize.items,[]);",
      "selectize.addOption({value:'CD3D'});selectize.setValue(['CD3D']);",
      "binding.receiveMessage(element,{url:'older'});",
      "binding.receiveMessage(element,{url:'newer',value:['MS4A1']});",
      "pending.pop()();pending.shift()();",
      "assert.deepStrictEqual(selectize.items,['MS4A1']);",
      "binding.receiveMessage(element,{value:[]});assert.deepStrictEqual(selectize.items,[]);",
      "selectize.addOption({value:'CD3D'});selectize.setValue(['CD3D']);",
      "binding.receiveMessage(element,{url:'dataset-change',value:[]});pending.shift()();",
      "assert.deepStrictEqual(selectize.items,[]);",
      "element.id='expression_rgb_gene_r';selectize.addOption({value:'CD3D'});selectize.setValue(['CD3D']);",
      "binding.receiveMessage(element,{url:'other-control'});pending.shift()();",
      "assert.deepStrictEqual(selectize.items,[]);"
    ),
    runner
  )
  expect_identical(system2("node", runner), 0L)
})
