test_that("progress updates preserve the actionable Stop button", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- withr::local_tempfile(fileext = ".js")
  writeLines(
    c(
      "const fs = require('fs'), assert = require('assert');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(
          builder_test_inst_path("builder", "www", "builder.js"),
          quote = '"'
        )
      ),
      "const start = source.indexOf('  function setBuilderOperationActions(');",
      "const end = source.indexOf('  function cancelBuilderProjectOpen(', start);",
      "const actions = {children: [], replaceChildren(){this.children=[];}, appendChild(x){this.children.push(x);}, setAttribute(){}};",
      "const card = {classList:{toggle(){}}};",
      "function builderOperationElements(){return {actions, card};}",
      "const document = {createElement(){return {addEventListener(name, fn){this[name]=fn;}};}};",
      "eval(source.slice(start, end));",
      "let stops = 0; const stop = () => stops++;",
      "setBuilderOperationActions([{label:'Stop', action:stop}]);",
      "const button = actions.children[0]; button.focused = true;",
      "for(let i=0;i<100;i++) setBuilderOperationActions([{label:'Stop', action:stop}]);",
      "assert.strictEqual(actions.children[0], button); assert.strictEqual(button.focused, true);",
      "button.click(); assert.strictEqual(stops, 1);",
      "button.disabled=true; button.textContent='Stopping…';",
      "setBuilderOperationActions([{label:'Stop', action:stop}]);",
      "assert.strictEqual(button.textContent,'Stopping…'); assert.strictEqual(button.disabled,true);",
      "setBuilderOperationActions([]); assert.strictEqual(actions.children.length,0);",
      "setBuilderOperationActions([{label:'Stop', action:stop}]); assert.notStrictEqual(actions.children[0],button);"
    ),
    runner
  )
  result <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
})

test_that("Save captures an unblurred numeric display edit before its action", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- withr::local_tempfile(fileext = ".js")
  writeLines(
    c(
      "const fs = require('fs'), assert = require('assert');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(
          builder_test_inst_path("builder", "www", "builder.js"),
          quote = '"'
        )
      ),
      "const start = source.indexOf('  function flushViewerDisplay(');",
      "const firstClick = source.indexOf('  document.addEventListener(\"click\"', start);",
      "const end = source.indexOf('  document.addEventListener(\"click\"', firstClick + 1);",
      "const events = [], sent = [], status = {}; let valid = true, reported = 0;",
      "const field = {dataset:{displayField:'hla_node_scale'},type:'number',value:'1.7',checkValidity(){return valid;},reportValidity(){reported++;}};",
      "const root = {dataset:{datasetId:'ds1',projectEpoch:'4',inputId:'appearance'},querySelectorAll(){return [field];},querySelector(){return status;}};",
      "const document = {addEventListener(type,fn,capture){events.push({type,fn,capture});},querySelector(){return root;}};",
      "function send(name,value){sent.push({name,value});return true;}",
      "eval(source.slice(start,end));",
      "const click = events.find(e=>e.type==='click' && e.capture).fn;",
      "let stopped = false; const event = {target:{closest(){return {disabled:false};}},preventDefault(){stopped=true;},stopImmediatePropagation(){stopped=true;}};",
      "click(event); assert.strictEqual(sent.length,1); assert.strictEqual(sent[0].value.values.hla_node_scale,1.7);",
      "assert.strictEqual(sent[0].value.id,'ds1'); assert.strictEqual(sent[0].value.epoch,4); assert.strictEqual(stopped,false);",
      "click(event); assert.strictEqual(sent.length,1, 'unchanged Save must not submit another appearance edit');",
      "valid=false; click(event); assert.strictEqual(sent.length,1); assert.strictEqual(stopped,true); assert.strictEqual(reported,1);"
    ),
    runner
  )
  result <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
})

test_that("Save success is nonmodal and failure preserves an actionable retry", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- withr::local_tempfile(fileext = ".js")
  writeLines(
    c(
      "const fs=require('fs'), assert=require('assert');",
      sprintf(
        "const source=fs.readFileSync(%s,'utf8');",
        encodeString(
          builder_test_inst_path("builder", "www", "builder.js"),
          quote = '"'
        )
      ),
      "const start=source.indexOf('  function showBuilderProjectSaveCompletion('), end=source.indexOf('  function updateBuilderProjectSourceProgress(',start);",
      "let builderProjectSaveResultOpen=true, builderProjectSaveResult={checked:3,reusable:1}, closed=0,buttons=[],sent=[],timeout=null,notice=null;",
      "const displayStatus={textContent:''},card={classList:{add(){},remove(){}}},actions={querySelector(){return {focus(){}};}};",
      "const document={querySelectorAll(){return [displayStatus];},getElementById(){return null;},createElement(){return {setAttribute(k,v){this[k]=v;},remove(){this.removed=true;}};},body:{appendChild(el){notice=el;}}};",
      "const window={setTimeout(fn,ms){timeout={fn,ms};}};",
      "function builderOperationElements(){return {card,actions};}",
      "function closeBuilderProjectSaveResult(){closed++;builderProjectSaveResultOpen=false;}",
      "function setBuilderOperationCopy(){} function setBuilderOperationActions(value){buttons=value;} function send(name,value){sent.push(name);}",
      "let flashes=[]; function showStatusFlash(message){flashes.push(message);}",
      "eval(source.slice(start,end));",
      "showBuilderProjectSaveCompletion('ready');",
      "assert.strictEqual(closed,1); assert.strictEqual(builderProjectSaveResultOpen,false); assert.strictEqual(buttons.length,0);",
      "assert.strictEqual(notice,null); assert.strictEqual(flashes.length,1); assert.strictEqual(flashes[0].type,'success'); assert(flashes[0].text.includes('2 dataset caches')); assert(flashes[0].hide_after > 0);",
      "builderProjectSaveResultOpen=true; notice=null; timeout=null; flashes=[]; showBuilderProjectSaveCompletion('failed');",
      "assert.strictEqual(builderProjectSaveResultOpen,true); assert.strictEqual(notice,null); assert.strictEqual(flashes.length,0); assert.strictEqual(buttons.length,2);",
      "buttons.find(x=>x.primary).action(); assert.strictEqual(builderProjectSaveResultOpen,false); assert.deepStrictEqual(sent,['save_builder_project']);"
    ),
    runner
  )
  result <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
})
