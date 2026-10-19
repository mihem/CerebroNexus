test_that("late initial navigation preserves reader intent", {
  expect_node_script(c(
      "const fs=require('fs'),vm=require('vm'),assert=require('assert');",
      sprintf(
        "const source=fs.readFileSync(%s,'utf8');",
        encodeString(viewer_test_path("www", "viewer-shell.js"), quote = '"')
      ),
      "const code=source.slice(source.indexOf('  function installInitialNavigation()'),source.indexOf('  ready(installInitialNavigation);'));",
      "function session(connected=true){",
      " const events={}, handlers={}, selected=[]; let onConnect;",
      " const link={getAttribute:()=> 'overview'};",
      " const document={addEventListener:(name,fn)=>events[name]=fn,querySelectorAll:()=>[link]};",
      " const window={jQuery:target=>({on:(name,fn)=>onConnect=fn,tab:()=>selected.push(target)})};",
      " const Shiny={addCustomMessageHandler:(name,fn)=>handlers[name]=fn};",
      " if(connected)window.Shiny=Shiny;",
      " vm.runInNewContext(code+';installInitialNavigation();',{window,document});",
      " return {selected,send:value=>handlers.cerebro_initial_page(value),click:nav=>events.click({target:{closest:()=>nav?link:null}}),connect:()=>{window.Shiny=Shiny;onConnect();}};",
      "}",
      "let s=session();s.send({selected:'overview'});assert.strictEqual(s.selected.length,1);",
      "s=session();s.click(false);s.send({selected:'overview'});assert.strictEqual(s.selected.length,1);",
      "s=session();s.click(true);s.send({selected:'overview'});assert.strictEqual(s.selected.length,0);",
      "s=session(false);s.click(true);s.connect();s.send({selected:'overview'});assert.strictEqual(s.selected.length,0);",
      "s=session();s.send({selected:'missing'});s.send(null);assert.strictEqual(s.selected.length,0);"
  ))
})
