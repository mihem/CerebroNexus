test_that("deferred categories do not block selection and clone completion", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs=require('fs'),vm=require('vm'),assert=require('assert');",
      sprintf(
        "const source=fs.readFileSync(%s,'utf8');",
        encodeString(viewer_test_path("www", "cell_views.js"), quote = '"')
      ),
      "function fn(name){let a=source.indexOf('  function '+name+'(');assert(a>=0);return source.slice(a,source.indexOf('\\n  }',a)+4);}",
      "const loaded={levels:['A','B'],values:new Int8Array([0,1])};",
      "const pending={levels:['X'],values:null,deferred:true};",
      "const c={D:{n:2,groups:{cell_type:pending,cluster:loaded},cat_extra:{}},colorBy:'cluster'};vm.createContext(c);",
      "vm.runInContext(fn('catOf')+fn('compGroupName'),c);",
      "assert.equal(c.catOf('cell_type'),null);assert.equal(c.compGroupName(),'cluster');",
      "c.colorBy='__rgb';assert.equal(c.compGroupName(),'cluster');",
      "pending.values=new Int8Array([0,0]);assert.equal(c.compGroupName(),'cell_type');",
      "pending.values=null;c.D.groups.cluster.values=null;assert.equal(c.compGroupName(),null);",
      "Object.assign(c,{datasetMessageFresh:()=>true,sameDatasetContext:(a,b)=>a===b,configFingerprint:()=> 'fp',performance:{now:()=>0},applyCloneSupplement:()=>true,reportWorkspaceReady:()=>{c.ready=!c.D.progressive},CBViewState:{telemetry:{snapshot:()=>({})}},transportMetrics:{},pendingCloneSupplement:null,requestWireFallback:()=>{throw Error('unexpected fallback')}});",
      "vm.runInContext(fn('applyHydratedSupplement'),c);",
      "Object.assign(c.D,{dataset_id:'d',dataset_context:'ctx',progressive_token:'p',progressive:true});",
      "const extra={dataset_id:'d',dataset_context:'ctx',dataset_fingerprint:'fp',progressive_token:'p',clone:{},spaces:[{id:'clone'}]};",
      "c.applyHydratedSupplement({...extra,progressive_complete:false},{});assert.equal(c.ready,false);",
      "c.applyHydratedSupplement({...extra,dataset_context:'old'},{});assert.equal(c.D.progressive,true);",
      "c.applyHydratedSupplement(extra,{});assert.equal(c.ready,true);assert.equal(c.D.progressive,false);",
      "console.log('linked deferred regression passed');"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
})
