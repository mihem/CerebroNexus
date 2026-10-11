test_that("specialist panels retain binary and JSON expression values", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs'), assert = require('assert');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(viewer_test_path("www", "cell_views.js"), quote = '"')
      ),
      "function take(start, end) { return source.slice(source.indexOf('  function ' + start), source.indexOf('  function ' + end)); }",
      "let D = {n: 3, fields: {}}, spaceById = {}; const FIELD_PREFIX = 'field:';",
      "const singleIndex = () => new Map([['a',0], ['b',1], ['c',2]]);",
      "const emptyVector = value => Array(D.n).fill(value);",
      "const singlePalette = scale => scale;",
      "eval(take('quantisedField', 'directColorField'));",
      "eval(take('alignFlatValues', 'buildSingleSpaces'));",
      "for (const values of [[4, 0, 2], new Float32Array([4, 0, 2])]) {",
      " const result = buildSpecialistPanels('spatial', {meta: {color_type:'continuous', color_variable:'GENE'}, data: {",
      "  color: values, selection_key:['b','a','c'], colorscale:[[0,'#aeb5bb'],[1,'#9f251f']],",
      "  panels:[{id:'one',label:'R1',selection_key:['b','c'],x:[1,3],y:[4,6]},",
      "          {id:'two',label:'R2',selection_key:['a'],x:[2],y:[5]}]}});",
      " const field = D.fields['single:spatial:value'];",
      " assert.deepStrictEqual(Array.from(field.raw), [0,4,2]);",
      " assert.deepStrictEqual(Array.from(field.v), [0,1000,500]);",
      " assert.strictEqual(field.max, 4); assert.strictEqual(result.spaces.length, 2);",
      " assert.deepStrictEqual(spaceById[result.spaces[0]].x, [null,1,3]);",
      " assert.deepStrictEqual(spaceById[result.spaces[1]].x, [2,null,null]);",
      "}"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
})
