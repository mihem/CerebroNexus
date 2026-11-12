// The address-bar handlers of viewer-shell.js: cerebro_page_url writes ?page=,
// cerebro_dataset_url writes ?dataset=. Run with the path of viewer-shell.js.
const assert = require('assert');
const fs = require('fs');
const vm = require('vm');
const source = fs.readFileSync(process.argv[2], 'utf8');
const code = source.slice(
  source.indexOf('  function installInitialNavigation()'),
  source.indexOf('  ready(installInitialNavigation);')
);

// A browser stand-in: the address bar is `href`; replaceState rewrites it and
// is the only way the handlers may change it (no navigation, no new entries).
function browser(href) {
  const handlers = {}, writes = [];
  const state = {id: 'kept'};
  const window = {
    location: {get href() { return window.__href; }},
    __href: href,
    history: {
      state,
      replaceState: (s, title, url) => {
        writes.push({state: s, title, url});
        window.__href = url;
      }
    },
    Shiny: {addCustomMessageHandler: (name, fn) => {handlers[name] = fn;}}
  };
  const document = {addEventListener: () => {}, querySelectorAll: () => []};
  vm.runInNewContext(code + ';installInitialNavigation();', {window, document, URL});
  return {
    handlers, writes, state,
    get url() {return new URL(window.__href);},
    send: (name, message) => handlers[name](message)
  };
}

// Both handlers are registered under the names the server sends.
{
  const b = browser('http://host/app/');
  assert.deepStrictEqual(
    ['cerebro_dataset_url', 'cerebro_page_url'],
    Object.keys(b.handlers).filter(n => /_url$/.test(n)).sort()
  );
}

// ?page= is set, other parameters stay, history is replaced not extended.
{
  const b = browser('http://host/app/?dataset=TCR&token=abc');
  b.send('cerebro_page_url', {page: 'spatial'});
  assert.strictEqual(b.url.searchParams.get('page'), 'spatial');
  assert.strictEqual(b.url.searchParams.get('dataset'), 'TCR');
  assert.strictEqual(b.url.searchParams.get('token'), 'abc');
  assert.strictEqual(b.writes.length, 1);
  assert.strictEqual(b.writes[0].state, b.state);
  assert.strictEqual(b.writes[0].title, '');
  b.send('cerebro_page_url', {page: 'groups'});
  assert.strictEqual(b.url.searchParams.getAll('page').length, 1);
  assert.strictEqual(b.url.searchParams.get('page'), 'groups');
}

// A malformed message changes nothing.
{
  const b = browser('http://host/app/?page=groups');
  for (const bad of [null, undefined, {}, {page: 3}, {page: null}]) {
    b.send('cerebro_page_url', bad);
  }
  for (const bad of [null, undefined, {}, {dataset: 3}, {dataset: null}]) {
    b.send('cerebro_dataset_url', bad);
  }
  assert.strictEqual(b.writes.length, 0);
}

// ?dataset= is set on a plain URL and keeps the page and other parameters.
{
  const b = browser('http://host/app/?page=groups&token=abc');
  b.send('cerebro_dataset_url', {dataset: 'sample 2'});
  assert.strictEqual(b.url.searchParams.get('dataset'), 'sample 2');
  assert.strictEqual(b.url.searchParams.get('page'), 'groups');
  assert.strictEqual(b.url.searchParams.get('token'), 'abc');
  assert.strictEqual(b.url.pathname, '/app/');
}

// A /name path that already names the data set is left alone on first load,
// but a ?dataset= that disagrees with it is overwritten. The path never moves.
{
  const b = browser('http://host/app/TCR');
  b.send('cerebro_dataset_url', {dataset: 'TCR'});
  assert.strictEqual(b.writes.length, 0);
  b.send('cerebro_dataset_url', {dataset: 'BCR'});
  assert.strictEqual(b.url.searchParams.get('dataset'), 'BCR');
  assert.strictEqual(b.url.pathname, '/app/TCR');
  b.send('cerebro_dataset_url', {dataset: 'TCR'});
  assert.strictEqual(b.url.searchParams.get('dataset'), 'TCR');
}
{
  const b = browser('http://host/sample%202?dataset=other');
  b.send('cerebro_dataset_url', {dataset: 'sample 2'});
  assert.strictEqual(b.url.searchParams.get('dataset'), 'sample 2');
}
{
  const b = browser('http://host/sample%202');
  b.send('cerebro_dataset_url', {dataset: 'sample 2'});
  assert.strictEqual(b.writes.length, 0);
}

// A malformed percent-escape in the path is not a name and must not stop the
// address bar from following the data set (decodeURIComponent throws on it).
{
  const b = browser('http://host/%E0%A4%A');
  assert.throws(() => decodeURIComponent('%E0%A4%A'), URIError);
  b.send('cerebro_dataset_url', {dataset: 'TCR'});
  assert.strictEqual(b.url.searchParams.get('dataset'), 'TCR');
}

console.log('PASS page and data set write to the address bar, malformed paths included');
