// Dependency-free generated-operation fixture. Requires JSON.parse reviver source
// context (Node >=22); a production browser must support it or use a lossless parser.
import assert from 'node:assert/strict';
const [mount, queue] = process.argv.slice(2);
const lossless = text => JSON.parse(text, (_key, value, context) => {
  if (typeof value === 'number' && Number.isInteger(value) && !Number.isSafeInteger(value)) {
    assert.ok(context?.source, 'exact source token support required');
    return BigInt(context.source);
  }
  return value;
});
const requested = [];
async function read(url) {
  requested.push(new URL(url).pathname);
  const response = await fetch(url);
  assert.equal(response.status, 200, await response.clone().text());
  return lossless(await response.text());
}
const descriptor = await read(mount);
assert.equal(descriptor.openapi_path, 'openapi.json');
assert.equal(descriptor.websocket_path, 'ws');
const specUrl = new URL(descriptor.openapi_path, mount);
const spec = await read(specUrl);
assert.equal(spec.servers[0].url, '.');
const base = new URL(spec.servers[0].url, specUrl);
const client = {};
for (const [path, methods] of Object.entries(spec.paths)) {
  for (const [method, operation] of Object.entries(methods)) {
    if (method !== 'get') continue;
    client[operation.operationId] = (params = {}, query = {}) => {
      const rendered = path.replace(/\{([^}]+)\}/g, (_, name) => encodeURIComponent(String(params[name])));
      const url = new URL(rendered.replace(/^\//, ''), base);
      url.search = new URLSearchParams(query).toString();
      return read(url);
    };
  }
}
const queues = await client.listQueues();
assert.ok(queues.some(q => q.queue_name === queue));
const page = await client.peekMessages({queue}, {limit: 1});
assert.equal(page.items[0].msg_id, 9007199254740993n);
assert.equal(page.next_cursor, 9007199254740993n);
const last = await client.peekMessages({queue}, {from: page.next_cursor.toString()});
assert.equal(last.items[0].msg_id, 9223372036854775807n);
assert.ok(!Object.hasOwn(last, 'next_cursor'));
const single = await client.lookupMessage({queue, msg_id: last.items[0].msg_id.toString()});
assert.equal(single.msg_id, 9223372036854775807n);
assert.ok(requested.every(path => path.startsWith(new URL(mount).pathname)));
assert.ok(requested.includes(new URL('queues', mount).pathname));
console.log(JSON.stringify({mount: new URL(mount).pathname, exact_ids: [page.next_cursor.toString(), single.msg_id.toString()], requested}));
