#!/usr/bin/env node

const { wish_engine } = require('./public/js/engine');
console.log(wish_engine.core.eval_string(`
{:foo 42}
`));
