#!/usr/bin/env node

const { wish_engine } = require('./target/ci-engine');
console.log(wish_engine.ci.eval_string(`
(:foo {:foo 42})
`));
