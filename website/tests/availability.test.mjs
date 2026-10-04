import { readFileSync } from 'node:fs';
import assert from 'node:assert/strict';
import test from 'node:test';
import ts from 'typescript';

const source = readFileSync(new URL('../src/lib/availability.ts', import.meta.url), 'utf8');
const { outputText } = ts.transpileModule(source, { compilerOptions: { module: ts.ModuleKind.ESNext } });
const { availabilityFresh, availabilityEligible } = await import(`data:text/javascript;base64,${Buffer.from(outputText).toString('base64')}`);
const now = Date.parse('2026-09-27T15:00:00Z');
test('a fresh verified starter is eligible, including space-formatted UTC timestamps', () => {
  assert(availabilityEligible({ eligible: true, checkedAt: '2026-09-27 14:00:00' }, now));
});
test('fresh QB uncertainty is distinct from stale data', () => {
  const value = { eligible: false, checkedAt: '2026-09-27T14:00:00Z' };
  assert(availabilityFresh(value, now));
  assert(!availabilityEligible(value, now));
  assert(!availabilityFresh({ checkedAt: '2026-09-27T08:00:00Z' }, now));
});
test('unknown, malformed, future and expired checks never enable recommendations', () => {
  for (const checkedAt of [null, '', 'bad date', '2026-09-28T15:00:00Z', '2026-09-27T08:00:00Z']) {
    assert(!availabilityEligible({ eligible: true, checkedAt }, now));
  }
  assert(!availabilityEligible(undefined, now));
});
