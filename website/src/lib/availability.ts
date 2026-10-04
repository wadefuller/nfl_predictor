export function availabilityFresh(value: { checkedAt?: string | null } | undefined, now = Date.now()): boolean {
  const raw = value?.checkedAt ?? '';
  const time = Date.parse(raw.includes('T') ? raw : raw.replace(' ', 'T') + 'Z');
  return Number.isFinite(time) && now >= time && now-time <= 6*3600_000;
}

export function availabilityEligible(value: { eligible?: boolean; checkedAt?: string | null } | undefined, now = Date.now()): boolean {
  return value?.eligible === true && availabilityFresh(value, now);
}
