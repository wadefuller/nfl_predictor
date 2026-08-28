const weekLabels: Record<number, string> = {
  1: 'Week 1',
  2: 'Week 2',
  3: 'Week 3',
  4: 'Week 4',
  5: 'Week 5',
  6: 'Week 6',
  7: 'Week 7',
  8: 'Week 8',
  9: 'Week 9',
  10: 'Week 10',
  11: 'Week 11',
  12: 'Week 12',
  13: 'Week 13',
  14: 'Week 14',
  15: 'Week 15',
  16: 'Week 16',
  17: 'Week 17',
  18: 'Week 18',
  19: 'Wild Card',
  20: 'Divisional',
  21: 'Conf. Championships',
  22: 'Super Bowl',
};

export function getWeekLabel(week: number): string {
  return weekLabels[week] ?? `Week ${week}`;
}

export function getWeeksForSeason(maxWeek: number): number[] {
  const weeks: number[] = [];
  for (let i = 1; i <= maxWeek; i++) {
    weeks.push(i);
  }
  return weeks;
}
