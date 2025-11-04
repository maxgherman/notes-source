// Self-hom-set Hom(List<int>, List<int>)
using System;
using System.Collections.Generic;
using System.Linq;

Func<List<int>, List<int>> sort = list => list.OrderBy(x => x).ToList();
Func<List<int>, List<int>> reverse = list => { var copy = new List<int>(list); copy.Reverse(); return copy; };
Func<List<int>, List<int>> doubleFn = list => list.Select(x => x * 2).ToList();
Func<List<int>, List<int>> identity = list => new List<int>(list);

// String transformations - Hom(string, string)
Func<string, string> upperCase = s => s.ToUpper();
Func<string, string> addPrefix = s => ">> " + s;
Func<string, string> trim = s => s.Trim();

// Chain transformations
Func<string, string> process = s => addPrefix(upperCase(trim(s)));