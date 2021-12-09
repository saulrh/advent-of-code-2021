import std.stdio;
import std.array;
import std.string;
import std.algorithm;
import std.conv;
import std.range;
import std.typecons;

struct Point {
  int row;
  int col;

  Point opBinary(string op)(Point rhs) const {
    return mixin("Point(row " ~ op ~ " rhs.row, col " ~ op ~ "rhs.col)");
  }
}

unittest {
  immutable auto a = Point(1, 2);
  immutable auto b = Point(3, 4);
  assert(a + b == Point(4, 6));
  assert(a - b == Point(-2, -2));
  assert(a * b == Point(3, 8));
}

immutable auto up = Point(-1, 0);
immutable auto down = Point(1, 0);
immutable auto left = Point(0, -1);
immutable auto right = Point(0, 1);

immutable auto dirs = [up, down, left, right];

int[] parseline(string line) {
  return line.strip().split("").to!(int[])();
}

int[Point] parsefile(string fname) {
  auto f = File(fname, "r");
  string line;
  int row = 0;
  int[Point] data;
  while ((line = f.readln()) !is null) {
    int[] linedata = parseline(line);
    foreach (col, d ; linedata) {
      data[Point(row, col.to!int())] = d;
    }
    row++;
  }
  return data;
}

Point[] lows(int[Point] data) {
  Point[] result;
points: foreach (pt, d ; data) {
    foreach (dir ; dirs) {
      immutable auto offset = pt + dir;
      if (offset in data) {
        if (data[offset] <= d) {
          continue points;
        }
      }
    }
    result ~= pt;
  }
  return result;
}

Point[][Point] basins(int[Point] data) {
  alias Front = Tuple!(Point, "basin", Point, "pt");
  Point[][Point] results;
  Front[] frontier;
  bool[Point] visited;
  foreach (low ; data.lows()) {
    frontier ~= Front(low, low);
  }
  while (!frontier.empty()) {
    Front nxt = frontier[frontier.length - 1];
    frontier = frontier.remove(frontier.length - 1);
    if (nxt.pt in visited) {
      continue;
    }
    if (nxt.pt !in data) {
      continue;
    }
    if (data[nxt.pt] >= 9) {
      continue;
    }
    visited[nxt.pt] = true;
    results[nxt.basin] ~= nxt.pt;
    foreach (dir ; dirs) {
      frontier ~= Front(nxt.basin, nxt.pt + dir);
    }
  }
  return results;
}

T product(T)(T[] data) {
  return data.fold!((acc, el) => acc * el)(1);
}

void main()
{
  auto data = parsefile("../input.txt");
  auto low_values = data.lows().map!(p => 1 + data[p]);
  writeln(low_values.sum());
  int[] basin_sizes = data.basins().byValue.map!(pts => pts.length.to!int()).array();
  basin_sizes.sort();
  basin_sizes[basin_sizes.length - 3 .. basin_sizes.length].product().writeln();
}

unittest {
  auto data = parsefile("../example.txt");
  auto low_values = data.lows().map!(p => 1 + data[p]);
  assert(low_values.sum() == 15);
}

unittest {
  auto data = parsefile("../input.txt");
  auto low_values = data.lows().map!(p => 1 + data[p]);
  assert(low_values.sum() == 562);
}

unittest {
  auto data = parsefile("../example.txt");
  int[] basin_sizes = data.basins().byValue.map!(pts => pts.length.to!int()).array();
  sort(basin_sizes);
  assert(basin_sizes == [3, 9, 9, 14]);
  assert(basin_sizes[basin_sizes.length - 3 .. basin_sizes.length].product() == 1134);
}

unittest {
  auto data = parsefile("../input.txt");
  auto basins = data.basins();
}
