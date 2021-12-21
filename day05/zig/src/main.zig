const std = @import("std");
const ArrayList = std.ArrayList;
const allocator = std.heap.page_allocator;
const expect = @import("std").testing.expect;

const example_filename = "../example.txt";
const input_filename = "../input.txt";

const Point = struct {
    x: i32,
    y: i32,

    pub fn format(
        self: Point,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{d},{d}", .{
            self.x,
            self.y,
        });
    }

    pub fn eq(self: Point, other: Point) bool {
        return self.x == other.x and self.y == other.y;
    }
};

test "pt eq" {
    const a = Point{
        .x = -1,
        .y = 1,
    };
    const b = Point{
        .x = 1,
        .y = 1,
    };
    try expect(!a.eq(b));
    try expect(a.eq(a));
    try expect(b.eq(b));
}

const Vent = struct {
    a: Point,
    b: Point,

    const PointsIter = struct {
        x: i32,
        y: i32,
        dx: i32,
        dy: i32,
        cur: i32,
        end: i32,

        pub fn next(self: *PointsIter) ?Point {
            if (self.cur > self.end) {
                return null;
            } else {
                const result = Point{
                    .x = self.x + self.dx * self.cur,
                    .y = self.y + self.dy * self.cur,
                };
                self.cur += 1;
                return result;
            }
        }
    };

    const NotAxisAlignedError = error{
        NotAxisAligned,
    };
    const IterPointsError = error{Overflow} || NotAxisAlignedError;

    pub fn iter_points(self: Vent) IterPointsError!PointsIter {
        return PointsIter{
            .x = self.a.x,
            .y = self.a.y,
            .dx = std.math.clamp(self.dx(), -1, 1),
            .dy = std.math.clamp(self.dy(), -1, 1),
            .cur = 0,
            .end = std.math.max(try self.dx_abs(), try self.dy_abs()),
        };
    }

    pub fn dx(self: Vent) i32 {
        return self.b.x - self.a.x;
    }

    pub fn dx_abs(self: Vent) error{Overflow}!i32 {
        return std.math.absInt(self.b.x - self.a.x);
    }

    pub fn dy(self: Vent) i32 {
        return self.b.y - self.a.y;
    }

    pub fn dy_abs(self: Vent) error{Overflow}!i32 {
        return std.math.absInt(self.b.y - self.a.y);
    }

    pub fn is_vertical(self: Vent) bool {
        return self.dx() == 0;
    }

    pub fn is_horizontal(self: Vent) bool {
        return self.dy() == 0;
    }

    pub fn axis_aligned(self: Vent) bool {
        return self.is_vertical() or self.is_horizontal();
    }

    pub fn format(
        self: Vent,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} -> {s}", .{
            self.a,
            self.b,
        });
    }
};

test "vent iter one pt if eq" {
    const vent = Vent{
        .a = Point{
            .x = 0,
            .y = 1,
        },
        .b = Point{
            .x = 0,
            .y = 1,
        },
    };
    var count: i32 = 0;
    var pts = try vent.iter_points();
    while (pts.next()) |pt| {
        count += 1;
    }
    try expect(count == 1);
}

test "vent iter n+1 pts" {
    const vent = Vent{
        .a = Point{
            .x = 0,
            .y = 1,
        },
        .b = Point{
            .x = 5,
            .y = 1,
        },
    };
    var count: i32 = 0;
    var pts = try vent.iter_points();
    while (pts.next()) |pt| {
        count += 1;
    }
    try expect(count == 6);
}

test "vent iter inclusive ends" {
    const vent = Vent{
        .a = Point{
            .x = 0,
            .y = 1,
        },
        .b = Point{
            .x = 5,
            .y = 1,
        },
    };
    var xmin: i32 = std.math.maxInt(i32);
    var xmax: i32 = std.math.minInt(i32);
    var pts = try vent.iter_points();
    while (pts.next()) |pt| {
        xmin = std.math.min(pt.x, xmin);
        xmax = std.math.max(pt.x, xmax);
    }
    try expect(xmin == std.math.min(vent.a.x, vent.b.x));
    try expect(xmax == std.math.max(vent.a.x, vent.b.x));
}

test "vent iter diag" {
    const vent = Vent{
        .a = Point{
            .x = 0,
            .y = 0,
        },
        .b = Point{
            .x = 5,
            .y = 5,
        },
    };
    var xmin: i32 = std.math.maxInt(i32);
    var xmax: i32 = std.math.minInt(i32);
    var ymin: i32 = std.math.maxInt(i32);
    var ymax: i32 = std.math.minInt(i32);
    var pts = try vent.iter_points();
    while (pts.next()) |pt| {
        xmin = std.math.min(pt.x, xmin);
        xmax = std.math.max(pt.x, xmax);
        ymin = std.math.min(pt.y, ymin);
        ymax = std.math.max(pt.y, ymax);
    }
    try expect(xmin == std.math.min(vent.a.x, vent.b.x));
    try expect(xmax == std.math.max(vent.a.x, vent.b.x));
    try expect(ymin == std.math.min(vent.a.y, vent.b.y));
    try expect(ymax == std.math.max(vent.a.y, vent.b.y));
}

pub fn str_to_pt(str: []const u8) anyerror!Point {
    var field_iter = std.mem.split(str, ",");
    return Point{
        .x = try std.fmt.parseInt(i32, field_iter.next().?, 10),
        .y = try std.fmt.parseInt(i32, field_iter.next().?, 10),
    };
}

pub fn line_to_vent(line: []const u8) anyerror!Vent {
    var point_iter = std.mem.split(line, " -> ");
    return Vent{
        .a = try str_to_pt(point_iter.next().?),
        .b = try str_to_pt(point_iter.next().?),
    };
}

pub fn read(f: []const u8) anyerror!ArrayList(Vent) {
    const file = try std.fs.cwd().openFile(f, .{ .read = true });
    defer file.close();

    var buffer = try allocator.alloc(u8, 1024);
    var result = ArrayList(Vent).init(allocator);
    while (try file.reader().readUntilDelimiterOrEof(buffer, '\n')) |line| {
        var vent = try line_to_vent(line);
        try result.append(vent);
    }

    return result;
}

pub fn solve(f: []const u8, part1: bool) anyerror!i32 {
    const vents = try read(f);
    var grid = std.AutoHashMap(Point, u32).init(allocator);
    defer grid.deinit();
    for (vents.items) |vent| {
        if (part1 and !vent.axis_aligned()) {
            continue;
        }
        var pts = try vent.iter_points();
        while (pts.next()) |pt| {
            const ct = grid.get(pt) orelse 0;
            try grid.put(pt, ct + 1);
        }
    }
    var count: i32 = 0;
    var iter = grid.iterator();
    while (iter.next()) |entry| {
        if (entry.value_ptr.* >= 2) {
            count += 1;
        }
    }
    return count;
}

pub fn main() anyerror!void {
    const stdout = std.io.getStdOut();
    const part1_ex = solve(example_filename, true);
    const part1_ans = solve(input_filename, true);
    try stdout.writer().print("part1 example: {d}\n", .{part1_ex});
    try stdout.writer().print("part1 input: {d}\n", .{part1_ans});

    const part2_ex = solve(example_filename, false);
    const part2_ans = solve(input_filename, false);
    try stdout.writer().print("part2 example: {d}\n", .{part2_ex});
    try stdout.writer().print("part2 input: {d}\n", .{part2_ans});
}

test "part 1 example" {
    const ans = try solve(example_filename, true);
    try expect(ans == 5);
}

test "part 1 input" {
    const ans = try solve(input_filename, true);
    try expect(ans == 7414);
}

test "part 2 example" {
    const ans = try solve(example_filename, false);
    try expect(ans == 12);
}

test "part 2 input" {
    const ans = try solve(input_filename, false);
    try expect(ans < 22429);
}
