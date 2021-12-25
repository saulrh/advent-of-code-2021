#!/usr/bin/env ruby
def array_product(as)
  as[0].product(*as[1..])
end

module Part2
  class Interval
    attr_reader :l, :r

    include Enumerable

    def initialize(l, r)
      @l = l
      @r = r
    end

    def len
      [@r - @l, 0].max
    end

    def ==(other)
      @l == other.l and @r == other.r
    end

    def empty?
      @l >= @r
    end

    def intersects?(other)
      !self.intersection(other).empty?
    end

    def &(other)
      intersection(other)
    end

    def intersection(other)
      Interval.new([@l, other.l].max, [@r, other.r].min)
    end

    def |(other)
      union(other)
    end

    def union(other)
      _union(other).reject{|i| i.empty?}
    end

    def -(removed)
      self.difference(removed)
    end

    def difference(removed)
      _difference(removed).reject{|i| i.empty?}
    end

    def >=(smaller)
      @l <= smaller.l and @r >= smaller.r
    end

    def >(smaller)
      @l < smaller.l and @r > smaller.r
    end

    def <=(larger)
      @l >= larger.l and @r <= larger.r
    end

    def <(larger)
      @l > larger.l and @r < larger.r
    end

    def subintervals(other)
      [*(self - other), self & other, *(other - self)]
    end

    def to_s
      "[#{@l}..#{@r})"
    end

    def split(n)
      if n <= @l
        return [self]
      elsif n >= @r
        return [self]
      else
        return [Interval.new(@l, n), Interval.new(n, @r)]
      end
    end

    def self.from_series(ns)
      ns.sort.uniq.each_cons(2).map { |a, b| Interval.new(a, b) }
    end

    def self.from_s(s)
      res = /(-?[[:digit:]]+)\.\.(-?[[:digit:]]+)/.match(s)
      return Interval.new(res[1].to_i, res[2].to_i + 1)
    end

    def each
      for i in @l ... @r
        yield i
      end
    end

    private

    def _union(other)
      if self.empty?
        [other]
      elsif other.empty?
        [self]
      elsif !self.intersects?(other)
        # we know they're each in order now, so we can just put
        # together the lefts and rights.
        if @r == other.l
          [Interval.new(@l, other.r)]
        elsif other.r == @l
          [Interval.new(other.l, @r)]
        else
          [self, other]
        end
      elsif self >= other
        [self]
      elsif other >= self
        [other]
      else
        # we know they overlap but do not cover each other, so it must
        # just be the biggest span.
        [Interval.new([@l, other.l].min, [@r, other.r].max)]
      end
    end

    def _difference(removed)
      if self.empty?
        []
      elsif removed.empty?
        [self]
      elsif !self.intersects?(removed)
        [self]
      elsif removed >= self
        []
      elsif self >= removed
        [Interval.new(@l, removed.l), Interval.new(removed.r, @r)]
      elsif removed.l <= @l
        [Interval.new(removed.r, @r)]
      elsif @r <= removed.r
        [Interval.new(@l, removed.l)]
      else
        raise "should be impossible?"
      end
    end
  end

  class AABB
    attr_reader :dims

    include Enumerable

    def initialize(dims)
      @dims = dims
    end

    def volume
      @dims.map{|i| i.len}.reduce(1, :*)
    end

    def empty?
      @dims.any?{|i| i.empty?}
    end

    def intersects?(other)
      !self.intersection(other).empty?
    end

    def >=(smaller)
      @dims.zip(other.dims).all?{|si, oi| si >= oi}
    end

    def >(smaller)
      @dims.zip(other.dims).all?{|si, oi| si > oi}
    end

    def <=(larger)
      @dims.zip(other.dims).all?{|si, oi| si <= oi}
    end

    def <(larger)
      @dims.zip(other.dims).all?{|si, oi| si < oi}
    end

    def &(other)
      intersection(other)
    end

    def intersection(other)
      AABB.new(@dims.zip(other.dims).map{|si, oi| si & oi})
    end

    def all_subboxes(other)
      subintervals = @dims.zip(other.dims).map{|si, oi| si.subintervals(oi)}
      array_product(subintervals).map{|is| AABB.new(is)}
    end

    def |(other)
      union(other)
    end

    def union(other)
      if !self.intersects?(other)
        [self, other]
      else
        all_subboxes(other).filter{|box| box.intersects?(self) or box.intersects?(other)}
      end
    end

    def -(removed)
      difference(removed)
    end

    def difference(removed)
      if !self.intersects?(removed)
        [self]
      else
        all_subboxes(removed).filter{|box| box.intersects?(self) and !box.intersects?(removed)}
      end
    end

    def ==(other)
      @dims == other.dims
    end

    def self.from_s(s)
      interval = "([[:digit:].-]+)"
      res = Regexp.new("x=#{interval},y=#{interval},z=#{interval}").match(s)
      AABB.new(res[1..].map{|r| Interval.from_s(r)})
    end

    def to_s
      "AABB(x=#{@dims[0]},y=#{@dims[1]},z=#{@dims[2]})"
    end

    def each
      for x in @dims[0]
        for y in @dims[1]
          for z in @dims[2]
            yield [x, y, z]
          end
        end
      end
    end
  end
end

def parse_line(s)
  res = /(on|off)[[:space:]]+(.+)/.match(s)
  return res[1].to_sym, Part2::AABB.from_s(res[2])
end

problem = File.foreach("../input.txt").map {|l| parse_line(l)}

data = []

for (op, box), idx in problem.each_with_index
  data = data.flat_map{|b| b - box }
  if op == :on
    data.push(box)
  end
end

# init_area = Part2::AABB.from_s("x=-50..50,y=-50..50,z=-50..50")
# data = data.map{|b| b & init_area}

puts data.map{|b| b.volume}.sum
