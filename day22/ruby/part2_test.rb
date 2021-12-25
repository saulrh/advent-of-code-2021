#!/usr/bin/env ruby

require_relative "part2"
require "test/unit"
require "set"

module CoreExtensions
  module Test
    module Unit
      module TestCase
        def assert_false(boolean, message = nil)
          assert(!boolean, message)
        end
      end
    end
  end
end


Test::Unit::TestCase.include CoreExtensions::Test::Unit::TestCase

I1 = Part2::Interval.new(1, 10)
I2 = Part2::Interval.new(5, 15)
I3 = Part2::Interval.new(15, 25)
I4 = Part2::Interval.new(10, 25)
I5 = Part2::Interval.new(15, 5)

IS = [I1, I2, I3, I4, I5]

class TestInterval < Test::Unit::TestCase
  def test_empty
    assert(Part2::Interval.new(1, 1).empty?)
    assert_false(Part2::Interval.new(1, 2).empty?)
    assert_false(Part2::Interval.new(1, 5).empty?)
    assert(Part2::Interval.new(5, 1).empty?)
  end

  def test_each
    assert_equal([1, 2, 3, 4, 5, 6, 7, 8, 9], I1.to_a)
  end

  def test_len
    assert_equal(9, I1.len)
    assert_equal(10, I2.len)
    assert_equal(10, I2.len)
    assert_equal(0, I5.len)

    assert_equal(I1.count, I1.len)
  end

  def test_intersects?
    assert(I1.intersects?(I2))
    assert_false(I1.intersects?(I3))
    assert_false(I1.intersects?(I4))
    assert_false(I1.intersects?(I5))

    assert_false(I2.intersects?(I3))
    assert(I2.intersects?(I4))
    assert_false(I2.intersects?(I5))

    assert(I3.intersects?(I4))
    assert_false(I3.intersects?(I5))

    assert_false(I4.intersects?(I5))
  end

  def test_intersection
    assert_equal(Part2::Interval.new(5, 10), I1 & I2)
    assert_equal(Part2::Interval.new(5, 10), I2 & I1)
    assert_equal(Part2::Interval.new(10, 15), I2 & I4)
    assert_equal(Part2::Interval.new(15, 25), I3 & I4)
    assert_equal(Part2::Interval.new(10, 15), I4 & I2)
    assert_equal(Part2::Interval.new(15, 25), I4 & I3)
  end

  def test_union
    assert_equal([Part2::Interval.new(1, 15)], I1 | I2)
    assert_equal([Part2::Interval.new(1, 10), Part2::Interval.new(15, 25)], I1 | I3)
    assert_equal([Part2::Interval.new(1, 25)], I1 | I4)
    assert_equal([I1], I1 | I5)

    assert_equal([Part2::Interval.new(5, 25)], I2 | I3)
    assert_equal([Part2::Interval.new(5, 25)], I2 | I4)
    assert_equal([I2], I2 | I5)

    assert_equal([Part2::Interval.new(10, 25)], I3 | I4)
    assert_equal([I3], I3 | I5)

    assert_equal([], I5 | I5)
  end

  def test_difference
    assert_equal([], I1 - I1)
    assert_equal([Part2::Interval.new(1, 5)], I1 - I2)
    assert_equal([I1], I1 - I3)
    assert_equal([I1], I1 - I4)
    assert_equal([I1], I1 - I5)

    assert_equal([Part2::Interval.new(10, 15)], I2 - I1)
    assert_equal([], I2 - I2)
    assert_equal([I2], I2 - I3)
    assert_equal([Part2::Interval.new(5, 10)], I2 - I4)
    assert_equal([I2], I2 - I5)

    assert_equal([I3], I3 - I1)
    assert_equal([I3], I3 - I2)
    assert_equal([], I3 - I3)
    assert_equal([], I3 - I4)
    assert_equal([I3], I3 - I5)

    assert_equal([I4], I4 - I1)
    assert_equal([Part2::Interval.new(15, 25)], I4 - I2)
    assert_equal([Part2::Interval.new(10, 15)], I4 - I3)
    assert_equal([], I4 - I4)
    assert_equal([I4], I4 - I5)
  end

  def assert_commutative(op)
    for i1 in IS
      for i2 in IS
        i1.method(op).call(i2) == i2.method(op).call(i1)
      end
    end
  end

  def test_commutative_union
    assert_commutative(:|)
  end

  def test_commutative_intersection
    assert_commutative(:&)
  end

  def test_commutative_eql
    assert_commutative(:==)
  end

  def test_comparisons?
    assert(I4 >= I3)
    assert_false(I3 >= I4)
    assert_false(I5 >= I2)
    assert(I1 >= I1)
  end

  def test_eql?
    assert_equal(Part2::Interval.new(1, 5),
                 Part2::Interval.new(1, 5))
    assert_not_equal(Part2::Interval.new(1, 5),
                     Part2::Interval.new(1, 6))
  end

  def test_from_series
    assert_equal(Part2::Interval.from_series([10, 20, 1, 10, 5, 15]), [
                   Part2::Interval.new(1, 5),
                   Part2::Interval.new(5, 10),
                   Part2::Interval.new(10, 15),
                   Part2::Interval.new(15, 20),
                 ])
  end

  def test_from_s
    assert_equal(Part2::Interval.new(-10, 25), Part2::Interval.from_s("-10..24"))
  end
end

B1 = Part2::AABB.from_s("x=-5..5,y=-10..20,z=30..50")
B2 = Part2::AABB.from_s("x=3..9,y=12..25,z=36..43")
B3 = Part2::AABB.from_s("x=-15..-10,y=-10..20,z=30..50")

class TestAABB < Test::Unit::TestCase
  def test_volume
    assert_equal(9 * 10 * 10, Part2::AABB.new([I1, I2, I3]).volume)
    assert_equal(B1.count, B1.volume)
  end

  def test_from_s
    assert_equal(Part2::AABB.new([Part2::Interval::from_s("-1..16"),
                                  Part2::Interval::from_s("-48..0"),
                                  Part2::Interval::from_s("-7..44")]),
                 Part2::AABB.from_s("x=-1..16,y=-48..0,z=-7..44"))
  end

  def test_intersects?
    assert(B1.intersects?(B2))
    assert_false(B1.intersects?(B3))
  end

  def test_intersection
    assert_equal(Part2::AABB.from_s("x=3..5,y=12..20,z=36..43"), B1 & B2)
    assert_equal(Part2::AABB.from_s("x=-5..-10,y=-10..20,z=30..50"), B1 & B3)

    b12 = B1 & B2
    assert_equal(b12.to_set, B1.to_a.to_set & B2.to_set)
  end

  def test_union
    b12 = B1 | B2
    assert_equal(b12.map{|b| b.to_set}.reduce(Set[], :|), B1.to_set | B2.to_set)
    assert_equal(B1.volume + B2.volume - (B1 & B2).volume, b12.map{|b| b.volume}.sum)
  end

  def test_difference
    b12 = B1 - B2
    assert_equal(b12.map{|b| b.to_set}.reduce(Set[], :|), B1.to_set - B2.to_set)
    assert_equal(B1.volume - (B1 & B2).volume, b12.map{|b| b.volume}.sum)
  end
end
