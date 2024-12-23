defmodule PointTest do
  use ExUnit.Case
  doctest Point

  def pt_a, do: %Point{x: 1, y: 2, z: 3}
  def pt_b, do: %Point{x: 4, y: 5, z: 7}
  def pt_c, do: %Point{x: 5, y: 7, z: 10}

  def pts, do: [pt_a(), pt_b(), pt_c()]

  test "equality" do
    assert pt_a() == %Point{x: 1, y: 2, z: 3}
    assert pt_a() != %Point{x: 9, y: 2, z: 3}
    assert pt_a() != %Point{x: 1, y: 9, z: 3}
    assert pt_a() != %Point{x: 1, y: 2, z: 9}
  end

  test "add" do
    assert Point.add(pt_a(), pt_b()) == pt_c()
  end

  test "sub" do
    assert Point.sub(pt_c(), pt_b()) == pt_a()
  end

  test "mul" do
    assert Point.mul(3, pt_a()) == %Point{x: 3, y: 6, z: 9}
  end

  test "dot" do
    assert Point.dot(pt_a(), pt_b()) == 1 * 4 + 2 * 5 + 3 * 7
  end

  test "sq_mag" do
    assert Point.sq_mag(pt_a()) == 1 + 4 + 9

    for p <- Point.all_units() do
      assert Point.sq_mag(p) == 1
    end
  end

  test "inv" do
    assert Point.inv(pt_a()) == %Point{x: -1, y: -2, z: -3}
    assert Point.add(pt_a(), Point.inv(pt_a())) == Point.zero()
    assert Point.add(pt_b(), Point.inv(pt_b())) == Point.zero()
    assert Point.add(pt_c(), Point.inv(pt_c())) == Point.zero()
  end

  test "transform" do
    for p <- pts() do
      assert Point.transform(p, Point.unit_x(), Point.unit_y(), Point.unit_z()) == p

      assert Point.transform(
               p,
               Point.inv(Point.unit_x()),
               Point.inv(Point.unit_y()),
               Point.inv(Point.unit_z())
             ) == Point.inv(p)
    end
  end

  test "cross" do
    assert Point.cross(%Point{x: 3, y: -3, z: 1}, %Point{x: 4, y: 9, z: 2}) == %Point{
             x: -15,
             y: -2,
             z: 39
           }

    for p <- pts() do
      assert Point.cross(p, p) == Point.zero()
    end

    for a <- pts(), b <- pts() do
      assert Point.cross(a, b) == Point.inv(Point.cross(b, a))
    end

    for a <- pts(), b <- pts(), c <- pts() do
      assert Point.cross(a, Point.add(b, c)) == Point.add(Point.cross(a, b), Point.cross(a, c))
    end
  end
end
