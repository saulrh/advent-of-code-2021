defmodule Day19Test do
  use ExUnit.Case
  doctest Day19

  test "orientations count 24" do
    assert length(Day19.orientations()) == 24
  end

  test "orientations all unique" do
    assert Day19.orientations() |> Enum.uniq() |> length() == 24
  end

  test "orientations all mutually perpendicular" do
    assert Enum.all?(Day19.orientations(), fn {x, y, z} ->
             Point.perpendicular?(x, y) and Point.perpendicular?(x, z) and
               Point.perpendicular?(y, z)
           end)
  end
end
