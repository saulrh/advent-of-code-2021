defmodule Point do
  @enforce_keys [:x, :y, :z]
  @derive Inspect

  use TypedStruct

  typedstruct enforce: true do
    field :x, integer()
    field :y, integer()
    field :z, integer()
  end

  defimpl String.Chars, for: Point do
    def to_string(value), do: "[#{value.x},#{value.y},#{value.z}]"
  end

  def zero, do: %Point{x: 0, y: 0, z: 0}

  def unit_x, do: %Point{x: 1, y: 0, z: 0}
  def unit_y, do: %Point{x: 0, y: 1, z: 0}
  def unit_z, do: %Point{x: 0, y: 0, z: 1}

  def basis, do: [unit_x(), unit_y(), unit_z()]

  def all_units, do: basis() ++ Enum.map(basis(), &inv/1)

  def unit?(p) do
    sq_mag(p) == 1
  end

  def perpendicular?(a, b) do
    dot(a, b) == 0
  end

  def add(a, b) do
    %Point{x: a.x + b.x, y: a.y + b.y, z: a.z + b.z}
  end

  def sub(a, b), do: add(a, inv(b))

  def inv(pt) do
    %Point{x: -pt.x, y: -pt.y, z: -pt.z}
  end

  def mul(v, pt) when is_integer(v) do
    %Point{x: v * pt.x, y: v * pt.y, z: v * pt.z}
  end

  def manhattan_mag(p) do
    abs(p.x) + abs(p.y) + abs(p.z)
  end

  def sq_mag(p) do
    dot(p, p)
  end

  def dot(a, b) do
    a.x * b.x + a.y * b.y + a.z * b.z
  end

  def transform(pt, {basis_x, basis_y, basis_z}) do
    transform(pt, basis_x, basis_y, basis_z)
  end

  def transform(pt, basis_x, basis_y, basis_z) do
    %Point{
      x: basis_x.x * pt.x + basis_y.x * pt.y + basis_z.x * pt.z,
      y: basis_x.y * pt.x + basis_y.y * pt.y + basis_z.y * pt.z,
      z: basis_x.z * pt.x + basis_y.z * pt.y + basis_z.z * pt.z
    }
  end

  def cross(a, b) do
    %Point{
      x: a.y * b.z - a.z * b.y,
      y: a.z * b.x - a.x * b.z,
      z: a.x * b.y - a.y * b.x
    }
  end
end
