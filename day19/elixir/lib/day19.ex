defmodule Day19 do
  defp ingest(fname) do
    parse(File.read!(fname))
  end

  def orientations() do
    for x <- Point.all_units(),
        y <- Point.all_units(),
        Point.perpendicular?(x, y) do
      {x, y, Point.cross(x, y)}
    end
  end

  def line_to_point(line) do
    [x, y, z] = String.split(line, ",") |> Enum.map(&String.to_integer/1)
    %Point{x: x, y: y, z: z}
  end

  def lines_to_scanner([scanner | beacons]) do
    [scanner_idx_s] = Regex.run(~r/--- scanner (?<n>\d+) ---/, scanner, capture: [:n])
    scanner_idx = String.to_integer(scanner_idx_s)
    beacons = beacons |> Enum.map(&line_to_point/1)
    beacons = MapSet.new(beacons)
    {scanner_idx, beacons}
  end

  def parse(filedata) do
    String.split(filedata, "\n")
    |> Enum.chunk_by(&(&1 == ""))
    |> Enum.take_every(2)
    |> Enum.map(&lines_to_scanner/1)
    |> Enum.sort()
    |> Enum.map(fn {_idx, beacs} -> beacs end)
  end

  def find_alignment({s1_a_beacons, _s1_pos}, s2_beacons) do
    Stream.flat_map(s1_a_beacons, fn b1_a ->
      Stream.flat_map(s2_beacons, fn b2 ->
        Stream.map(orientations(), fn orient2 ->
          s2_pos = Point.sub(b1_a, Point.transform(b2, orient2))

          b2_a =
            MapSet.new(
              Enum.map(s2_beacons, fn b -> Point.add(s2_pos, Point.transform(b, orient2)) end)
            )

          matches = MapSet.size(MapSet.intersection(s1_a_beacons, b2_a))

          if matches >= 12 do
            {b2_a, s2_pos}
          end
        end)
      end)
    end)
    |> Enum.find(fn el -> el end)
  end

  def register(registered, []) do
    registered
  end

  def register(registered, [u_cand | unregistered]) do
    case Enum.find_value(registered, fn u_reg -> find_alignment(u_reg, u_cand) end) do
      # rotate to try a new element of the unregistered list,
      # termination is guaranteed because we're assured that the whole
      # set of scanners is a single connected component. this happens
      # when the unregistered candidates are in the wrong order and
      # the current candidate is only connected to scanners that are
      # after it in the unregistered candidate list.
      #
      # append to end is slow, but these lists will be at most length
      # 40 so this isn't a huge problem.
      nil ->
        register(registered, unregistered ++ [u_cand])

      # if we found something, it goes into the registered set and we
      # move on to the rest of the unregistered candidates
      {new_beacons, new_scanner_pos} ->
        n_reg = length(registered) + 1
        n_unreg = length(unregistered)
        ProgressBar.render(n_reg, n_reg + n_unreg)
        register([{new_beacons, new_scanner_pos} | registered], unregistered)
    end
  end

  def part1(result) do
    union =
      Enum.reduce(result, MapSet.new(), fn {beacons, _pos}, acc -> MapSet.union(beacons, acc) end)

    MapSet.size(union)
  end

  def part2(result) do
    for {_, p1} <- result, {_, p2} <- result do
      Point.manhattan_mag(Point.sub(p2, p1))
    end
    |> Enum.max()
  end

  def do_problems(input) do
    [registered | unregistered] = input
    result = register([{registered, Point.zero()}], unregistered)
    IO.puts(part1(result))
    IO.puts(part2(result))
  end

  def main do
    fname = "../example_1.txt"
    do_problems(ingest(fname))

    fname = "../input.txt"
    do_problems(ingest(fname))
  end
end
