defmodule Day19 do
  defp progress(done, left) do
    ProgressBar.render(done, done + left,
      bar: "═",
      blank: "─",
      bar_color: IO.ANSI.green(),
      blank_color: IO.ANSI.red(),
      suffix: :count
    )
  end

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

  def register(registered, frontier, unregistered) do
    case MapSet.size(unregistered) do
      0 ->
        MapSet.union(registered, frontier)

      _ ->
        {new_frontier, new_unregistered} =
          Task.async_stream(
            unregistered,
            fn u_cand ->
              found = Enum.find_value(frontier, fn u_reg -> find_alignment(u_reg, u_cand) end)

              case found do
                {reg_beacons, reg_pos} -> {reg_beacons, reg_pos}
                nil -> {u_cand, nil}
              end
            end,
            timeout: :infinity
          )
          |> Enum.map(fn {:ok, el} -> el end)
          |> Enum.split_with(fn el -> elem(el, 1) end)

        # Move everything we found into the frontier.
        new_registered = MapSet.union(registered, frontier)
        # we know that none of the remaining unregistered scanners
        # connect to any of the registered scanners, so we can skip
        # those entirely in future iteratoins and only retry the
        # remaining unregistered scanners against the new frontier.
        new_frontier = MapSet.new(new_frontier)
        # move everything we didn't find into our new unregistered
        # set.
        new_unregistered = for e <- new_unregistered, into: MapSet.new(), do: elem(e, 0)

        progress(
          MapSet.size(new_frontier) + MapSet.size(new_registered),
          MapSet.size(new_unregistered)
        )

        register(new_registered, new_frontier, new_unregistered)
    end
  end

  def part1(result) do
    MapSet.size(
      for {beacons, _pos} <- result, reduce: MapSet.new() do
        acc -> MapSet.union(beacons, acc)
      end
    )
  end

  def part2(result) do
    for {_, p1} <- result, {_, p2} <- result do
      Point.manhattan_mag(Point.sub(p2, p1))
    end
    |> Enum.max()
  end

  def do_problems(input) do
    [scanner_zero | unregistered] = input
    scanner_zero = MapSet.new([{scanner_zero, Point.zero()}])
    unregistered = MapSet.new(unregistered)
    progress(1, MapSet.size(unregistered))
    result = register(MapSet.new(), scanner_zero, unregistered)
    {part1(result), part2(result)}
  end

  def main do
    fname = "../example_1.txt"
    {79, 3621} = do_problems(ingest(fname))

    fname = "../input.txt"
    {491, 13374} = do_problems(ingest(fname))
  end
end
