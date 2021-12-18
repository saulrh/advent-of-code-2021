package.cpath = package.cpath .. ';/home/saul/.luarocks/lib/lua/5.1/?.so'
package.path = package.path .. ';/home/saul/.luarocks/share/lua/5.1/?.lua'

inspect = require("inspect")
lpeg = require("lpeg")
re = require("re")
dbg = require("debugger")

local tokenlist = lpeg.P{
   "S";
   S = lpeg.Ct(lpeg.V("F")^1) * -lpeg.P(1),
   F = (lpeg.R("09")^1 / tonumber) + lpeg.C(lpeg.S("[]")) + lpeg.S(","),
}

function table.slice(tbl, first, last, step)
  local sliced = {}

  for i = first or 1, last or #tbl, step or 1 do
    sliced[#sliced+1] = tbl[i]
  end

  return sliced
end

function trim(s)
   return (s:gsub("^%s*(.-)%s*$", "%1"))
end

function readall(filename)
  local fh = assert(io.open(filename, "rb"))
  local contents = assert(fh:read(_VERSION <= "Lua 5.2" and "*a" or "a"))
  fh:close()
  return contents
end

function splitlines(s)
   return split(s, "[^\n]+")
end

function lex(s)
   toks = {}
   for _, tok in ipairs(split(s, "^%s+")) do
      toks[#toks + 1] = tok
   end
   return toks
end

function parse(tokens)
   return tokens
end

function split(s, block)
   arr = {}
   for match in string.gmatch(s, block) do
      arr[#arr + 1] = match
   end
   return arr
end

function normalization_err(toks)
   local depth = 0
   for idx, tok in ipairs(toks) do
      if tok == "[" then
         depth = depth + 1
      elseif tok == "]" then
         depth = depth - 1
      end
      if depth > 4 then
         return "explode", idx
      end
   end
   for idx, tok in ipairs(toks) do
      if type(tok) == "number" and tok > 9 then
         return "split", idx
      end
   end
   return nil
end

function explode_at(toks, open_bracket_idx)
   local left_idx = open_bracket_idx + 1
   local right_idx = open_bracket_idx + 2
   local close_bracket_idx = open_bracket_idx + 3

   -- print("exploding: at idx "
   --       .. open_bracket_idx
   --       .. ": "
   --       .. toks[open_bracket_idx]
   --       .. toks[left_idx]
   --       .. " " .. toks[right_idx]
   --       .. toks[close_bracket_idx])

   -- left number
   local prev_number_idx = nil
   for idx = left_idx-1, 0, -1 do
      if type(toks[idx]) == "number" then
         prev_number_idx = idx
         break
      end
   end
   if prev_number_idx then
      toks[prev_number_idx] = toks[prev_number_idx] + toks[left_idx]
   end

   -- right number
   local next_number_idx = nil
   for idx = right_idx+1, #toks-1, 1 do
      if type(toks[idx]) == "number" then
         next_number_idx = idx
         break
      end
   end
   if next_number_idx then
      toks[next_number_idx] = toks[next_number_idx] + toks[right_idx]
   end

   -- pair delorted
   table.remove(toks, close_bracket_idx)
   table.remove(toks, right_idx)
   table.remove(toks, left_idx)
   table.remove(toks, open_bracket_idx)

   -- insert zero
   table.insert(toks, open_bracket_idx, 0)
end

function assert_lists_eq(a, b)
   assert(type(a) == type(b), "a and b have different types: " .. type(a) .. ", " .. type(b))
   assert(#a == #b, "a and b have different lengths: " .. #a .. ", " .. #b)
   for idx = 1, #a do
      assert(a[idx] == b[idx], "a and b differ at idx " .. idx .. ": " .. inspect(a[idx]) .. " != " .. inspect(b[idx]))
   end
end

function split_at(toks, number_idx)
   local value = toks[number_idx]
   local mid = math.floor(value / 2)
   local left = mid
   local right = mid + (value % 2)

   -- print("splitting: " .. value .. " at idx " .. number_idx)

   -- number delorted
   table.remove(toks, number_idx)

   -- insert new pair, right to left because we're doing repeated inserts
   table.insert(toks, number_idx, "]")
   table.insert(toks, number_idx, right)
   table.insert(toks, number_idx, left)
   table.insert(toks, number_idx, "[")
end

function step(toks)
   err, idx = normalization_err(toks)
   if err == "explode" then
      explode_at(toks, idx)
   elseif err == "split" then
      split_at(toks, idx)
   end
   return err ~= nil
end

function chain(ts)
   local result = {}
   for _, t in ipairs(ts) do
      if type(t) == "table" then
         for _, v in ipairs(t) do
            table.insert(result, v)
         end
      else
         table.insert(result, t)
      end
   end
   return result
end

function add(a, b)
   local result = chain({"[", a, b, "]"})
   while step(result) do
      -- print(render(result))
      -- print(render_depths(result))
   end
   return result
end

function table.map(f, c)
   result = {}
   for _, v in ipairs(c) do
      table.insert(result, f(v))
   end
   return result
end

function run_example(str)
   local lines = table.map(function(el) return tokenlist:match(el) end, splitlines(str))
   local expected_result = lines[1]
   table.remove(lines, 1)
   local sum = accumulate(lines)
   assert_lists_eq(sum, expected_result)
end

function accumulate(fishes)
   local acc = fishes[1]
   for idx = 2, #fishes do
      acc = add(acc, fishes[idx])
   end
   return acc
end

function needs_comma(lt, rt)
   return (type(lt) == "number" and type(rt) == "number")
      or (lt == "]" and rt == "[")
      or (type(lt) == "number" and rt == "[")
      or (lt == "]" and type(rt) == "number")
end

function render(toks)
   local results = {}
   for idx = 1, #toks - 1 do
      lt = toks[idx]
      rt = toks[idx + 1]
      if needs_comma(lt, rt) then
         table.insert(results, tostring(lt))
         table.insert(results, ",")
      else
         table.insert(results, tostring(lt))
      end
   end
   table.insert(results, toks[#toks])
   return table.concat(results)
end

function render_depths(toks)
   local result = {}
   local depth = 0
   for idx = 1, #toks - 1 do
      lt = toks[idx]
      rt = toks[idx + 1]

      if lt == "[" then
         depth = depth + 1
         table.insert(result, tostring(depth))
      elseif lt == "]" then
         depth = depth - 1
         table.insert(result, tostring(depth))
      else
         table.insert(result, " ")
      end

      if needs_comma(lt, rt) then
         table.insert(result, " ")
      end
   end
   table.insert(result, "0")
   return table.concat(result)
end

function magnitude(s, left_idx, right_idx)
   if type(s) == "number" then
      return s
   end
   if left_idx == nil then
      left_idx = 1
   end
   if right_idx == nil then
      right_idx = #s
   end
   if left_idx == right_idx then
      return s[left_idx]
   end

   -- It's a sequence. find the middle by counting depths and recurse.
   assert(s[left_idx] == "[", "Did not recurse into a list? "
          .. left_idx .. ": " .. s[left_idx] .. ", "
          .. right_idx .. ": " .. s[right_idx] .. "\n" .. render(s))
   assert(s[right_idx] == "]")

   -- find where the left side ends
   local start_of_left_idx = left_idx + 1
   local end_of_left_idx = nil
   local depth = 0
   for idx = start_of_left_idx, right_idx do
      tok = s[idx]
      if tok == "[" then
         depth = depth + 1
      elseif tok == "]" then
         depth = depth - 1
      end
      if depth == 0 then
         -- we're all the way back down
         end_of_left_idx = idx
         break
      end
   end

   -- recurse
   local left_mag = magnitude(s, start_of_left_idx, end_of_left_idx)
   local right_mag = magnitude(s, end_of_left_idx + 1, right_idx - 1)
   local result = (3 * left_mag) + (2 * right_mag)
   -- print(left_idx, right_idx, result, render(table.slice(s, left_idx, right_idx)))
   return result
end

function part1(fishes)
   return magnitude(accumulate(fishes))
end

function part2(fishes)
   local max = 0
   for a_idx = 1, #fishes do
      for b_idx = 1, #fishes do
         if a_idx ~= b_idx then
            max = math.max(max, magnitude(add(fishes[a_idx], fishes[b_idx])))
         end
      end
   end
   return max
end

function parse(str)
   return table.map(function(el) return tokenlist:match(el) end, splitlines(str))
end

function main()
   local explodes = readall("../single-explodes.txt")
   for idx, line in ipairs(splitlines(explodes)) do
      local left, explode_idx, right = unpack(split(line, "%S+"))
      explode_idx = tonumber(explode_idx)
      local left_toks = tokenlist:match(left)
      local right_toks = tokenlist:match(right)
      explode_at(left_toks, explode_idx)
      assert_lists_eq(left_toks, right_toks)
   end

   local splits = readall("../single-splits.txt")
   for idx, line in ipairs(splitlines(splits)) do
      local left, split_idx, right = unpack(split(line, "%S+"))
      split_idx = tonumber(split_idx)
      local left_toks = tokenlist:match(left)
      local right_toks = tokenlist:match(right)
      split_at(left_toks, split_idx)
      assert_lists_eq(left_toks, right_toks)
   end

   run_example(readall("../example1.txt"))
   run_example(readall("../example2.txt"))
   run_example(readall("../example3.txt"))
   run_example(readall("../example4.txt"))

   local mags = readall("../magnitudes.txt")
   for idx, line in ipairs(splitlines(mags)) do
      local fish, expected_mag = unpack(split(line, "%S+"))
      expected_mag = tonumber(expected_mag)
      fish = tokenlist:match(fish)
      local mag = magnitude(fish, 1, #fish)
      assert(mag == expected_mag, "magnitude incorrect for line " .. idx .. ": " .. mag .. " != " .. expected_mag)
   end

   local example = parse(readall("../example_sum.txt"))
   local example_part1 = part1(example)
   assert(example_part1 == 4140, "example part1: " .. example_part1 .. " != " .. 4140)
   local example_part2 = part2(example)
   assert(example_part2 == 3993, "example part2: " .. example_part2 .. " != " .. 4140)

   local input = parse(readall("../input.txt"))
   local part1_soln = part1(input)
   assert(part1_soln == 3981, "part1_soln incorrect")
   print(part1_soln)
   local part2_soln = part2(input)
   assert(part2_soln > 4681, "part2_soln too low")
   print(part2_soln)
end

-- dbg.call(main)
main()
