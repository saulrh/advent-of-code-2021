#!/usr/bin/env ruby

require 'set'

def parse_line(s)
  num = "([-[:digit:]]+)"
  reg = Regexp.new("(on|off)[[:space:]]+x=#{num}\.\.#{num},y=#{num}\.\.#{num},z=#{num}\.\.#{num}")
  res = reg.match(s)
  nums = res[2..7].map {|x| x.to_i }
  return res[1].to_sym, [nums[0]..nums[1], nums[2]..nums[3], nums[4]..nums[5]]
end

ex = File.foreach("../example2.txt").map {|l| parse_line(l)}

data = Set[]

bounds = -50..50

for idx, (op, ranges) in ex.each_with_index
  puts "done with #{idx}/#{ex.length}"
  for x in ranges[0]
    if !bounds.include?(x)
      next
    end
    for y in ranges[1]
      if !bounds.include?(y)
        next
      end
      for z in ranges[2]
        if !bounds.include?(z)
          next
        end
        pt = [x, y, z]
        if op == :on
          data.add(pt)
        elsif op == :off
          data.delete(pt)
        end
      end
    end
  end
end

puts data.length
