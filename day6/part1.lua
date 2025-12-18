function parse()
  local lines = {}

  for line in io.lines('../inputs/day6.txt') do
      local currLin = {}

      for num in line:gmatch('%d+') do
          table.insert(currLin, tonumber(num))
      end

      for char in line:gmatch('[+*]') do
          table.insert(currLin, char)
      end

      table.insert(lines, currLin)
  end

  return lines
end

local lines = parse()

local res = 0
local actRes = 0

local op = '+'


for j = #lines[1], 1, -1 do
    for i = #lines, 1, -1 do 
        if lines[i][j] == '+' or lines[i][j] == '*' then
            op = lines[i][j]
            goto continue
        end
        if op == '+' then actRes = actRes + lines[i][j]
        else
            if actRes == 0 then actRes = actRes + lines[i][j]
            else actRes = actRes * lines[i][j]
            end
        end
        ::continue::
    end
    res = res + actRes
    actRes = 0
end

print(res)
