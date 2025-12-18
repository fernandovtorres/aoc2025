function parse()
  local lines = {}
  local maxLen = 0

  local rawLines = {}
  for line in io.lines('../inputs/day6.txt') do
      table.insert(rawLines, line)
      if #line > maxLen then maxLen = #line end
  end

  for _, line in ipairs(rawLines) do
      local padded = line .. string.rep(" ", maxLen - #line)
      local currLin = {}
      for k = 1, #padded do
          table.insert(currLin, padded:sub(k, k))
      end
      table.insert(lines, currLin)
  end

  return lines, maxLen
end

local lines, width = parse()

local grandTotal = 0
local blockNumbers = {}
local currentOp = nil

for j = width, 1, -1 do
    local isSpaceColumn = true
    local colNumStr = ""
    local foundOp = nil

    for i = 1, #lines do
        local char = lines[i][j]
        if char ~= " " then
            isSpaceColumn = false
            if char:match("%d") then
                colNumStr = colNumStr .. char
            elseif char == '+' or char == '*' then
                foundOp = char
            end
        end
    end

    if isSpaceColumn then
        if #blockNumbers > 0 then
            local res = blockNumbers[1]
            for k = 2, #blockNumbers do
                if currentOp == '*' then
                    res = res * blockNumbers[k]
                else
                    res = res + blockNumbers[k]
                end
            end
            grandTotal = grandTotal + res
        end
        blockNumbers = {}
        currentOp = nil
    else
        if foundOp then currentOp = foundOp end
        if #colNumStr > 0 then
            table.insert(blockNumbers, tonumber(colNumStr))
        end
    end
end

if #blockNumbers > 0 then
    local res = blockNumbers[1]
    for k = 2, #blockNumbers do
        if currentOp == '*' then
            res = res * blockNumbers[k]
        else
            res = res + blockNumbers[k]
        end
    end
    grandTotal = grandTotal + res
end

print(string.format("%.0f", grandTotal))
