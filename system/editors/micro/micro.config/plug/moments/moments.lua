local config = import("micro/config")
local shell = import("micro/shell")
local buffer = import("micro/buffer")
local fmt = import("fmt")

-- https://github.com/zyedidia/micro/blob/master/runtime/help/tutorial.md
-- https://pkg.go.dev/github.com/zyedidia/micro/v2@v2.0.11/internal/buffer#Loc

-- function init()
    -- -- true means overwrite any existing binding to Ctrl-r
    -- -- this will modify the bindings.json file
    -- config.TryBindKey("Ctrl-r", "lua:initlua.gorun", true)
-- end

function init()
    config.MakeCommand("now", now, config.NoComplete)
    config.MakeCommand("nowtop", nowtop, config.NoComplete)
    config.MakeCommand("today", todaytop, config.NoComplete)
    config.MakeCommand("complete", markcomplete, config.NoComplete)
    config.MakeCommand("com", markcomplete, config.NoComplete)
    config.MakeCommand("req", requested, config.NoComplete)
    config.MakeCommand("also", also, config.NoComplete)
end

function also(bp)
	local cursor = bp.Buf:GetActiveCursor()
	bp.Buf:Insert(buffer.Loc(0, cursor.Y), 'also [' .. os.date('%Y.%m.%d %H:%M:%S') .. '] \n')
end

function requested(bp)
	local cursor = bp.Buf:GetActiveCursor()
	local line = bp.Buf:Line(cursor.Y)
	if line:sub(1, 2) == '# ' then
		local timestamp = line:sub(3, 21)
		local tags = line:sub(22)
		local newLine = 'requested [' .. timestamp .. ']' .. tags	
	end
end

function markcomplete(bp)
	local cursor = bp.Buf:GetActiveCursor()
	local line = bp.Buf:Line(cursor.Y)
	if line:sub(1, 2) == '# ' then
		local timestamp = line:sub(3, 21)
		local tags = line:sub(22)
		local newLine = 'requested [' .. timestamp .. ']'
		bp.Buf:Replace(buffer.Loc(0, cursor.Y), buffer.Loc(#line, cursor.Y), newLine)
		
		-- Insert a new line above the current one
		bp.Buf:Insert(buffer.Loc(0, cursor.Y), '# ' .. os.date('%Y.%m.%d %H:%M:%S complete') .. tags .. '\n')
	end
end

function now(bp)
    -- local buf = bp.Buf
    -- local cursor = buf:GetActiveCursor()
    -- -- TODO: assign location to be current cursor postion
	-- local location = buffer.Loc(0, 0)
	-- buf:Insert(location, )
	local stamp = os.date("# %Y.%m.%d %H:%M:%S ")
    -- buffer.Log("Stamp " .. stamp .. "\n")
	-- fmt.Printf(stamp)
	-- https://github.com/zyedidia/micro/issues/2183
	bp.Buf:Insert(-bp.Cursor.Loc, stamp)
end

function nowtop(bp)
    local buf = bp.Buf
    local cursor = buf:GetActiveCursor()
    -- this only goes to the start of the line
	-- cursor:StartOfText()
	local location = buffer.Loc(0, 0)
	-- location:ParseCursorLocation("0:0")
	-- local beginning =
	buf:Insert(location, os.date("# %Y.%m.%d %H:%M:%S \n\n"))
	cursor:GotoLoc(location)
	cursor:End()
end

function todaytop(bp)
    local buf = bp.Buf
    local cursor = buf:GetActiveCursor()
    -- this only goes to the start of the line
	-- cursor:StartOfText()
	local location = buffer.Loc(0, 0)
	-- location:ParseCursorLocation("0:0")
	-- local beginning =
	buf:Insert(location, os.date("# %Y.%m.%d \n\n"))
	cursor:GotoLoc(location)
	cursor:End()
end
