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

function startselection(bp)
	-- buffer.Log("Starting selection\n")
    local buf = bp.Buf
	-- buffer.Log("buf assigned\n")
    local cursor = buf:GetActiveCursor()
	-- buffer.Log("cursor assigned\n")
    local pos = cursor:GetVisualX()
    buffer.Log("Position " .. pos .. "\n")
    -- getting an error here:
    cursor:SetSelectionStart(pos)
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

function complete(bp)
	prefixEnd = bp.Cursor.Loc
	prefixEnd:Move(2)
	bp.Buf:Remove(-bp.Cursor.Loc, prefixEnd)
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
