local micro = import("micro")
local config = import("micro/config")
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
    config.MakeCommand("load", loadBuffers, config.NoComplete)
    -- looking for an intuitive command name here
    config.MakeCommand("list", enumBuffers, config.NoComplete)
    config.MakeCommand("buffs", enumBuffers, config.NoComplete)
    config.MakeCommand("files", enumBuffers, config.NoComplete)
    config.MakeCommand("tabs", enumBuffers, config.NoComplete)
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

function loadBuffers(bp)
    -- TODO: this is not working
    -- Get the selected text in the buffer
    local selection = bp.Buf:GetSelection()

    -- Split the selection into lines
    local files = {}
    for path in selection:gmatch("[^\n]+") do
        table.insert(files, path)
    end

    -- Open each file in a new tab
    for i = 1, #files do
        local path = files[i]
        if path ~= "" then
            micro.InfoBar():Message("Opening " .. path)
            micro.InfoBar():Update()
            -- create a new tab before opening the path
            micro.Tabs():NewTab()
            micro.CurPane():OpenFile(path)
        end
    end
end

function enumBuffers(bp)
    local tabs = micro.Tabs()
    -- This yields error: "Lua API error: attempt to call a non-function object"
    -- local curCursor = bp.Buf:Cursor() -- get the current cursor

    for i = 1, #tabs.List do
        tab = tabs.List[i]
        bp.Buf:Insert(-bp.Cursor.Loc, string.format("%v\n", tab:CurPane().Buf.AbsPath))
        -- this is nil
        -- bp.Buf:Insert(-bp.Cursor.Loc, string.format("%v\n", tab:CurPane().Buf.RelPath))
        -- bp.Buf:Insert(-bp.Cursor.Loc, string.format("tab %.0f. current: %v\n", i, tab:CurPane().Buf.AbsPath))
        -- tabs can have multiple panes ... include if you want those too
        -- for j = 1, #tab.Panes do
        --     bp.Buf:Insert(-bp.Cursor.Loc, string.format("    pane %.0f. path: %s\n", j, tab.Panes[j].Buf.AbsPath))
        -- end
    end
end
