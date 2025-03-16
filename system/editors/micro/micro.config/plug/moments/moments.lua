VERSION = "1.0.0"

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
    -- commands for opening files from a list
    config.MakeCommand("openfiles", openFiles, config.NoComplete)
    config.MakeCommand("opentabs", openFiles, config.NoComplete)
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
        -- Replace the current line with the new line
        bp.Buf:Replace(buffer.Loc(0, cursor.Y), buffer.Loc(#line, cursor.Y), newLine)
    else
        micro.InfoBar():Message("Current line is not a timestamp line (should start with '# ')")
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

function openFiles(bp)
    local buf = bp.Buf
    local cursor = bp.Buf:GetActiveCursor()
    local files = {}
    
    -- Get the selection directly using GetSelection
    local selection = bp.Buf:GetSelection()
    
    -- Debug info
    micro.InfoBar():Message("Selection length: " .. #selection)
    
    if selection ~= "" then
        -- Split the selection into lines
        for path in selection:gmatch("[^\r\n]+") do
            path = path:match("^%s*(.-)%s*$") -- Trim whitespace
            if path ~= "" then
                table.insert(files, path)
                micro.InfoBar():Message("Added path: " .. path)
            end
        end
    else
        -- Fallback to checking cursor selection
        if cursor:HasSelection() then
            -- Get the selected text
            local startLoc = cursor.CurSelection[1]
            local endLoc = cursor.CurSelection[2]
            
            micro.InfoBar():Message("Using cursor selection from line " .. startLoc.Y .. " to " .. endLoc.Y)
            
            -- Process each line in the selection
            for i = startLoc.Y, endLoc.Y do
                local line = buf:Line(i)
                if line:match("%S") then  -- Skip empty lines
                    local path = line:match("^%s*(.-)%s*$")  -- Trim whitespace
                    table.insert(files, path)
                    micro.InfoBar():Message("Added path from line " .. i .. ": " .. path)
                end
            end
        else
            -- If no selection, get all lines in the buffer
            micro.InfoBar():Message("No selection found, using all lines")
            for i = 0, buf:LinesNum() - 1 do
                local line = buf:Line(i)
                if line:match("%S") then  -- Skip empty lines
                    table.insert(files, line:match("^%s*(.-)%s*$"))  -- Trim whitespace
                end
            end
        end
    end
    
    micro.InfoBar():Message("Found " .. #files .. " file paths")
    
    -- Count how many valid files we found
    local count = 0
    
    -- Open each file in new tabs
    for i = 1, #files do
        local path = files[i]
        if path ~= "" then
            -- Try to open the file
            local success, err = pcall(function()
                -- Create a new tab
                micro.Tabs():NewTab()
                -- The new tab becomes the current tab, so we can open the file in its pane
                local err = micro.CurPane():OpenFile(path)
                if err ~= nil then
                    micro.InfoBar():Message("Error opening " .. path .. ": " .. err)
                else
                    count = count + 1
                end
            end)
            
            if not success then
                micro.InfoBar():Message("Failed to open: " .. path .. " (" .. tostring(err) .. ")")
            end
        end
    end
    
    -- Show a summary message
    if count > 0 then
        micro.InfoBar():Message("Opened " .. count .. " file(s) in new tabs")
    else
        micro.InfoBar():Message("No valid files found to open")
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
