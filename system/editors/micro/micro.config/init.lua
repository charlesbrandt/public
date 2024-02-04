-- function init()
--     -- true means overwrite any existing binding to Ctrl-space
--     -- this will modify the bindings.json file
--     config.TryBindKey("CtrlSpace", "lua:initlua.startselection", true)
-- end

function startselection(bp)
    local buf = bp.Buf
	-- buf.Log("buf assigned\n")
	-- buf.Log("Starting selection\n")
    local cursor = buf:GetActiveCursor()
	-- buf.Log("cursor assigned\n")
    local pos = cursor:GetVisualX()
    buf.Log("Position " .. pos .. "\n")
    -- getting an error here:
    cursor:SetSelectionStart(pos)
end

