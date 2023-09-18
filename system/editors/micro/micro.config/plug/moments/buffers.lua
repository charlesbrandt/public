local micro = import("micro")
local config = import("micro/config")
local buffer = import("micro/buffer")
local fmt = import("fmt")

function init()
        config.MakeCommand("enumbuffers", enumBuffers, config.NoComplete)
end

local log = nil

function enumBuffers(bp)
        -- open log buff
        if bp.Buf.Type ~= buffer.BTLog then
                bp:OpenLogBuf()
                log = buffer.LogBuf()
        end

        local tabs = micro.Tabs()

        for i = 1,#tabs.List do
                tab = tabs.List[i]
                fmt.Fprintf(log, "tab %.0f. current: %v\n", i, tab:CurPane().Buf.AbsPath)
                for j = 1,#tab.Panes do
                        fmt.Fprintf(log, "    pane %.0f. path: %s\n", j, tab.Panes[j].Buf.AbsPath)
                end
        end
end
