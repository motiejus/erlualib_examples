
function init()
    return erlang.atom("ok"), {} -- return empty state
end

function handle_call(req, from, tbl)
    -- Call function which name is req[1] (first element in incoming tuple)
    -- With tbl as the first argument, and other function arguments later.
    ret, tbl = _G[req[1]](tbl, unpack(req, 2))
    return erlang.atom("reply"), ret, tbl
end

function add_name(tbl, name, address)
    tbl[name] = address
    return erlang.atom("ok"), tbl
end

function get_addr(tbl, name)
    io.stderr:write(name)
    return tbl[name] or erlang.atom("error"), tbl
end
