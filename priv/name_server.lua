-- Call function which name is req[1] (first element in incoming tuple)
-- With tbl as the first argument, and other function arguments later.
function call(tbl, req)
    return _G[req[1]](tbl, unpack(req, 2))
end

function init()
    return erlang.atom("ok"), {} -- return empty state
end

function handle_call(req, from, tbl)
    return erlang.atom("reply"), call(tbl, req)
end

function add_name(tbl, name, address)
    tbl[name] = address
    return erlang.atom("ok"), tbl
end

function get_addr(tbl, name)
    return tbl[name] or erlang.atom("error"), tbl
end
