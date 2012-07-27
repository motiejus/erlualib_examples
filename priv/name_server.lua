-- Call function which name is req[1] (first element in incoming tuple)
-- With tbl as the first argument, and other function arguments later.
function call(tbl, req)
    return _G[req[1]](tbl, unpack(req, 2))
end

function init()
    -- This empty table will be our state
    return erlang.atom("ok"), {}
end

-- Forwards the call to function which is specified in req[1]. Returns
-- {reply, Return, NewTable}. Return and NewTable are values from the
-- callback.
function handle_call(req, from, tbl)
    return erlang.atom("reply"), call(tbl, req)
end

-- Adds name to State. Returns {ok, Table}.
function add_name(tbl, name, address)
    tbl[name] = address
    return erlang.atom("ok"), tbl
end

-- Gets name table and current name. Returns:
-- { 'error' | Value, Table}
-- Note: table is unmodified in this call.
function get_addr(tbl, name)
    return tbl[name] or erlang.atom("error"), tbl
end
