
function add(arg1, arg2)
    return erlang.atom("ok"), arg1 + arg2
end

function sin(arg)
    return erlang.atom("ok"), math.sin(arg)
end
