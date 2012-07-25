-define(PRIV_DIR(AppName), (
        case code:priv_dir(AppName) of
            {error, bad_name} ->
                filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
            Dir -> Dir
        end
    )).
