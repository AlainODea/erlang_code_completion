-module(tm_menu).
-export([selection/1]).
-include_lib("xmerl/include/xmerl.hrl").

selection([]) -> "";
selection(MenuItems) ->
    %% TODO: need better way to get path for tm_dialog
    PlistXml = os:cmd(io_lib:format("/Applications/TextMate.app/Contents/PlugIns/Dialog.tmplugin/Contents/Resources/tm_dialog -u -p \"~s\"", [ascii_plist(MenuItems)])),
    {PlistDoc, _Rest} = xmerl_scan:string(PlistXml),
    selected( xmerl_xpath:string("/plist/dict/dict[preceding-sibling::key[1]='selectedMenuItem']/string[preceding-sibling::key[1]='title']/text()", PlistDoc)).

selected([#xmlText{value=Selected}]) -> Selected;
selected([]) ->"".

ascii_plist(MenuItems) ->
    lists:reverse([");}"|
        lists:foldl(
            fun(Item, Completions) ->
                [io_lib:format("{title = ~s;},", [Item])|Completions]
            end,
            ["{menuItems = ("],
            MenuItems
        )
    ]).
