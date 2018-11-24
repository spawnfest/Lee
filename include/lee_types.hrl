-ifndef(LEE_TYPES_HRL).
-define(LEE_TYPES_HRL, true).

-import({lee_types, [ union/2
                    , boolean/0
                    , integer/0
                    , non_neg_integer/0
                    , string/0
                    , float/0
                    , tuple/0
                    , term/0
                    , list/1
                    , non_empty_list/1
                    , map/2
                    , exact_map/1
                    ]}).

%-compile({parse_transform, lee_transform}).

-endif.
