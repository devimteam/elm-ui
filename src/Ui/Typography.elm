module Ui.Typography exposing (headline, pad12, pad24, pad4)

import Ui.Options as Options exposing (Property, styled, cs, css)


headline : Property c m
headline =
    cs "header"


pad4 : Property c m
pad4 =
    css "padding-bottom" "4px"


pad12 : Property c m
pad12 =
    css "padding-bottom" "12px"


pad24 : Property c m
pad24 =
    css "padding-bottom" "24px"


pad26 : Property c m
pad26 =
    css "padding-bottom" "26px"
