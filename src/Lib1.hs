module Lib1
    ( completions
    ) where


-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [   
    -- Actions
    "take_order",
    "prepare_order",
    "serve_order",
    "handle_payment",
    -- Seating
    "Bar",
    "Outdoor",
    "DiningTable",
    -- Payment Method
    "Cash",
    "Card",
    "MobilePayment"
    ]