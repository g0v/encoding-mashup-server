CREATE TABLE IF NOT EXISTS char_info (
    -- serial INTEGER PRIMARY KEY,
    charname TEXT NOT NULL PRIMARY KEY,
    hidden INTEGER NOT NULL DEFAULT 0,
    tabled INTEGER NOT NULL DEFAULT 0,
    display_uni TEXT,
    display_ids TEXT,
    display_pua TEXT,
    exact_cns TEXT,
    exact_manualuni TEXT,
    comments TEXT NOT NULL DEFAULT '',
    checked INTEGER NOT NULL DEFAULT 0,
    timestamp INTEGER NOT NULL DEFAULT 0
);

-- CREATE UNIQUE INDEX charname_unique ON char_info (charname ASC);
