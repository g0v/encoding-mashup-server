CREATE TABLE IF NOT EXISTS char_info (
    serial INTEGER PRIMARY KEY,
    charname TEXT NOT NULL,
    hidden INTEGER NOT NULL DEFAULT 0,
    tabled INTEGER NOT NULL DEFAULT 0,
    display_uni TEXT NOT NULL DEFAULT '',
    display_ids TEXT NOT NULL DEFAULT '',
    display_pua TEXT NOT NULL DEFAULT '',
    exact_cns TEXT,
    exact_manualuni TEXT,
    comments TEXT NOT NULL DEFAULT '',
    checked INTEGER NOT NULL DEFAULT 0,
    timestammp DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX charname_unique ON char_info (charname ASC);
