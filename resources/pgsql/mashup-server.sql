CREATE TABLE IF NOT EXISTS char_info (
    charname VARCHAR(64) NOT NULL PRIMARY KEY,
    hidden BOOLEAN NOT NULL DEFAULT FALSE,
    tabled BOOLEAN NOT NULL DEFAULT FALSE,
    display_uni VARCHAR(8),
    display_ids VARCHAR(8),
    display_pua VARCHAR(8),
    exact_cns VARCHAR(8),
    exact_manualuni VARCHAR(8),
    comments TEXT NOT NULL DEFAULT '',
    checked BOOLEAN NOT NULL DEFAULT FALSE,
    timestamp TIMESTAMP with time zone DEFAULT CURRENT_TIMESTAMP
);
