CREATE TYPE STATUS AS ENUM ('exist', 'deleted');

CREATE TABLE IF NOT EXISTS char_info (
    charname VARCHAR(64) NOT NULL PRIMARY KEY,
    hidden BOOLEAN NOT NULL DEFAULT FALSE,
    tabled BOOLEAN NOT NULL DEFAULT FALSE,
    display_uni VARCHAR(8),
    display_ids VARCHAR(8),
    display_pua VARCHAR(8),
    exact_cns VARCHAR(8),
    exact_forceduni VARCHAR(8),
    comment TEXT NOT NULL DEFAULT '',
    status STATUS NOT NULL DEFAULT 'exist',
    etag VARCHAR(64) NOT NULL UNIQUE
);
