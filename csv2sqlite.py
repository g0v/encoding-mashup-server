#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sqlite3
import logging
import csv
from sys import argv

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger('csv2sqlite')

DDL_COMMANDS = [
  """
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
  );""",
  'CREATE UNIQUE INDEX charname_unique ON char_info (charname ASC);'
]

def usage():
  print 'Usage: ./csv2sqlite [input.csv] [output.sqlite]'

def read_csv(csv_filename):
  logger.info('Reading CSV data from: %s', csv_filename)
  with open(csv_filename) as f:
    reader = csv.reader(f)
    rows = [row for row in reader]

  rows = rows[3:] # Skip the first 3 rows

  rows = [[unicode(cell, 'utf-8') for cell in row] for row in rows]

  return rows

def write_sqlite(sqlite_filename, rows):
  data = []
  for row in rows:
    datum = {
      u'charname': (u'moe:revised/%s' % row[0]),
      u'hidden': 0,
      u'tabled': 1 if (row[2]==u'是') else 0,
      u'display_uni': row[4],
      u'display_ids': row[5],
      u'exact_cns': row[11],
      u'exact_manualuni': row[13],
      u'comments': row[18],
      u'checked': 0,
    }
    data.append(datum)

  keys = data[0].keys()

  tuples = []
  for datum in data:
    t = tuple([datum[key] for key in keys])
    tuples.append(t)

  logger.info('Writing SQLITE data to: %s', sqlite_filename)
  connection = sqlite3.connect(sqlite_filename)
  cursor = connection.cursor()

  logger.info('Drop and recreate char_info table...')
  cursor.execute('DROP TABLE IF EXISTS char_info')
  for sql in DDL_COMMANDS:
    cursor.execute(sql)

  logger.info('Inserting %d rows into char_info table', len(tuples))
  sql = 'INSERT INTO char_info (%s) VALUES (%s)' % (','.join(keys), ','.join(['?']*len(keys)))
  logger.debug('SQL template: %s', sql)
  cursor.executemany(sql, tuples)
  connection.commit()

def main():
  if len(argv)<3:
    usage()
    exit(1)
  csv_filename, sqlite_filename = argv[1:3]

  rows = read_csv(csv_filename)
  write_sqlite(sqlite_filename, rows)

if __name__ == '__main__':
  main()
