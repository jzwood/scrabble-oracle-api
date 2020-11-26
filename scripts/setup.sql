DROP SCHEMA IF EXISTS oracle CASCADE;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE SCHEMA oracle;

CREATE TABLE oracle.query_rack_board (
  id SERIAL PRIMARY KEY,
  rack VARCHAR(15) NOT NULL,
  board TEXT NOT NULL CONSTRAINT board_check CHECK (char_length(board) = 225),
  uuid UUID DEFAULT uuid_generate_v4 (),
  UNIQUE (rack, board)
);

CREATE TABLE oracle.best_play (
  id SERIAL PRIMARY KEY,
  board VARCHAR(225) NOT NULL,
  word VARCHAR(15) NOT NULL,
  score SMALLINT NOT NULL,
  query_rack_board_id INTEGER,
  UNIQUE query_rack_board_id,
  CONSTRAINT fk_query_rack_board
    FOREIGN KEY(query_rack_board_id)
      REFERENCES oracle.query_rack_board(id)
);
