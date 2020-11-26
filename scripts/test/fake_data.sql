-- FAKE DATA
INSERT INTO oracle.query_rack_board (rack, board)
  VALUES ('ASDFEUI', 'QWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPA');

INSERT INTO oracle.best_play (board, word, score, query_rack_board_id)
  VALUES ('JUIHDUFHYK', 'KITTY', 34, 1);


-- TEST QUERIES
SELECT id, rack, uuid FROM oracle.query_rack_board;
SELECT id, word, score, query_rack_board_id FROM oracle.best_play;


SELECT word, score FROM oracle.best_play AS bp
  INNER JOIN oracle.query_rack_board AS qrb ON qrb.id = bp.query_rack_board_id
  WHERE qrb.uuid = '8dcc55c4-c5e2-4e59-a3d8-8f7564349d26'
  LIMIT 1;

SELECT word, score FROM oracle.best_play AS bp
  INNER JOIN oracle.query_rack_board AS qrb ON qrb.id = bp.query_rack_board_id
  WHERE qrb.uuid = (
    SELECT uuid FROM oracle.query_rack_board LIMIT 1
  )
  LIMIT 1;
