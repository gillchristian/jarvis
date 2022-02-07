{-# LANGUAGE QuasiQuotes #-}

module Jarvis.Query where

import qualified Database.SQLite.Simple as Sql
import Text.RawString.QQ

createChannelActiveNotes :: Sql.Query
createChannelActiveNotes =
  [r|
    CREATE TABLE IF NOT EXISTS channel_active_notes (
      channel_id TEXT PRIMARY KEY NOT NULL,
      note_name TEXT NOT NUll
    );
  |]

createMessageQueueTable :: Sql.Query
createMessageQueueTable =
  [r|
    CREATE TABLE IF NOT EXISTS message_queue (
      status TEXT NOT NULL,
      message_id TEXT PRIMARY KEY NOT NULL,
      channel_id TEXT NOT NULL,
      note_name TEXT
    );
  |]

insertMsg :: Sql.Query
insertMsg =
  [r|
    INSERT INTO message_queue
    (status, message_id, channel_id, note_name)
    VALUES (?,?,?,?);
  |]

allPendingMsgs :: Sql.Query
allPendingMsgs =
  [r|
    SELECT status, message_id, channel_id, note_name
    FROM message_queue
    WHERE status = "pending";
  |]

insertChannelNote :: Sql.Query
insertChannelNote =
  [r|
    INSERT INTO channel_active_notes
    (channel_id, note_name)
    VALUES (?,?);
  |]

updateNoteForChannel :: Sql.Query
updateNoteForChannel =
  [r|
    UPDATE channel_active_notes
    SET note_name = ?
    WHERE channel_id = ?;
  |]

updateNoteForMsg :: Sql.Query
updateNoteForMsg =
  [r|
    UPDATE message_queue
    SET note_name = ?
    WHERE channel_id = ? AND note_name IS NULL;
  |]

selectChannelNote :: Sql.Query
selectChannelNote =
  [r|
    SELECT channel_id, note_name
    FROM channel_active_notes
    WHERE channel_id = ?;
  |]

allChannelNotes :: Sql.Query
allChannelNotes =
  [r|
    SELECT channel_id, note_name
    FROM channel_active_notes;
  |]

delMsg :: Sql.Query
delMsg =
  [r|
    DELETE FROM message_queue
    WHERE message_id = ?;
  |]

channelStats :: Sql.Query
channelStats =
  [r|
    SELECT
      SUM(status = 'pending') AS pending,
      SUM(note_name IS NULL) AS noteless_msgs
    FROM message_queue
    WHERE channel_id = ?;
  |]

delNoteOfChannel :: Sql.Query
delNoteOfChannel =
  [r|
    DELETE FROM channel_active_notes
    WHERE channel_id = ?;
  |]
