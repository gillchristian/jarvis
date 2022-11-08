{-# LANGUAGE QuasiQuotes #-}

module Jarvis.Query where

import qualified Database.SQLite.Simple as Sql
import Text.RawString.QQ

createChannelActiveNoteTable :: Sql.Query
createChannelActiveNoteTable =
  [r|
    CREATE TABLE IF NOT EXISTS channel_active_notes (
      channel_id TEXT PRIMARY KEY NOT NULL,
      channel_name TEXT NOT NULL,
      note_name TEXT NOT NUll
    );
  |]

createChannelActiveVaultTable :: Sql.Query
createChannelActiveVaultTable =
  [r|
    CREATE TABLE IF NOT EXISTS channel_active_vault (
      channel_id TEXT PRIMARY KEY NOT NULL,
      channel_name TEXT PRIMARY KEY NOT NULL,
      vault_name TEXT NOT NUll
    );
  |]

createMsgQueueTable :: Sql.Query
createMsgQueueTable =
  [r|
    CREATE TABLE IF NOT EXISTS message_queue (
      message_id TEXT PRIMARY KEY NOT NULL,
      content TEXT NOT NULL,
      note_name TEXT NOT NULL,
      vault_name TEXT,
      mode TEXT NOT NULL
    );
  |]

insertMsg :: Sql.Query
insertMsg =
  [r|
    INSERT INTO message_queue
    (message_id, content, note_name, vault_name, mode)
    VALUES (?,?,?,?,?);
  |]

getMsg :: Sql.Query
getMsg =
  [r|
    SELECT message_id, content, note_name, vault_name, mode
    FROM message_queue
    WHERE message_id = ?;
  |]

insertChannelNote :: Sql.Query
insertChannelNote =
  [r|
    INSERT INTO channel_active_notes
    (channel_id, channel_name, note_name)
    VALUES (?,?,?);
  |]

insertChannelVault :: Sql.Query
insertChannelVault =
  [r|
    INSERT INTO channel_active_vault
    (channel_id, channel_name, vault_name)
    VALUES (?,?,?);
  |]

updateNoteForChannel :: Sql.Query
updateNoteForChannel =
  [r|
    UPDATE channel_active_notes
    SET note_name = ?
    WHERE channel_id = ?;
  |]

updateVaultForChannel :: Sql.Query
updateVaultForChannel =
  [r|
    UPDATE channel_active_vault
    SET vault_name = ?
    WHERE channel_id = ?;
  |]

selectChannelNote :: Sql.Query
selectChannelNote =
  [r|
    SELECT channel_id, channel_name, note_name
    FROM channel_active_notes
    WHERE channel_id = ?;
  |]

selectChannelVault :: Sql.Query
selectChannelVault =
  [r|
    SELECT channel_id, channel_name, vault_name
    FROM channel_active_vault
    WHERE channel_id = ?;
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
    SELECT vault_name, note_name, COUNT(*)
    FROM message_queue
    GROUP BY vault_name, note_name;
  |]

delNoteOfChannel :: Sql.Query
delNoteOfChannel =
  [r|
    DELETE FROM channel_active_notes
    WHERE channel_id = ?;
  |]

delVaultOfChannel :: Sql.Query
delVaultOfChannel =
  [r|
    DELETE FROM channel_active_vault
    WHERE channel_id = ?;
  |]
