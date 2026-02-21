# Revision history for pgmq-core

## 0.1.0.0 -- 2026-02-21

* Initial release
* Core types: `Message`, `MessageBody`, `MessageHeaders`, `MessageId`, `Queue`, `QueueName`, `PgmqError`
* Queue name validation following pgmq-rs conventions
* Template Haskell `Lift` instance for `QueueName`
