-- | Topic management statements (pgmq 1.11.0+)
-- Provides AMQP-like topic-based routing: bind/unbind patterns to queues,
-- validate routing keys and patterns, test routing, and list bindings.
module Pgmq.Hasql.Statements.TopicManagement
  ( bindTopic,
    unbindTopic,
    validateRoutingKey,
    validateTopicPattern,
    testRouting,
    listTopicBindings,
    listTopicBindingsForQueue,
  )
where

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement, preparable)
import Pgmq.Hasql.Decoders (routingMatchDecoder, topicBindingDecoder)
import Pgmq.Hasql.Encoders
  ( bindTopicEncoder,
    queueNameEncoder,
    routingKeyValue,
    topicPatternValue,
    unbindTopicEncoder,
  )
import Pgmq.Hasql.Statements.Types (BindTopic, UnbindTopic)
import Pgmq.Types (QueueName, RoutingKey, RoutingMatch, TopicBinding, TopicPattern)

-- | Bind a topic pattern to a queue (pgmq 1.11.0+)
bindTopic :: Statement BindTopic ()
bindTopic = preparable sql bindTopicEncoder D.noResult
  where
    sql = "select from pgmq.bind_topic($1, $2)"

-- | Unbind a topic pattern from a queue (pgmq 1.11.0+)
unbindTopic :: Statement UnbindTopic Bool
unbindTopic = preparable sql unbindTopicEncoder decoder
  where
    sql = "select pgmq.unbind_topic($1, $2)"
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

-- | Validate a routing key (pgmq 1.11.0+)
validateRoutingKey :: Statement RoutingKey Bool
validateRoutingKey = preparable sql encoder decoder
  where
    sql = "select pgmq.validate_routing_key($1)"
    encoder = E.param (E.nonNullable routingKeyValue)
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

-- | Validate a topic pattern (pgmq 1.11.0+)
validateTopicPattern :: Statement TopicPattern Bool
validateTopicPattern = preparable sql encoder decoder
  where
    sql = "select pgmq.validate_topic_pattern($1)"
    encoder = E.param (E.nonNullable topicPatternValue)
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

-- | Test which queues a routing key would match (pgmq 1.11.0+)
testRouting :: Statement RoutingKey [RoutingMatch]
testRouting = preparable sql encoder decoder
  where
    sql = "select * from pgmq.test_routing($1)"
    encoder = E.param (E.nonNullable routingKeyValue)
    decoder = D.rowList routingMatchDecoder

-- | List all topic bindings (pgmq 1.11.0+)
listTopicBindings :: Statement () [TopicBinding]
listTopicBindings = preparable sql E.noParams decoder
  where
    sql = "select * from pgmq.list_topic_bindings()"
    decoder = D.rowList topicBindingDecoder

-- | List topic bindings for a specific queue (pgmq 1.11.0+)
listTopicBindingsForQueue :: Statement QueueName [TopicBinding]
listTopicBindingsForQueue = preparable sql queueNameEncoder decoder
  where
    sql = "select * from pgmq.list_topic_bindings($1)"
    decoder = D.rowList topicBindingDecoder
