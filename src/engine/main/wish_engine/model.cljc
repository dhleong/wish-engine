(ns wish-engine.model)

(defprotocol WishEngine
  (create-state
    [this]
    "Create an empty, initial state for use with this engine")

  (eval-source-form
    [this state form]
    "Evaluate the given `form` in the context of `state`")

  (parse-string
    [this s]
    "Parse `s` into a form that can be eval'd"))

