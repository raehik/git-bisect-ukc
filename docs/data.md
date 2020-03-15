# Data flow and state
## App
Returns `ExceptT App.Error IO [App.RepoSolution]`:

  * `App.Error` indicates some irrecoverable app error (e.g. file couldn't be
    read)
  * `App.RepoSolution` is labelled list of instance solutions.

The whole app 
