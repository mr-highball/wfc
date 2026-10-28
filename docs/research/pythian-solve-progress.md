# Generic solve work observation

[Pass API](../passes.md#solve-progress-and-cancellation) · [Roadmap](../../ROADMAP.md)

Big Boss, 2026-10-28. Pythian's retained generation needs a synchronous work
observer whose exception aborts a solve. Its pinned-source public reproduction
confirms that the diagnostic trace sink deliberately contains the exception and
commits the solution. Retain that contract. This branch starts at reviewed
6b419e6 and adds optional call-scoped observation to the primary graph and numeric
solver without changing search, budgets, randomness or trace identities.

This is generic solver work, with no audio vocabulary or Pythian dependency.
`wfc_solve_progress` is included in both package definitions. Native build entry
points execute the maintained regression; browser test discovery includes it.

Local evidence: checked FPC3.2.2 Win32/Win64 and actual pas2js/Node each pass44807
checks and56 original abort/retries. All six public solve families preserve exact
state/trace and random-stream replay. True three-restart and one-negotiation-
backtrack controls retain original transcript hashes36DF8F92 and6F76591D;
cancellation at the second attempt restores the initial state and exact retry.
Native executions report zero unfreed blocks. A separately compiled frozen
6b419e6 probe matches the current complete states, flags/domains, trace summaries
and subsequent random draws across six solve families and two seeds:56160 bytes,
SHA256`bbbbd8051e0f4c5ac6e7f2f1f13b7202c41bae707eb76da827417147dbe8b02e`.
This is deterministic engineering evidence, not a responsiveness or musical claim.
Published upstream validation and downstream integration remain separate gates.

Development repairs are retained in the ignored validation packet. An initial
signature edit searched a comment's word `implementation` and missed interface
defaults; the actual unit delimiter repaired it. The first prepare-interior abort
exposed a nested-procedure extraction error that omitted builder visit checks;
the model builder and helpers now have26 owned graph visit sites. The portable
fixture uses the explicit public Entry property rather than ambiguous chained
indexing. Its nonlinear negotiation control initially used a zero local
backtrack budget, which correctly rejected recovery; it now reuses the reviewed
one-cell fixture's original1/1 budgets. No solver budget or policy was retuned.
The existing immutable-provider quota regression then caught an uninitialized
work pointer during pre-transaction validation. The scope now starts explicitly
nil before any helper can run; the complete67/67 quota controls pass, including
preserved-provider negotiation, and the original failure log remains retained.

See the public API documentation for synchronous residuals and callback limits.
Observer exceptions propagate before accepted publication; callback bodies cannot
mutate/reenter the graph or consume its randomness. Rollback/destruction never
invoke the work observer. No upstream branch merge or full downstream operation
acceptance follows merely from these local results.